(ns glfw-clj.core
  (:require
   [coffi.mem :as mem :refer [defalias]]
   [coffi.ffi :as ffi :refer [defcfn]]))

(ffi/load-system-library "glfw")

;;; Initialization and Error Handling

(defcfn init
  "Initializes GLFW.

  Returns true if it initializes successfully. Automatically terminates GLFW if
  it fails. If it succeeds, then you should call [[terminate]] before
  application exit."
  "glfwInit" [] ::mem/int
  glfw-init
  []
  (not (zero? (glfw-init))))

(defcfn terminate
  "Shuts down GLFW.

  The contexts of any windows must not be current on any other thread when this
  is called."
  "glfwTerminate" [] ::mem/void)

(def ^:private init-hint->enum
  "Map from hint keywords to the enum values they represent."
  {:joystick-hat-buttons (int 0x00050001)
   :cocoa-chdir-resources (int 0x00051001)
   :cocoa-menubar (int 0x00051002)})
(def init-hints (set (keys init-hint->enum)))

(defmethod mem/primitive-type ::init-hint
  [_type]
  ::mem/int)

(defmethod mem/serialize* ::init-hint
  [obj _type _scope]
  (init-hint->enum obj))

(defcfn init-hint
  "Sets the given `hint` with the boolean `value`.

  The `hint` must be one of the following:
  - `:joystick-hat-buttons`
  - `:cocoa-chdir-resources`
  - `:cocoa-menubar`

  Hints starting with `:cocoa` are MacOS-specific.

  This must be called before [[init]]."
  "glfwInitHint" [::init-hint ::mem/int] ::mem/void
  glfw-init-hint
  [hint value]
  (assert (init-hints hint) (str "`hint` is one of " (pr-str init-hints)))
  (glfw-init-hint hint (if value 1 0)))

(defcfn get-version
  "Gets a vector of the major, minor, and revision version of GLFW.

  This can be called on any thread, and before [[init]]."
  "glfwGetVersion" [::mem/pointer ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-version
  []
  (with-open [scope (mem/stack-scope)]
    (let [major (mem/alloc-instance ::mem/int scope)
          minor (mem/alloc-instance ::mem/int scope)
          rev (mem/alloc-instance ::mem/int scope)]
      (glfw-get-version (mem/address-of major)
                        (mem/address-of minor)
                        (mem/address-of rev))
      (mapv #(mem/deserialize-from % ::mem/int) [major minor rev]))))

(defcfn get-version-string
  "Gets a string of the version of GLFW.

  Don't parse the string to get the version. Instead use [[get-version]].

  This can be called on any thread, and before [[init]]."
  "glfwGetVersionString" [] ::mem/c-string)

(def ^:private error-code->keyword
  "Map from error code enums to keywords naming the errors."
  {0x00000000 :no-error
   0x00010001 :not-initialized
   0x00010002 :no-current-context
   0x00010003 :invalid-enum
   0x00010004 :invalid-value
   0x00010005 :out-of-memory
   0x00010006 :api-unavailable
   0x00010007 :version-unavailable
   0x00010008 :platform-error
   0x00010009 :format-unavailable
   0x0001000A :no-window-context})

(defmethod mem/primitive-type ::error-code
  [_type]
  ::mem/int)

(defmethod mem/deserialize* ::error-code
  [obj _type]
  (error-code->keyword obj))

(defcfn get-error
  "Gets the most recent error which has occurred on this thread.

  If there is an error to fetch, it is returned as an [[ex-info]] with the
  message as the error description and the key `:type` in the [[ex-data]] with
  the error code's human-readable name.

  This function may be called before [[init]]."
  "glfwGetError" [::mem/pointer] ::error-code
  glfw-get-error
  []
  (with-open [scope (mem/stack-scope)]
    (let [description (mem/alloc-instance ::mem/pointer scope)
          error-code (glfw-get-error (mem/address-of description))
          description-str (mem/deserialize-from description ::mem/c-string)]
      (when-not (= :no-error error-code)
        (ex-info description-str {:type error-code})))))

(defalias ::error-fn [::ffi/fn [::error-code ::mem/c-string] ::mem/void])

(defcfn set-error-callback
  "Sets the global error callback for GLFW.

  The callback is a function of an integer error code and a string description.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used. If the callback is called
  by GLFW after the scope has been released, it will cause a JVM crash.

  This function may be called before [[init]]."
  "glfwSetErrorCallback" [::mem/pointer] ::mem/pointer
  glfw-set-error-callback
  ([callback]
   (set-error-callback callback (mem/global-scope)))
  ([callback scope]
   (mem/deserialize*
    (glfw-set-error-callback
     (mem/serialize* callback ::error-fn scope))
    ::error-fn)))

;;; Window Management

(def ^:private window-hint->enum
  "Map from window creation hints and attributes to their enum values."
  {:focused (int 0x00020001)
   :iconified (int 0x00020002)
   :resizable (int 0x00020003)
   :visible (int 0x00020004)
   :decorated (int 0x00020005)
   :auto-iconify (int 0x00020006)
   :floating (int 0x00020007)
   :maximized (int 0x00020008)
   :center-cursor (int 0x00020009)
   :transparent-framebuffer (int 0x0002000A)
   :hovered (int 0x0002000B)
   :focus-on-show (int 0x0002000C)
   :red-bits (int 0x00021001)
   :green-bits (int 0x00021002)
   :blue-bits (int 0x00021003)
   :alpha-bits (int 0x00021004)
   :depth-bits (int 0x00021005)
   :stencil-bits (int 0x00021006)
   :accum-red-bits (int 0x00021007)
   :accum-green-bits (int 0x00021008)
   :accum-blue-bits (int 0x00021009)
   :accum-alpha-bits (int 0x0002100A)
   :aux-buffers (int 0x0002100B)
   :stereo (int 0x0002100C)
   :samples (int 0x0002100D)
   :srgb-capable (int 0x0002100E)
   :refresh-rate (int 0x0002100F)
   :doublebuffer (int 0x00021010)
   :client-api (int 0x00022001)
   :context-version-major (int 0x00022002)
   :context-version-minor (int 0x00022003)
   :context-revision (int 0x00022004)
   :context-robustness (int 0x00022005)
   :opengl-forward-compat (int 0x00022006)
   :opengl-debug-context (int 0x00022007)
   :opengl-profile (int 0x00022008)
   :context-release-behavior (int 0x00022009)
   :context-no-error (int 0x0002200A)
   :context-creation-api (int 0x0002200B)
   :scale-to-monitor (int 0x0002200C)
   :cocoa-retina-framebuffer (int 0x00023001)
   :cocoa-frame-name (int 0x00023002)
   :cocoa-graphics-switching (int 0x00023003)
   :x11-class-name (int 0x00024001)
   :x11-instance-name (int 0x00024002)})
(def window-hints (set (keys window-hint->enum)))

(def ^:private boolean-window-hints
  #{:resizable :visible :decorated :focused
    :auto-iconify :floating :maximized :center-cursor
    :transparent-framebuffer :focus-on-show :scale-to-monitor
    :stereo :srgb-capable :doublebuffer
    :opengl-forward-compat :opengl-debug-context
    :cocoa-retina-framebuffer :cocoa-graphics-switching})

(defmethod mem/primitive-type ::window-hint
  [_type]
  ::mem/int)

(defmethod mem/serialize* ::window-hint
  [obj _type _scope]
  (window-hint->enum obj))

(defcfn default-window-hints
  "Resets all the window creation init-hints to their default values."
  "glfwDefaultWindowHints" [] ::mem/void)

(defn- reverse-map
  "Takes a 1:1 map and returns a map from the values to the keys."
  [m]
  (into {} (map (comp vec reverse)) m))

(def ^:private keyword->client-api
  {:opengl 0x00030001
   :opengl-es 0x00030002
   :none 0})
(def ^:private client-api->keyword (reverse-map keyword->client-api))
(def client-api-opts (set (keys keyword->client-api)))

(def ^:private keyword->context-api
  {:native 0x00036001
   :egl 0x00036002
   :osmesa 0x00036003})
(def ^:private context-api->keyword (reverse-map keyword->context-api))
(def context-api-opts (set (keys keyword->context-api)))

(def ^:private keyword->context-robustness
  {:no-robustness 0
   :no-reset-notification 0x00031001
   :lose-context-on-reset 0x00031002})
(def ^:private context-robustness->keyword (reverse-map keyword->context-robustness))
(def context-robustness-opts (set (keys keyword->context-robustness)))

(def ^:private keyword->release-behavior
  {:any 0
   :none 0x00035002
   :flush 0x00035001})
(def ^:private release-behavior->keyword (reverse-map keyword->release-behavior))
(def release-behavior-opts (set (keys keyword->release-behavior)))

(def ^:private keyword->opengl-profile
  {:any 0
   :core 0x00032001
   :compat 0x00032002})
(def ^:private opengl-profile->keyword (reverse-map keyword->opengl-profile))
(def opengl-profile-opts (set (keys keyword->opengl-profile)))

(defcfn window-hint
  "Sets a window hint for the next window to be created."
  "glfwWindowHint" [::window-hint ::mem/int] ::mem/void
  glfw-window-hint
  [hint value]
  (glfw-window-hint
   hint
   (if (boolean-window-hints hint)
     (if value 1 0)
     (case value
       :client-api (keyword->client-api (or value :opengl))
       :context-creation-api (keyword->context-api (or value :native))
       :context-robustness (keyword->context-robustness (or value :no-robustness))
       :context-release-behavior (keyword->release-behavior (or value :any))
       :opengl-profile (keyword->opengl-profile (or value :any))
       (if (= :dont-care value)
         -1
         value)))))

(defcfn window-hint-string
  "Sets a string-valued window hint for the next window to be created."
  {:arglists '([hint value])}
  "glfwWindowHintString" [::window-hint ::mem/c-string] ::mem/void)

(defalias ::window ::mem/pointer)
(defalias ::monitor ::mem/pointer)

(defcfn create-window
  "Constructs a brand new window of the given size and with the given `title`.

  If the window should be created as a fullscreen window, then pass a `monitor`
  object. The `share` parameter is another window object which the context will
  share resources with.

  Returns an opaque window object.

  See [[destroy-window]]."
  "glfwCreateWindow" [::mem/int ::mem/int ::mem/c-string ::monitor ::window] ::window
  glfw-create-window
  ([width height title]
   (create-window width height title nil nil))
  ([width height title monitor]
   (create-window width height title monitor nil))
  ([width height title monitor share]
   (glfw-create-window width height title monitor share)))

(defcfn destroy-window
  "Destroys the given `window`.

  See [[create-window]]."
  "glfwDestroyWindow" [::window] ::mem/void)

(defcfn window-should-close
  "Checks if the given `window`'s close flag is set."
  "glfwWindowShouldClose" [::window] ::mem/int
  glfw-window-should-close
  [window]
  (not (zero? (glfw-window-should-close window))))

(defcfn set-window-should-close
  "Sets the given `window`'s close flag."
  "glfwSetWindowShouldClose" [::window ::mem/int] ::mem/void
  glfw-set-window-should-close
  [window should-close?]
  (glfw-set-window-should-close window (if should-close? 1 0)))

(defcfn set-window-title
  "Sets the `title` of the given `window`."
  {:arglists '([window title])}
  "glfwSetWindowTitle" [::window ::mem/c-string] ::mem/void)

(defalias ::image
  [::mem/struct
   [[:width ::mem/int]
    [:height ::mem/int]
    [:pixels ::mem/pointer]]])

(defcfn set-window-icon
  "Sets the icon of the `window` to one of the `images`.

  The image with the closest resolution to the one desired by the OS will be
  used, rescaling if needed.

  Each image is a map with the keys `:width`, `:height`, and `:pixels`. The
  `:width` and `:height` are integers, while `:pixels` is a pointer to the
  memory which will be copied for the icon. The image data will be finished
  copying before this function returns.

  The image data is 8 bits color per channel, RGBA, little-endian, and
  non-premultiplied. Pixels are arranged in rows starting from the top left."
  "glfwSetWindowIcon" [::window ::mem/int ::mem/pointer] ::mem/void
  glfw-set-window-icon
  [window images]
  (with-open [scope (mem/stack-scope)]
    (let [image-array (mem/alloc-instance [::mem/array ::image (count images)] scope)]
      (dorun
       (map (fn [segment image]
              (mem/serialize-into image ::image segment scope))
            (mem/slice-segments image-array (mem/size-of ::image))
            images))
      (glfw-set-window-icon window (count images) (mem/address-of image-array)))))

(defcfn get-window-pos
  "Gets the current x and y position of the given `window` as a vector."
  "glfwGetWindowPos" [::window ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-window-pos
  [window]
  (with-open [scope (mem/stack-scope)]
    (let [xpos (mem/alloc-instance ::mem/int scope)
          ypos (mem/alloc-instance ::mem/int scope)]
      (glfw-get-window-pos window (mem/address-of xpos) (mem/address-of ypos))
      [(mem/deserialize-from xpos ::mem/int) (mem/deserialize-from ypos ::mem/int)])))

(defcfn set-window-pos
  "Sets the `x` and `y` positions of the given `window`."
  {:arglists '([window x y])}
  "glfwSetWindowPos" [::window ::mem/int ::mem/int] ::mem/void)

(defcfn get-window-size
  "Gets the width and height of the content area of the given `window` as a vector."
  "glfwGetWindowSize" [::window ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-window-size
  [window]
  (with-open [scope (mem/stack-scope)]
    (let [width (mem/alloc-instance ::mem/int scope)
          height (mem/alloc-instance ::mem/int scope)]
      (glfw-get-window-size window (mem/address-of width) (mem/address-of height))
      [(mem/deserialize-from width ::mem/int) (mem/deserialize-from height ::mem/int)])))

(defcfn set-window-size-limits
  "Sets the size limits of the content area of the window"
  "glfwSetWindowSizeLimits" [::window ::mem/int ::mem/int ::mem/int ::mem/int] ::mem/void
  glfw-set-window-size-limits
  [window min-width min-height max-width max-height]
  (apply glfw-set-window-size-limits
         window
         (map #(if (= :dont-care %) -1 %) [min-width min-height max-width max-height])))

(defcfn set-window-aspect-ratio
  "Sets a required aspect ratio of the content area of the `window`.

  This has no effect on fullscreen windows.

  The `ratio` must be [[rational?]], with the numerator and denominator
  extracted before being passed to the 3-arity version of this function."
  "glfwSetWindowAspectRatio" [::window ::mem/int ::mem/int] ::mem/void
  glfw-set-window-aspect-ratio
  ([window ratio]
   (assert (rational? ratio))
   (if (ratio? ratio)
     (set-window-aspect-ratio window (numerator ratio) (denominator ratio))
     (set-window-aspect-ratio window ratio 1)))
  ([window numer denom]
   (glfw-set-window-aspect-ratio window numer denom)))

(defcfn set-window-size
  "Sets the `width` and `height` of the content area of `window`."
  {:arglists '([window width height])}
  "glfwSetWindowSize" [::window ::mem/int ::mem/int] ::mem/void)

(defcfn get-framebuffer-size
  "Gets the size in pixels of the framebuffer for rendering to the `window`, as a vector."
  "glfwGetFramebufferSize" [::window ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-framebuffer-size
  [window]
  (with-open [scope (mem/stack-scope)]
    (let [width (mem/alloc-instance ::mem/int scope)
          height (mem/alloc-instance ::mem/int scope)]
      (glfw-get-framebuffer-size window (mem/address-of width) (mem/address-of height))
      [(mem/deserialize-from width ::mem/int) (mem/deserialize-from height ::mem/int)])))

(defcfn get-window-frame-size
  "Gets the size of the `window` (including decorations).

  Returns a vector of the pixel lengths of the left, top, right, and bottom
  edges of the window."
  "glfwGetWindowFrameSize" [::window ::mem/pointer ::mem/pointer ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-window-frame-size
  [window]
  (with-open [scope (mem/stack-scope)]
    (let [left (mem/alloc-instance ::mem/int scope)
          top (mem/alloc-instance ::mem/int scope)
          right (mem/alloc-instance ::mem/int scope)
          bottom (mem/alloc-instance ::mem/int scope)]
      (glfw-get-window-frame-size window
                                  (mem/address-of left)
                                  (mem/address-of top)
                                  (mem/address-of right)
                                  (mem/address-of bottom))
      [(mem/deserialize-from left ::mem/int)
       (mem/deserialize-from top ::mem/int)
       (mem/deserialize-from right ::mem/int)
       (mem/deserialize-from bottom ::mem/int)])))

(defcfn get-window-content-scale
  "Gets the current content scale for the given `window`.

  The content scale is the ratio between the current DPI and the platform default DPI."
  "glfwGetWindowContentScale" [::window ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-window-content-scale
  [window]
  (with-open [scope (mem/stack-scope)]
    (let [x-scale (mem/alloc-instance ::mem/float scope)
          y-scale (mem/alloc-instance ::mem/float scope)]
      (glfw-get-window-content-scale window (mem/address-of x-scale) (mem/address-of y-scale))
      [(mem/deserialize-from x-scale ::mem/float) (mem/deserialize-from y-scale ::mem/float)])))

(defcfn get-window-opacity
  "Gets the opacity of the `window`, including any decorations."
  "glfwGetWindowOpacity" [::window] ::mem/float)

(defcfn set-window-opacity
  "Sets the opacity of the `window`, including any decorations."
  {:arglists '([window opacity])}
  "glfwSetWindowOpacity" [::window ::mem/float] ::mem/void)

(defcfn iconify-window
  "Minimizes the `window` if it was previously restored.

  On a fullscreen `window`, returns the monitor to its original resolution until
  the `window` is restored."
  "glfwIconifyWindow" [::window] ::mem/void)

(defcfn restore-window
  "Restores the `window` to its chosen resolution if it was previously maximized or iconified.

  If the `window` was fullscreen, returns the monitor to its original
  resolution."
  "glfwRestoreWindow" [::window] ::mem/void)

(defcfn maximize-window
  "Maximizes the `window`.

  If the `window` was fullscreen, this function does nothing."
  "glfwMaximizeWindow" [::window] ::mem/void)

(defcfn show-window
  "Shows the `window` if it was previously hidden."
  "glfwShowWindow" [::window] ::mem/void)

(defcfn hide-window
  "Hides the `window` if it was previously shown."
  "glfwHideWindow" [::window] ::mem/void)

(defcfn focus-window
  "Requests focus on the `window`, bringing it to the front.

  The `window` should already be visible and not iconified.

  Do not use this function to steal focus from other applications unless you are
  absolutely sure that is what the user wants. Prefer using attention
  requests.

  See [[request-window-attention]]."
  "glfwFocusWindow" [::window] ::mem/void)

(defcfn request-window-attention
  "Requests the attention of the user on the `window`.

  Once the user has given the application attention, usually by focusing the
  window, the attention request ends."
  "glfwRequestWindowAttention" [::window] ::mem/void)

(defcfn get-window-monitor
  "Gets an opaque representation of the monitor for the `window`.

  Returns null if the `window` is not a fullscreen window."
  "glfwGetWindowMonitor" [::window] ::monitor)

(defcfn set-window-monitor
  "Sets the `window` to be fullscreen on `monitor`, or to windowed mode if nil.

  If `monitor` is nil, the `refresh-rate` is ignored. If `monitor` is non-nil,
  then `x-pos` and `y-pos` are ignored. Alternate-arity versions of the function
  are provided to match those uses."
  {:arglists '([window monitor width height refresh-rate]
               [window x-pos y-pos width height]
               [window monitor x-pos y-pos width height refresh-rate])}
  "glfwSetWindowMonitor" [::window ::monitor ::mem/int ::mem/int ::mem/int ::mem/int ::mem/int] ::mem/void
  glfw-set-window-monitor
  ([window monitor-or-x arg1 arg2 arg3]
   (if (mem/address? monitor-or-x)
     (set-window-monitor window monitor-or-x 0 0 arg1 arg2 arg3)
     (set-window-monitor window nil monitor-or-x arg1 arg2 arg3 :dont-care)))
  ([window monitor x-pos y-pos width height refresh-rate]
   (glfw-set-window-monitor window monitor x-pos y-pos width height
                            (if (= :dont-care refresh-rate) -1 refresh-rate))))

(defcfn get-window-attrib
  "Gets the current value of `attrib` from the `window`.

  Framebuffer related hints are not attributes."
  "glfwGetWindowAttrib" [::window ::window-hint] ::mem/int
  glfw-get-window-attrib
  [window attrib]
  (let [res (glfw-get-window-attrib window attrib)]
    (if (boolean-window-hints attrib)
      (not (zero? res))
      (case attrib
        :client-api (client-api->keyword res)
        :context-creation-api (context-api->keyword res)
        :context-robustness (context-robustness->keyword res)
        :context-release-behavior (release-behavior->keyword res)
        :opengl-profile (opengl-profile->keyword res)
        (if (= -1 res)
          :dont-care
          res)))))

(defcfn set-window-attrib
  "Sets the `value` of `attrib` for the `window`.

  Can only set `:decorated`, `:resizable`, `:floating`, `:auto-iconify`, and
  `:focus-on-show`."
  "glfwSetWindowAttrib" [::window ::window-hint ::mem/int] ::mem/void
  glfw-set-window-attrib
  [window attrib value]
  (glfw-set-window-attrib
   window
   attrib
   (if (boolean-window-hints attrib)
     (if value 1 0)
     (if (= :dont-care value)
       -1
       value))))

(defcfn set-window-user-pointer
  "Sets a user-defined pointer value associated with the `window`.

  The current value is retained until the `window` is destroyed."
  "glfwSetWindowUserPointer" [::window ::mem/pointer] ::mem/void)

(defcfn get-window-user-pointer
  "Gets a user-defined pointer value associated with the `window`."
  "glfwGetWindowUserPointer" [::window] ::mem/pointer)

(defalias ::window-pos-fn [::ffi/fn [::window ::mem/int ::mem/int] ::mem/void])

(defcfn set-window-pos-callback
  "Sets the position `callback` for the given `window`.

  The `callback` is a function of the window the event is from and the two ints
  describing the new position of the window.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  "glfwSetWindowPosCallback" [::window ::mem/pointer] ::mem/pointer
  glfw-set-window-pos-callback
  ([window callback] (set-window-pos-callback window callback (mem/global-scope)))
  ([window callback scope]
   (mem/deserialize*
    (glfw-set-window-pos-callback
     window (mem/serialize* callback ::window-pos-fn scope))
    ::window-pos-fn)))

(defalias ::window-size-fn [::ffi/fn [::window ::mem/int ::mem/int] ::mem/void])

(defcfn set-window-size-callback
  "Sets the window resize `callback` for the `window`.

  The `callback` is a function of the window the event is from and the two ints
  describing the new size of the content area of the window.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  "glfwSetWindowSizeCallback" [::window ::mem/pointer] ::mem/pointer
  glfw-set-window-size-callback
  ([window callback] (set-window-size-callback window callback (mem/global-scope)))
  ([window callback scope]
   (mem/deserialize*
    (glfw-set-window-size-callback
     window (mem/serialize* callback ::window-size-fn scope))
    ::window-size-fn)))

(defalias ::window-close-fn [::ffi/fn [::window] ::mem/void])

(defcfn set-window-close-callback
  "Sets the window close `callback` for the `window`.

  The `callback` is a function of the window the event is from.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  "glfwSetWindowCloseCallback" [::window ::mem/pointer] ::mem/pointer
  glfw-set-window-close-callback
  ([window callback] (set-window-close-callback window callback (mem/global-scope)))
  ([window callback scope]
   (mem/deserialize*
    (glfw-set-window-close-callback
     window (mem/serialize* callback ::window-close-fn scope))
    ::window-close-fn)))

(defalias ::window-refresh-fn [::ffi/fn [::window] ::mem/void])

(defcfn set-window-refresh-callback
  "Sets the window refresh `callback` for the `window`.

  The `callback` is a function of the window the event is from.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  "glfwSetWindowRefreshCallback" [::window ::mem/pointer] ::mem/pointer
  glfw-set-window-refresh-callback
  ([window callback] (set-window-refresh-callback window callback (mem/global-scope)))
  ([window callback scope]
   (mem/deserialize*
    (glfw-set-window-refresh-callback
     window (mem/serialize* callback ::window-refresh-fn scope))
    ::window-refresh-fn)))

(defalias ::window-focus-fn [::ffi/fn [::window ::mem/int] ::mem/void])

(defcfn set-window-focus-callback
  "Sets the window focus `callback` for the `window`.

  The `callback` is a function of the window the event is from and a boolean of
  if the window is focused.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  "glfwSetWindowFocusCallback" [::window ::mem/pointer] ::mem/pointer
  glfw-set-window-focus-callback
  ([window callback] (set-window-focus-callback window callback (mem/global-scope)))
  ([window callback scope]
   (mem/deserialize*
    (glfw-set-window-focus-callback
     window (mem/serialize* #(callback %1 (if (zero? %2) false true)) ::window-focus-fn scope))
    ::window-focus-fn)))

(defalias ::window-iconify-fn [::ffi/fn [::window ::mem/int] ::mem/void])

(defcfn set-window-iconify-callback
  "Sets the window iconify `callback` for the `window`.

  The `callback` is a function of the window the event is from and a boolean of
  if the window is iconified.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  "glfwSetWindowIconifyCallback" [::window ::mem/pointer] ::mem/pointer
  glfw-set-window-iconify-callback
  ([window callback] (set-window-iconify-callback window callback (mem/global-scope)))
  ([window callback scope]
   (mem/deserialize*
    (glfw-set-window-iconify-callback
     window (mem/serialize* #(callback %1 (if (zero? %2) false true)) ::window-iconify-fn scope))
    ::window-iconify-fn)))

(defalias ::window-maximize-fn [::ffi/fn [::window ::mem/int] ::mem/void])

(defcfn set-window-maximize-callback
  "Sets the window maximize `callback` for the `window`.

  The `callback` is a function of the window the event is from and a boolean of
  if the window is maximized.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  "glfwSetWindowMaximizeCallback" [::window ::mem/pointer] ::mem/pointer
  glfw-set-window-maximize-callback
  ([window callback] (set-window-maximize-callback window callback (mem/global-scope)))
  ([window callback scope]
   (mem/deserialize*
    (glfw-set-window-maximize-callback
     window (mem/serialize* #(callback %1 (if (zero? %2) false true)) ::window-maximize-fn scope))
    ::window-maximize-fn)))

(defalias ::window-framebuffer-size-fn [::ffi/fn [::window ::mem/int ::mem/int] ::mem/void])

(defcfn set-window-framebuffer-size-callback
  "Sets the window framebuffer size `callback` for the `window`.

  The `callback` is a function of the window the event is from and two ints
  for the size of the framebuffer.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  "glfwSetWindowFramebuffer-SizeCallback" [::window ::mem/pointer] ::mem/pointer
  glfw-set-window-framebuffer-size-callback
  ([window callback] (set-window-framebuffer-size-callback window callback (mem/global-scope)))
  ([window callback scope]
   (mem/deserialize*
    (glfw-set-window-framebuffer-size-callback
     window (mem/serialize* callback ::window-framebuffer-size-fn scope))
    ::window-framebuffer-size-fn)))

(defalias ::window-content-scale-fn [::ffi/fn [::window ::mem/float ::mem/float] ::mem/void])

(defcfn set-window-content-scale-callback
  "Sets the window content scale `callback` for the `window`.

  The `callback` is a function of the window the event is from and two floats
  for the size of the framebuffer.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  "glfwSetWindowContent-ScaleCallback" [::window ::mem/pointer] ::mem/pointer
  glfw-set-window-content-scale-callback
  ([window callback] (set-window-content-scale-callback window callback (mem/global-scope)))
  ([window callback scope]
   (mem/deserialize*
    (glfw-set-window-content-scale-callback
     window (mem/serialize* callback ::window-content-scale-fn scope))
    ::window-content-scale-fn)))

(defcfn poll-events
  "Process all events on all the windows.

  This may call the callbacks which are set, however the callbacks may be called
  at other times depending on the platform.

  On some platforms certain window events will block. The window-refresh
  callback should perform any rendering during these events.

  Joystick input does not depend on event processing."
  "glfwPollEvents" [] ::mem/void)

(defcfn wait-events
  "Blocks the thread until at least one event is available, and processes it.

  See [[poll-events]]."
  "glfwWaitEvents" [] ::mem/void)

(defcfn wait-events-timeout
  "Process events as [[wait-events]], but returns after `timeout` seconds.

  See [[poll-events]]."
  {:arglists '([timeout])}
  "glfwWaitEventsTimeout" [::mem/double] ::mem/void)

(defcfn post-empty-event
  "Post an empty event to the event queue, causing [[wait-events]]
  and [[wait-events-timeout]] to return."
  "glfwPostEmptyEvent" [] ::mem/void)

(defcfn swap-buffers
  "Swap the front and back buffers of a GL or GLES context for `window`.

  If the swap interval is greater than zero, the GPU driver will wait that many
  screen updates before swapping the buffers.

  When rendering with Vulkan, use `vkQueuePresentKHR` instead."
  "glfwSwapBuffers" [::window] ::mem/void)
