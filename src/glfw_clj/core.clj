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

(def ^:private keyword->client-api
  {:opengl 0x00030001
   :opengl-es 0x00030002
   :none 0})
(def client-api-opts (set (keys keyword->client-api)))

(def ^:private keyword->context-api
  {:native 0x00036001
   :egl 0x00036002
   :osmesa 0x00036003})
(def context-api-opts (set (keys keyword->context-api)))

(def ^:private keyword->context-robustness
  {:no-robustness 0
   :no-reset-notification 0x00031001
   :lose-context-on-reset 0x00031002})
(def context-robustness-opts (set (keys keyword->context-robustness)))

(def ^:private keyword->release-behavior
  {:any 0
   :none 0x00035002
   :flush 0x00035001})
(def release-behavior-opts (set (keys keyword->release-behavior)))

(def ^:private keyword->opengl-profile
  {:any 0
   :core 0x00032001
   :compat 0x00032002})

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
