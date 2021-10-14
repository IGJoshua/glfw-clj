(ns glfw-clj.core
  (:refer-clojure :rename {keys map-keys})
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [coffi.mem :as mem :refer [defalias]]
   [coffi.ffi :as ffi :refer [defcfn]]))

;;; Utilities

(defn- reverse-map
  "Takes a 1:1 map and returns a map from the values to the keys."
  [m]
  (into {} (map (comp vec reverse)) m))

(defmacro ^:private defenum
  [enum-name val-map]
  (let [plural (if (= \s (last (name enum-name)))
                 "es"
                 "s")
        enum-sym (symbol (str (name enum-name) plural))
        enum->int (symbol (str (name enum-name) "->enum"))
        int->enum (symbol (str "enum->" (name enum-name)))]
    `(do
       (def ^:private ~enum->int
         ~(reduce-kv #(assoc %1 %2 `(int ~%3)) {} val-map))
       (def ^:private ~int->enum
         (reverse-map ~enum->int))
       (def ~enum-sym (set (map-keys ~enum->int)))

       (defmethod mem/primitive-type ~enum-name
         [_type#]
         ::mem/int)
       (defmethod mem/serialize* ~enum-name
         [obj# _type# _scope#]
         (or (~enum->int obj#)
             (some ~enum->int (parents obj#))))
       (defmethod mem/deserialize* ~enum-name
         [obj# _type#]
         (~int->enum obj#)))))
(s/fdef defenum
  :args (s/cat :enum-name qualified-keyword?
               :val-map (s/map-of qualified-keyword? number?)))

(defn ^:private camel-case
  [s]
  (str/replace s #"(^|-)(\S)" #(str/upper-case (nth % 2))))

(defn- defcallback-body
  [fn-name docstring fn-type extra-args]
  (let [type-name (keyword "glfw-clj.core" (str (name fn-name) "-fn"))
        set-var-name (symbol (str "set-" (name fn-name) "-callback"))
        native-symbol (str "glfwSet" (camel-case (name fn-name)) "Callback")]
    `(do
       (defalias ~type-name ~fn-type)
       (defcfn ~(with-meta set-var-name
                  (meta fn-name))
         ~docstring
         ~native-symbol [::mem/pointer] ::mem/pointer
         native-fn#
         ([~@extra-args ~'callback] (~set-var-name ~@extra-args ~'callback (mem/global-scope)))
         ([~@extra-args ~'callback ~'scope]
          (mem/deserialize*
           (native-fn#
            ~@extra-args
            (mem/serialize*
             (fn [~'& args#]
               (try (apply ~'callback args#)
                    ;; NOTE(Joshua): This actually *should* catch Throwable
                    ;; because anything that gets thrown past a callback
                    ;; boundary *will* crash the JVM.
                    (catch Throwable e#
                      (log/error e# ~(str "Caught an exception in callback " (name fn-name)))
                      ;; FIXME(Joshua): If this is used for a callback that
                      ;; returns a non-void type, this will still crash.
                      nil)))
             ~type-name ~'scope))
           ~type-name))))))

(defmacro ^:private defcallback
  [fn-name docstring fn-type]
  (defcallback-body fn-name docstring fn-type nil))
(s/fdef defcallback
  :args (s/cat :fn-name simple-keyword?
               :docstring string?
               :fn-type ::mem/type))

(defmacro ^:private def-wnd-callback
  [fn-name docstring fn-type]
  (defcallback-body fn-name docstring fn-type '(window)))
(s/fdef def-wnd-callback
  :args (s/cat :fn-name simple-keyword?
               :docstring string?
               :fn-type ::mem/type))

(defmacro ^:private with-out-args
  {:style/indent 1}
  [bindings expr]
  (let [segments (repeatedly (/ (count bindings) 2)
                             #(gensym "segment"))
        scope (gensym "scope")]
    `(with-open [~scope (mem/stack-scope)]
       (let [~@(->> bindings
                    (partition 2)
                    (map (partial apply vector) segments)
                    (mapcat (fn [[segment binding type]]
                              [segment `(mem/alloc-instance ~type ~scope)
                               binding `(mem/address-of ~segment)])))]
         ~expr
         [~@(->> bindings
                 (partition 2)
                 (map second)
                 (map (fn [segment type]
                        `(mem/deserialize-from ~segment ~type))
                      segments))]))))
(s/fdef with-out-args
  :args (s/cat :bindings (s/and (s/* (s/cat :binding simple-symbol?
                                            :type ::mem/type))
                                vector?)
               :expr any?))

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

(defenum ::init-hint
  {::joystick-hat-buttons 0x00050001
   ::cocoa-chdir-resources 0x00051001
   ::cocoa-menubar 0x00051002})

(defmethod mem/primitive-type ::bool
  [_type]
  ::mem/int)

(defmethod mem/serialize* ::bool
  [obj _type _scope]
  (int (if obj 1 0)))

(defmethod mem/deserialize* ::bool
  [obj _type]
  (not (zero? obj)))

(defcfn init-hint
  "Sets the given `hint` with the boolean `value`.

  The `hint` must be one of the values from [[init-hints]].

  Hints starting with `:cocoa` are MacOS-specific.

  This must be called before [[init]]."
  "glfwInitHint" [::init-hint ::bool] ::mem/void)

(defcfn get-version
  "Gets a vector of the major, minor, and revision version of GLFW.

  This can be called on any thread, and before [[init]]."
  "glfwGetVersion" [::mem/pointer ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-version
  []
  (with-out-args [major ::mem/int
                  minor ::mem/int
                  rev ::mem/int]
    (glfw-get-version major minor rev)))

(defcfn get-version-string
  "Gets a string of the version of GLFW.

  Don't parse the string to get the version. Instead use [[get-version]].

  This can be called on any thread, and before [[init]]."
  "glfwGetVersionString" [] ::mem/c-string)

(defenum ::error-code
  {::no-error 0x00000000
   ::not-initialized 0x00010001
   ::no-current-context 0x00010002
   ::invalid-enum 0x00010003
   ::invalid-value 0x00010004
   ::out-of-memory 0x00010005
   ::api-unavailable 0x00010006
   ::version-unavailable 0x00010007
   ::platform-error 0x00010008
   ::format-unavailable 0x00010009
   ::no-window-context 0x0001000A})

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
      (when-not (identical? ::no-error error-code)
        (ex-info description-str {:type error-code})))))

(defcallback :error
  "Sets the global error callback for GLFW.

  The callback is a function of an integer error code and a string description.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used. If the callback is called
  by GLFW after the scope has been released, it will cause a JVM crash.

  This function may be called before [[init]]."
  [::ffi/fn [::error-code ::mem/c-string] ::mem/void])

;;; Window Management

(defenum ::window-hint
  {::focused 0x00020001
   ::iconified 0x00020002
   ::resizable 0x00020003
   ::visible 0x00020004
   ::decorated 0x00020005
   ::auto-iconify 0x00020006
   ::floating 0x00020007
   ::maximized 0x00020008
   ::center-cursor 0x00020009
   ::transparent-framebuffer 0x0002000A
   ::hovered 0x0002000B
   ::focus-on-show 0x0002000C
   ::red-bits 0x00021001
   ::green-bits 0x00021002
   ::blue-bits 0x00021003
   ::alpha-bits 0x00021004
   ::depth-bits 0x00021005
   ::stencil-bits 0x00021006
   ::accum-red-bits 0x00021007
   ::accum-green-bits 0x00021008
   ::accum-blue-bits 0x00021009
   ::accum-alpha-bits 0x0002100A
   ::aux-buffers 0x0002100B
   ::stereo 0x0002100C
   ::samples 0x0002100D
   ::srgb-capable 0x0002100E
   ::refresh-rate 0x0002100F
   ::doublebuffer 0x00021010
   ::client-api 0x00022001
   ::context-version-major 0x00022002
   ::context-version-minor 0x00022003
   ::context-revision 0x00022004
   ::context-robustness 0x00022005
   ::opengl-forward-compat 0x00022006
   ::opengl-debug-context 0x00022007
   ::opengl-profile 0x00022008
   ::context-release-behavior 0x00022009
   ::context-no-error 0x0002200A
   ::context-creation-api 0x0002200B
   ::scale-to-monitor 0x0002200C
   ::cocoa-retina-framebuffer 0x00023001
   ::cocoa-frame-name 0x00023002
   ::cocoa-graphics-switching 0x00023003
   ::x11-class-name 0x00024001
   ::x11-instance-name 0x00024002})

(def ^:private boolean-window-hints
  #{::resizable ::visible ::decorated ::focused
    ::auto-iconify ::floating ::maximized ::center-cursor
    ::transparent-framebuffer ::focus-on-show ::scale-to-monitor
    ::stereo ::srgb-capable ::doublebuffer
    ::opengl-forward-compat ::opengl-debug-context
    ::cocoa-retina-framebuffer ::cocoa-graphics-switching})

(defcfn default-window-hints
  "Resets all the window creation init-hints to their default values."
  "glfwDefaultWindowHints" [] ::mem/void)

(def ^:private client-api->enum
  {::opengl-api 0x00030001
   ::opengl-es-api 0x00030002
   ::no-api 0})
(def ^:private enum->client-api (reverse-map client-api->enum))
(def client-api-opts (set (map-keys client-api->enum)))

(def ^:private context-api->enum
  {::native-context-api 0x00036001
   ::egl-context-api 0x00036002
   ::osmesa-context-api 0x00036003})
(def ^:private enum->context-api (reverse-map context-api->enum))
(def context-api-opts (set (map-keys context-api->enum)))

(def ^:private context-robustness->enum
  {::no-robustness 0
   ::no-reset-notification 0x00031001
   ::lose-context-on-reset 0x00031002})
(def ^:private enum->context-robustness (reverse-map context-robustness->enum))
(def context-robustness-opts (set (map-keys context-robustness->enum)))

(def ^:private release-behavior->enum
  {::any-release-behavior 0
   ::release-behavior-none 0x00035002
   ::release-behavior-flush 0x00035001})
(def ^:private enum->release-behavior (reverse-map release-behavior->enum))
(def release-behavior-opts (set (map-keys release-behavior->enum)))

(def ^:private opengl-profile->enum
  {::opengl-any-profile 0
   ::opengl-core-profile 0x00032001
   ::opengl-compat-profile 0x00032002})
(def ^:private enum->opengl-profile (reverse-map opengl-profile->enum))
(def opengl-profile-opts (set (map-keys opengl-profile->enum)))

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
       ::client-api (client-api->enum (or value ::opengl-api))
       ::context-creation-api (context-api->enum (or value ::native-context-api))
       ::context-robustness (context-robustness->enum (or value ::no-robustness))
       ::context-release-behavior (release-behavior->enum (or value ::any-release-behavior))
       ::opengl-profile (opengl-profile->enum (or value ::opengl-any-profile))
       (if (identical? ::dont-care value)
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
  "glfwSetWindowShouldClose" [::window ::bool] ::mem/void)

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
  (with-out-args [xpos ::mem/int
                  ypos ::mem/int]
    (glfw-get-window-pos window xpos ypos)))

(defcfn set-window-pos
  "Sets the `x` and `y` positions of the given `window`."
  {:arglists '([window x y])}
  "glfwSetWindowPos" [::window ::mem/int ::mem/int] ::mem/void)

(defcfn get-window-size
  "Gets the width and height of the content area of the given `window` as a vector."
  "glfwGetWindowSize" [::window ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-window-size
  [window]
  (with-out-args [width ::mem/int
                  height ::mem/int]
    (glfw-get-window-size window width height)))

(defcfn set-window-size-limits
  "Sets the size limits of the content area of the window"
  "glfwSetWindowSizeLimits" [::window ::mem/int ::mem/int ::mem/int ::mem/int] ::mem/void
  glfw-set-window-size-limits
  [window min-width min-height max-width max-height]
  (apply glfw-set-window-size-limits
         window
         (map #(if (identical? ::dont-care %) -1 %) [min-width min-height max-width max-height])))

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
  (with-out-args [width ::mem/int
                  height ::mem/int]
    (glfw-get-framebuffer-size window width height)))

(defcfn get-window-frame-size
  "Gets the size of the `window` (including decorations).

  Returns a vector of the pixel lengths of the left, top, right, and bottom
  edges of the window."
  "glfwGetWindowFrameSize"
  [::window ::mem/pointer ::mem/pointer ::mem/pointer ::mem/pointer]
  ::mem/void
  glfw-get-window-frame-size
  [window]
  (with-out-args [left ::mem/int
                  top ::mem/int
                  right ::mem/int
                  bottom ::mem/int]
    (glfw-get-window-frame-size window left top right bottom)))

(defcfn get-window-content-scale
  "Gets the current content scale for the given `window`.

  The content scale is the ratio between the current DPI and the platform default DPI."
  "glfwGetWindowContentScale" [::window ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-window-content-scale
  [window]
  (with-out-args [x-scale ::mem/float
                  y-scale ::mem/float]
    (glfw-get-window-content-scale window x-scale y-scale)))

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
                            (if (identical? :dont-care refresh-rate) -1 refresh-rate))))

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
        ::client-api (enum->client-api res)
        ::context-creation-api (enum->context-api res)
        ::context-robustness (enum->context-robustness res)
        ::context-release-behavior (enum->release-behavior res)
        ::opengl-profile (enum->opengl-profile res)
        (if (= -1 res)
          ::dont-care
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
     (if (identical? ::dont-care value)
       -1
       value))))

(defcfn set-window-user-pointer
  "Sets a user-defined pointer value associated with the `window`.

  The current value is retained until the `window` is destroyed."
  "glfwSetWindowUserPointer" [::window ::mem/pointer] ::mem/void)

(defcfn get-window-user-pointer
  "Gets a user-defined pointer value associated with the `window`."
  "glfwGetWindowUserPointer" [::window] ::mem/pointer)

(def-wnd-callback :window-pos
  "Sets the position `callback` for the given `window`.

  The `callback` is a function of the window the event is from and the two ints
  describing the new position of the window.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  [::ffi/fn [::window ::mem/int ::mem/int] ::mem/void])

(def-wnd-callback :window-size
  "Sets the window resize `callback` for the `window`.

  The `callback` is a function of the window the event is from and the two ints
  describing the new size of the content area of the window.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  [::ffi/fn [::window ::mem/int ::mem/int] ::mem/void])

(def-wnd-callback :window-close
  "Sets the window close `callback` for the `window`.

  The `callback` is a function of the window the event is from.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  [::ffi/fn [::window] ::mem/void])

(def-wnd-callback :window-refresh
  "Sets the window refresh `callback` for the `window`.

  The `callback` is a function of the window the event is from.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  [::ffi/fn [::window] ::mem/void])

(def-wnd-callback :window-focus
  "Sets the window focus `callback` for the `window`.

  The `callback` is a function of the window the event is from and a boolean of
  if the window is focused.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  [::ffi/fn [::window ::mem/int] ::mem/void])

(def-wnd-callback :window-iconify
  "Sets the window iconify `callback` for the `window`.

  The `callback` is a function of the window the event is from and a boolean of
  if the window is iconified.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  [::ffi/fn [::window ::mem/int] ::mem/void])

(def-wnd-callback :window-maximize
  "Sets the window maximize `callback` for the `window`.

  The `callback` is a function of the window the event is from and a boolean of
  if the window is maximized.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  [::ffi/fn [::window ::mem/int] ::mem/void])

(def-wnd-callback :framebuffer-size
  "Sets the window framebuffer size `callback` for the `window`.

  The `callback` is a function of the window the event is from and two ints
  for the size of the framebuffer.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  [::ffi/fn [::window ::mem/int ::mem/int] ::mem/void])

(def-wnd-callback :window-content-scale
  "Sets the window content scale `callback` for the `window`.

  The `callback` is a function of the window the event is from and two floats
  for the size of the framebuffer.

  Returns the previous callback, or nil if there was none.

  If `scope` is passed, the callback will be kept valid for the duration of that
  scope. If it is not, a [[mem/global-scope]] is used."
  [::ffi/fn [::window ::mem/float ::mem/float] ::mem/void])

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

;;; Context Creation

(defcfn make-context-current
  "Binds the context for the given `window` to the current thread.

  Only one context can be bound on the thread at once. If nil is passed, any
  context already bound to this thread will be released."
  "glfwMakeContextCurrent" [::window] ::mem/void)

(defcfn get-current-context
  "Gets the window whose context is bound to the current thread."
  "glfwGetCurrentContext" [] ::window)

(defcfn swap-interval
  "Sets the number of vblanks to wait for before showing the buffer.

  A context must be current on this thread to call this function.

  This function does not apply to Vulkan. If you are rendering using Vulkan, set
  the present mode of your swapchain instead.

  See [[make-context-current]]."
  "glfwSwapInterval" [::mem/int] ::mem/void)

(defcfn extension-supported
  "Checks if a given api extension is supported for the given context.

  This can search both for context creation extensions and client extensions.

  This function does not apply to Vulkan. If you are using Vulkan
  see [[get-required-instance-extensions]], and use
  `vkEnumerateInstanceExtensionProperties` and
  `vkEnumerateDeviceExtensionProperties` instead."
  {:arglists '([extension-name])}
  "glfwExtensionSupported" [::mem/c-string] ::bool)

(defcfn get-proc-address
  "Returns a pointer to the specified procedure.

  The returned address is not guaranteed to be the same between contexts.

  This function may return a non-nil value even for unsupported functions.
  Always check the version and extensions supported first.

  This function does not apply if you are using Vulkan. If you are using Vulkan,
  see [[get-instance-proc-address]], and use `vkGetInstanceProcAddr` and
  `vkGetDeviceProcAddr` instead.

  The pointer is valid until the context is destroyed."
  {:arglists '([proc-name])}
  "glfwGetProcAddress" [::mem/c-string] ::mem/pointer)

;;; Monitors

(defcfn get-monitors
  "Gets a sequence of opaque monitor objects."
  "glfwGetMonitors" [::mem/pointer] ::mem/pointer
  glfw-get-monitors
  []
  (with-open [scope (mem/stack-scope)]
    (let [count (mem/alloc-instance ::mem/int scope)
          monitors (glfw-get-monitors (mem/address-of count))
          count (mem/deserialize-from count ::mem/int)
          array-size (mem/size-of [::mem/array ::monitor count])
          monitors (mem/slice-global monitors array-size)]
      (mem/seq-of ::monitor monitors))))

(defcfn get-primary-monitor
  "Gets an opaque handle to the current primary monitor."
  "glfwGetPrimaryMonitor" [] ::monitor)

(defcfn get-monitor-pos
  "Gets the virtual screen coordinate of the upper left corner of the `monitor`."
  "glfwGetMonitorPos" [::monitor ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-monitor-pos
  [monitor]
  (with-out-args [xpos ::mem/int
                  ypos ::mem/int]
    (glfw-get-monitor-pos monitor xpos ypos)))

(defcfn get-monitor-workarea
  "Gets the upper left point and extents of the `monitor`'s work area.

  This is returned as a vector of the x and y position of the upper left corner
  of the monitor, and the width and height of the work area.

  The work area excludes things like a menubar or hotbar.

  If there is no OS hotbar, the width and height are the resolution of the
  monitor in screen coordinates."
  "glfwGetMonitorWorkarea"
  [::monitor ::mem/pointer ::mem/pointer ::mem/pointer ::mem/pointer]
  ::mem/void
  glfw-get-monitor-workarea
  [monitor]
  (with-out-args [xpos ::mem/int
                  ypos ::mem/int
                  width ::mem/int
                  height ::mem/int]
    (glfw-get-monitor-workarea monitor xpos ypos width height)))

(defcfn get-monitor-physical-size
  "Gets the size of the `monitor` in millimeters.

  Returns a vector of the width and height."
  "glfwGetMonitorPhysicalSize" [::monitor ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-monitor-physical-size
  [monitor]
  (with-out-args [width ::mem/int
                  height ::mem/int]
    (glfw-get-monitor-physical-size monitor width height)))

(defcfn get-monitor-content-scale
  "Gets the x and y scale of the content in the `monitor`.

  The content scale is the ratio between the current DPI and the platform
  default DPI."
  "glfwGetMonitorContentScale" [::monitor ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-monitor-content-scale
  [monitor]
  (with-out-args [xscale ::mem/float
                  yscale ::mem/float]
    (glfw-get-monitor-content-scale monitor xscale yscale)))

(defcfn get-monitor-name
  "Gets a human-readable, non-unique name for the `monitor`."
  "glfwGetMonitorName" [::monitor] ::mem/c-string)

(defcfn set-monitor-user-pointer
  "Sets a user-defined pointer for the `monitor`.

  Useful when code needs to rely on the monitor but not on global state."
  "glfwSetMonitorUserPointer" [::monitor ::mem/pointer] ::mem/void)

(defcfn get-monitor-user-pointer
  "Gets the user-defined pointer for the `monitor`."
  "glfwGetMonitorUserPointer" [::monitor] ::mem/pointer)

(defmethod mem/primitive-type ::connection-event
  [_type]
  ::mem/int)

(defmethod mem/deserialize* ::connection-event
  [obj _type]
  (case obj
    0x00040001 ::connected
    0x00040002 ::disconnected))

(defcallback :monitor
  "Set a callback to be called whenever the monitor configuration changes.

  The callback is a function of a monitor and one of `:connected` or
  `:disconnected`."
  [::ffi/fn [::monitor ::connection-event] ::mem/void])

(defalias ::vidmode
  [::mem/struct
   [[:width ::mem/int]
    [:height ::mem/int]
    [:red-bits ::mem/int]
    [:green-bits ::mem/int]
    [:blue-bits ::mem/int]
    [:refresh-rate ::mem/int]]])

(defcfn get-video-modes
  "Get all video modes supported by the `monitor`.

  Each video mode is a map with the keys `:width`, `:height`, `:red-bits`,
  `:green-bits`, `:blue-bits`, and `:refresh-rate`, all of which are integer
  values."
  "glfwGetVideoModes" [::monitor ::mem/pointer] ::mem/pointer
  glfw-get-video-modes
  [monitor]
  (with-open [scope (mem/stack-scope)]
    (let [count (mem/alloc-instance ::mem/int scope)
          vidmodes (glfw-get-video-modes monitor (mem/address-of count))
          count (mem/deserialize-from count ::mem/int)
          vidmodes (mem/slice-global vidmodes (mem/size-of [::mem/array ::vidmode count]))]
      (mem/seq-of ::vidmode vidmodes))))

(defcfn get-video-mode
  "Get the current video mode set on the `monitor`.

  See [[get-video-modes]]."
  "glfwGetVideoMode" [::monitor] [::mem/pointer ::vidmode])

(defcfn set-gamma
  "Generates an appropriately sized gamma ramp for the exponent and
  calls [[set-gamma-ramp]] with it."
  {:arglists '([monitor gamma])}
  "glfwSetGamma" [::monitor ::mem/float] ::mem/void)

(def ^:private gamma-ramp-type
  [::mem/struct
   [[:red ::mem/pointer]
    [:green ::mem/pointer]
    [:blue ::mem/pointer]
    [:size ::mem/int]
    [:padding [::mem/padding 4]]]])

(defmethod mem/c-layout ::gamma-ramp
  [_type]
  (mem/c-layout gamma-ramp-type))

(defmethod mem/serialize-into ::gamma-ramp
  [obj _type segment scope]
  (let [size (count obj)
        reds (mem/serialize (map :red obj) [::mem/array ::mem/short size] scope)
        greens (mem/serialize (map :green obj) [::mem/array ::mem/short size] scope)
        blues (mem/serialize (map :blue obj) [::mem/array ::mem/short size] scope)]
    (mem/serialize-into
     {:red (mem/address-of reds)
      :green (mem/address-of greens)
      :blue (mem/address-of blues)
      :size size}
     gamma-ramp-type
     segment
     scope)))

(defmethod mem/deserialize-from ::gamma-ramp
  [segment _type]
  (let [struct (mem/deserialize-from segment gamma-ramp-type)
        size (:size struct)
        reds (mem/deserialize* (:red struct) [::mem/pointer [::mem/array ::mem/short size]])
        greens (mem/deserialize* (:green struct) [::mem/pointer [::mem/array ::mem/short size]])
        blues (mem/deserialize* (:blue struct) [::mem/pointer [::mem/array ::mem/short size]])]
    (map (fn [red green blue]
           {:red (Short/toUnsignedInt red)
            :green (Short/toUnsignedInt green)
            :blue (Short/toUnsignedInt blue)})
         reds greens blues)))

(defcfn get-gamma-ramp
  "Gets the gamma ramp for the `monitor`.

  The gamma ramp is a sequence of maps with the keys `:red`, `:green`, and
  `:blue`, all of which map to unsigned shorts."
  "glfwGetGammaRamp" [::monitor] [::mem/pointer ::gamma-ramp])

(defcfn set-gamma-ramp
  "Sets the gamma ramp for the `monitor`.

  See [[get-gamma-ramp]]."
  {:arglists '([monitor gamma-ramp])}
  "glfwSetGammaRamp" [::monitor [::mem/pointer ::gamma-ramp]] ::mem/void)
