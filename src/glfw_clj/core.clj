(ns glfw-clj.core
  "Wrappers for GLFW functions.

  Any function that does not explicitly say it can be called from any thread
  should be called only from the main thread.

  GLFW enums are represented with keywords in this namespace sans the `GLFW_`
  prefix, and are in lower kebab case. `GLFW_TRUE` and `GLFW_FALSE` are
  represented with booleans."
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
         [obj# _type# _arena#]
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
  [fn-name docstring fn-type & {:keys [window?]}]
  (let [type-name (keyword "glfw-clj.core" (str (name fn-name) "-fn"))
        set-var-name (symbol (str "set-" (name fn-name) "-callback"))
        native-symbol (str "glfwSet" (camel-case (name fn-name)) "Callback")]
    `(do
       (defalias ~type-name ~fn-type)
       (defcfn ~(with-meta set-var-name
                  (meta fn-name))
         ~docstring
         ~native-symbol [~@(when window? [::window]) ::mem/pointer] ::mem/pointer
         native-fn#
         ([~@(when window? '[window]) ~'callback]
          (~set-var-name ~@(when window? '[window]) ~'callback (mem/global-arena)))
         ([~@(when window? '[window]) ~'callback ~'arena]
          (mem/deserialize*
           (native-fn#
            ~@(when window? '[window])
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
             ~type-name ~'arena))
           ~type-name))))))

(defmacro ^:private defcallback
  [fn-name docstring fn-type]
  (defcallback-body fn-name docstring fn-type))
(s/fdef defcallback
  :args (s/cat :fn-name simple-keyword?
               :docstring string?
               :fn-type ::mem/type))

(defmacro ^:private def-wnd-callback
  [fn-name docstring fn-type]
  (defcallback-body fn-name docstring fn-type :window? true))
(s/fdef def-wnd-callback
  :args (s/cat :fn-name simple-keyword?
               :docstring string?
               :fn-type ::mem/type))

(defmacro ^:private with-out-args
  {:style/indent 1}
  [bindings expr]
  (let [segments (repeatedly (/ (count bindings) 2)
                             #(gensym "segment"))
        arena (gensym "arena")]
    `(with-open [~arena (mem/confined-arena)]
       (let [~@(->> bindings
                    (partition 2)
                    (map (partial apply vector) segments)
                    (mapcat (fn [[segment binding type]]
                              [segment `(mem/alloc-instance ~type ~arena)
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
  [obj _type _arena]
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
  (with-open [arena (mem/confined-arena)]
    (let [description (mem/alloc-instance ::mem/pointer arena)
          error-code (glfw-get-error (mem/address-of description))
          description-str (mem/deserialize-from description ::mem/c-string)]
      (when-not (identical? ::no-error error-code)
        (ex-info description-str {:type error-code})))))

(defcallback :error
  "Sets the global error callback for GLFW.

  The callback is a function of an integer error code and a string description.

  Returns the previous callback, or nil if there was none.

  If `arena` is passed, the callback will be kept valid for the duration of that
  arena. If it is not, a [[mem/global-arena]] is used. If the callback is called
  by GLFW after the arena has been released, it will cause a JVM crash.

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

(def ^:private string-window-hints
  #{::cocoa-frame-name ::x11-class-name :x11-instance-name})

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

(defcfn window-hint-string
  "Sets a string-valued window hint for the next window to be created."
  {:arglists '([hint value])}
  "glfwWindowHintString" [::window-hint ::mem/c-string] ::mem/void)

(defcfn window-hint
  "Sets a window hint for the next window to be created."
  "glfwWindowHint" [::window-hint ::mem/int] ::mem/void
  glfw-window-hint
  [hint value]
  (if (string-window-hints hint)
    (window-hint-string hint value)
    (glfw-window-hint
     hint
     (if (boolean-window-hints hint)
       (if value 1 0)
       (case hint
         ::client-api (client-api->enum (or value ::opengl-api))
         ::context-creation-api (context-api->enum (or value ::native-context-api))
         ::context-robustness (context-robustness->enum (or value ::no-robustness))
         ::context-release-behavior (release-behavior->enum (or value ::any-release-behavior))
         ::opengl-profile (opengl-profile->enum (or value ::opengl-any-profile))
         (if (identical? ::dont-care value)
           -1
           value))))))

(defn window-hints
  "Sets the default window hints with changes specified by the map `hints`."
  [hints]
  (default-window-hints)
  (run! window-hint hints))

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
  (with-open [arena (mem/confined-arena)]
    (let [image-array (mem/alloc-instance [::mem/array ::image (count images)] arena)]
      (dorun
       (map (fn [segment image]
              (mem/serialize-into image ::image segment arena))
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

  If `arena` is passed, the callback will be kept valid for the duration of that
  arena. If it is not, a [[mem/global-arena]] is used."
  [::ffi/fn [::window ::mem/int ::mem/int] ::mem/void])

(def-wnd-callback :window-size
  "Sets the window resize `callback` for the `window`.

  The `callback` is a function of the window the event is from and the two ints
  describing the new size of the content area of the window.

  Returns the previous callback, or nil if there was none.

  If `arena` is passed, the callback will be kept valid for the duration of that
  arena. If it is not, a [[mem/global-arena]] is used."
  [::ffi/fn [::window ::mem/int ::mem/int] ::mem/void])

(def-wnd-callback :window-close
  "Sets the window close `callback` for the `window`.

  The `callback` is a function of the window the event is from.

  Returns the previous callback, or nil if there was none.

  If `arena` is passed, the callback will be kept valid for the duration of that
  arena. If it is not, a [[mem/global-arena]] is used."
  [::ffi/fn [::window] ::mem/void])

(def-wnd-callback :window-refresh
  "Sets the window refresh `callback` for the `window`.

  The `callback` is a function of the window the event is from.

  Returns the previous callback, or nil if there was none.

  If `arena` is passed, the callback will be kept valid for the duration of that
  arena. If it is not, a [[mem/global-arena]] is used."
  [::ffi/fn [::window] ::mem/void])

(def-wnd-callback :window-focus
  "Sets the window focus `callback` for the `window`.

  The `callback` is a function of the window the event is from and a boolean of
  if the window is focused.

  Returns the previous callback, or nil if there was none.

  If `arena` is passed, the callback will be kept valid for the duration of that
  arena. If it is not, a [[mem/global-arena]] is used."
  [::ffi/fn [::window ::mem/int] ::mem/void])

(def-wnd-callback :window-iconify
  "Sets the window iconify `callback` for the `window`.

  The `callback` is a function of the window the event is from and a boolean of
  if the window is iconified.

  Returns the previous callback, or nil if there was none.

  If `arena` is passed, the callback will be kept valid for the duration of that
  arena. If it is not, a [[mem/global-arena]] is used."
  [::ffi/fn [::window ::mem/int] ::mem/void])

(def-wnd-callback :window-maximize
  "Sets the window maximize `callback` for the `window`.

  The `callback` is a function of the window the event is from and a boolean of
  if the window is maximized.

  Returns the previous callback, or nil if there was none.

  If `arena` is passed, the callback will be kept valid for the duration of that
  arena. If it is not, a [[mem/global-arena]] is used."
  [::ffi/fn [::window ::mem/int] ::mem/void])

(def-wnd-callback :framebuffer-size
  "Sets the window framebuffer size `callback` for the `window`.

  The `callback` is a function of the window the event is from and two ints
  for the size of the framebuffer.

  Returns the previous callback, or nil if there was none.

  If `arena` is passed, the callback will be kept valid for the duration of that
  arena. If it is not, a [[mem/global-arena]] is used."
  [::ffi/fn [::window ::mem/int ::mem/int] ::mem/void])

(def-wnd-callback :window-content-scale
  "Sets the window content scale `callback` for the `window`.

  The `callback` is a function of the window the event is from and two floats
  for the size of the framebuffer.

  Returns the previous callback, or nil if there was none.

  If `arena` is passed, the callback will be kept valid for the duration of that
  arena. If it is not, a [[mem/global-arena]] is used."
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
  (with-open [arena (mem/confined-arena)]
    (let [count (mem/alloc-instance ::mem/int arena)
          monitors (glfw-get-monitors (mem/address-of count))
          count (mem/deserialize-from count ::mem/int)
          array-size (mem/size-of [::mem/array ::monitor count])
          monitors (mem/as-segment monitors array-size)]
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
  (with-open [arena (mem/confined-arena)]
    (let [count (mem/alloc-instance ::mem/int arena)
          vidmodes (glfw-get-video-modes monitor (mem/address-of count))
          count (mem/deserialize-from count ::mem/int)
          vidmodes (mem/as-segment vidmodes (mem/size-of [::mem/array ::vidmode count]))]
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
  [obj _type segment arena]
  (let [size (count obj)
        reds (mem/serialize (map :red obj) [::mem/array ::mem/short size] arena)
        greens (mem/serialize (map :green obj) [::mem/array ::mem/short size] arena)
        blues (mem/serialize (map :blue obj) [::mem/array ::mem/short size] arena)]
    (mem/serialize-into
     {:red (mem/address-of reds)
      :green (mem/address-of greens)
      :blue (mem/address-of blues)
      :size size}
     gamma-ramp-type
     segment
     arena)))

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

;;; Input

(defenum ::input-mode
  {::cursor 0x00033001
   ::sticky-map-keys 0x00033002
   ::sticky-mouse-buttons 0x00033003
   ::lock-key-mods 0x00033004
   ::raw-mouse-motion 0x00033005})

(def ^:private enum->cursor-mode
  {(int 0x00034001) ::cursor-normal
   (int 0x00034002) ::cursor-hidden
   (int 0x00034003) ::cursor-disabled})
(def ^:private cursor-mode->enum (reverse-map enum->cursor-mode))
(def cursor-modes (set (map-keys cursor-mode->enum)))

(defcfn get-input-mode
  "Gets the value of the given `input-mode` for the `window`.

  Values for `:glfw-clj.core/cursor` are `:glfw-clj.core/normal`, as well as
  `hidden`, and `disabled` keywords in the same ns.

  Other modes are either true or false.

  See [[input-modes]]."
  "glfwGetInputMode" [::window ::input-mode] ::mem/int
  glfw-get-input-mode
  [window input-mode]
  (let [result (glfw-get-input-mode window input-mode)]
    (if (identical? ::cursor input-mode)
      (enum->cursor-mode result)
      (not (zero? result)))))

(defcfn set-input-mode
  "Sets the `input-mode` for the given `window`.

  Mode values for `:glfw-clj.core/cursor` are `:glfw-clj.core/normal`, as well
  as `hidden`, and `disabled` keywords in the same ns.

  Other modes take true or false.

  See [[input-modes]]."
  "glfwSetInputMode" [::window ::input-mode ::mem/int] ::mem/void
  glfw-set-input-mode
  [window input-mode mode]
  (glfw-set-input-mode
   window
   input-mode
   (if (identical? ::cursor input-mode)
     (cursor-mode->enum mode)
     (if mode 1 0))))

(defcfn raw-mouse-motion-supported
  "Returns if raw mouse motion is supported by the current system.

  This value will not change after [[init]] is called, so it needs only be
  called once.

  Raw mouse motion is useful in situations where mouse input is used for
  something besides moving a cursor, and as such raw mouse motion is only
  provided when the cursor input mode is disabled."
  "glfwRawMouseMotionSupported" [] ::bool)

(defenum ::key
  {::key-unknown -1
   ::key-space 32
   ::key-apostrophe 39
   ::key-comma 44
   ::key-minus 45
   ::key-period 46
   ::key-slash 47
   ::key-0 48
   ::key-1 49
   ::key-2 50
   ::key-3 51
   ::key-4 52
   ::key-5 53
   ::key-6 54
   ::key-7 55
   ::key-8 56
   ::key-9 57
   ::key-semicolon 59
   ::key-equal 61
   ::key-a 65
   ::key-b 66
   ::key-c 67
   ::key-d 68
   ::key-e 69
   ::key-f 70
   ::key-g 71
   ::key-h 72
   ::key-i 73
   ::key-j 74
   ::key-k 75
   ::key-l 76
   ::key-m 77
   ::key-n 78
   ::key-o 79
   ::key-p 80
   ::key-q 81
   ::key-r 82
   ::key-s 83
   ::key-t 84
   ::key-u 85
   ::key-v 86
   ::key-w 87
   ::key-x 88
   ::key-y 89
   ::key-z 90
   ::key-left-bracket 91
   ::key-backslash 92
   ::key-right-bracket 93
   ::key-grave-accent 96
   ::key-world-1 161
   ::key-world-2 162
   ::key-escape 256
   ::key-enter 257
   ::key-tab 258
   ::key-backspace 259
   ::key-insert 260
   ::key-delete 261
   ::key-right 262
   ::key-left 263
   ::key-down 264
   ::key-up 265
   ::key-page-up 266
   ::key-page-down 267
   ::key-home 268
   ::key-end 269
   ::key-caps-lock 280
   ::key-scroll-lock 281
   ::key-num-lock 282
   ::key-print-screen 283
   ::key-pause 284
   ::key-f1 290
   ::key-f2 291
   ::key-f3 292
   ::key-f4 293
   ::key-f5 294
   ::key-f6 295
   ::key-f7 296
   ::key-f8 297
   ::key-f9 298
   ::key-f10 299
   ::key-f11 300
   ::key-f12 301
   ::key-f13 302
   ::key-f14 303
   ::key-f15 304
   ::key-f16 305
   ::key-f17 306
   ::key-f18 307
   ::key-f19 308
   ::key-f20 309
   ::key-f21 310
   ::key-f22 311
   ::key-f23 312
   ::key-f24 313
   ::key-f25 314
   ::key-kp-0 320
   ::key-kp-1 321
   ::key-kp-2 322
   ::key-kp-3 323
   ::key-kp-4 324
   ::key-kp-5 325
   ::key-kp-6 326
   ::key-kp-7 327
   ::key-kp-8 328
   ::key-kp-9 329
   ::key-kp-decimal 330
   ::key-kp-divide 331
   ::key-kp-multiply 332
   ::key-kp-subtract 333
   ::key-kp-add 334
   ::key-kp-enter 335
   ::key-kp-equal 336
   ::key-left-shift 340
   ::key-left-control 341
   ::key-left-alt 342
   ::key-left-super 343
   ::key-right-shift 344
   ::key-right-control 345
   ::key-right-alt 346
   ::key-right-super 347
   ::key-menu 348})

(defcfn get-key-name
  "Get the printable name of a key as a string.

  This is generally the character the key will produce without any modifier
  keys, intended for displaying keybindings to a user.

  If the `key` is nil or `:glfw-clj.core/key-unknown`, it is ignored and
  `scancode` is used, otherwise `scancode` is ignored.

  For non-printable keys, returns nil."
  {:arglists '([key scancode])}
  "glfwGetKeyName" [::key ::mem/int] ::mem/c-string
  glfw-get-key-name
  [key scancode]
  (glfw-get-key-name (or key ::key-unknown) (or scancode 0)))

(defcfn get-key-scancode
  "Gets the scancode of the `key`.

  If the `key` is unknown or nil, or the key doesn't exist on the keyboard,
  returns nil."
  "glfwGetKeyScancode" [::key] ::mem/int
  glfw-get-key-scancode
  [key]
  (let [ret (glfw-get-key-scancode (or key ::key-unknown))]
    (if (not= -1 ret)
      ret
      nil)))

(defenum ::key-event
  {::press 1
   ::release 0
   ::repeat 2})

(defcfn get-key
  "Gets the most recent key event for the `key` in `window`.

  Returns either `:glfw-clj.core/press` or `:glfw-clj.core/release`. To get the
  higher-level input `:glfw-clj.core/repeat` you must use the callback
  via [[set-key-callback]]."
  "glfwGetKey" [::window ::key] ::key-event)

(defenum ::mouse-button
  {::mouse-button-1 0
   ::mouse-button-2 1
   ::mouse-button-3 2
   ::mouse-button-4 3
   ::mouse-button-5 4
   ::mouse-button-6 5
   ::mouse-button-7 6
   ::mouse-button-8 7})

(derive ::mouse-button-left ::mouse-button-1)
(derive ::mouse-button-right ::mouse-button-2)
(derive ::mouse-button-middle ::mouse-button-3)

(defcfn get-mouse-button
  "Gets the most recent button event for the `mouse-button` in `window`.

  Returns either `:glfw-clj.core/press` or `:glfw-clj.core/release` "
  "glfwGetMouseButton" [::window ::mouse-button] ::key-event)

(defcfn get-cursor-pos
  "Gets the current cursor position as a vector of the x and y.

  The coordinates are screen coordinates starting from the top left corner of
  the content area of the `window`.

  If the cursor is disabled then the value is not connected to the `window`
  directly and is unbounded."
  "glfwGetCursorPos" [::window ::mem/pointer ::mem/pointer] ::mem/void
  glfw-get-cursor-pos
  [window]
  (with-out-args [xpos ::mem/double
                  ypos ::mem/double]
    (glfw-get-cursor-pos window xpos ypos)))

(defcfn set-cursor-pos
  "Sets the position of the cursor relative to the `window`.

  Do not use this function to implement things like camera controls. Instead set
  the cursor mode to disabled."
  {:arglists '([window xpos ypos])}
  "glfwSetCursorPos" [::window ::mem/double ::mem/double] ::mem/void)

(defalias ::cursor ::mem/pointer)

(defcfn create-cursor
  "Creates and returns a custom cursor object.

  The resulting cursor object should be cleaned up with [[destroy-cursor]].

  The hotspot is the location in pixels of the point on the cursor which will be
  reported as its location, starting from the top left.

  See [[set-cursor]], [[set-window-icon]]."
  {:arglists '([image hotspot-x hotspot-y])}
  "glfwCreateCursor" [::image ::mem/int ::mem/int] ::cursor)

(defenum ::standard-cursor
  {::arrow-cursor 0x00036001
   ::ibeam-cursor 0x00036002
   ::crosshair-cursor 0x00036003
   ::hand-cursor 0x00036004
   ::hresize-cursor 0x00036005
   ::vresize-cursor 0x00036006})

(defcfn create-standard-cursor
  "Create a cursor object with a standard shape.

  See [[standard-cursors]], [[create-cursor]]."
  "glfwCreateStandardCursor" [::standard-cursor] ::cursor)

(defcfn destroy-cursor
  "Destroy a cursor object and release its related resources.

  All remaining cursors are also destroyed on [[terminate]].

  See [[create-cursor]]."
  "glfwDestroyCursor" [::cursor] ::mem/void)

(defcfn set-cursor
  "Sets the cursor image for the given `window` to the passed `cursor` object.

  The cursor image will only be visible when the cursor mode is
  `:glfw-clj.core/cursor-normal`.

  See [[create-cursor]], [[create-standard-cursor]]."
  "glfwSetCursor" [::window ::cursor] ::mem/void)

(def ^:private mod->enum
  {::mod-shift 0x0001
   ::mod-control 0x0002
   ::mod-alt 0x0004
   ::mod-super 0x0008
   ::mod-caps-lock 0x0010
   ::mod-num-lock 0x0020})
(def mods (set (map-keys mod->enum)))

(defmethod mem/primitive-type ::mods
  [_type]
  ::mem/int)

(defmethod mem/serialize* ::mods
  [obj _type _arena]
  (int (transduce (map mod->enum) (completing bit-or) 0 obj)))

(defmethod mem/deserialize* ::mods
  [obj _type]
  (transduce
   (comp (filter #(not (zero? (bit-and (second %) obj))))
         (map first))
   conj
   #{}
   mod->enum))

(def-wnd-callback :key
  "Sets the `callback` to be called when a key event occurs.

  This should not be used for text input. Instead use [[set-char-callback]].

  The callback is a function of the window, a key name, scancode, key event, and
  active key mods at the time of the event. The return value is ignored.

  When the `window` loses focus, artificial key release events are generated for
  all currently pressed keys. You can distinguish the difference between real
  events and these generated ones by the fact that the focus loss event will be
  processed before these release events.

  Some keys will register with a key name of `:glfw-clj.core/key-unknown`. These
  are keys that GLFW has no name for and are identified by the scancode passed.
  They are platform and keyboard-specific, so should not be used across devices,
  and cannot be queried with [[get-key]].

  In some cases additional events will be generated by GLFW, which may have a
  scancode of 0.

  Returns the old callback, if any was set."
  [::ffi/fn [::window ::key ::mem/int ::key-event ::mods] ::mem/void])

(defmethod mem/primitive-type ::codepoint
  [_type]
  ::mem/int)

(defmethod mem/serialize* ::codepoint
  [obj _type _arena]
  (Character/codePointAt (char-array (if (char? obj) [obj] obj)) 0))

(defmethod mem/deserialize* ::codepoint
  [obj _type]
  (apply str (Character/toChars obj)))

(def-wnd-callback :char
  "Sets the `callback` to be called when a character is input.

  This is used for Unicode character input.

  The callback is a function of the window and a string representing a single
  unicode character. It may or may not be representable in a [[char]]. The
  return value is ignored.

  Returns the old callback if any was set."
  [::ffi/fn [::window ::codepoint] ::mem/void])

(def-wnd-callback :char-mods
  "Sets the `callback` to be called when a character is input, including key mods.

  This is used for Unicode character input.

  The callback is a function of the window and a string representing a single
  unicode character, plus a key mod set. The unicode character may or may not be
  representable in a [[char]]. The return value is ignored.

  Returns the old callback if any was set.

  Deprecated and will be removed in GLFW 4.0."
  [::ffi/fn [::window ::codepoint ::mods] ::mem/void])
#_{:clj-kondo/ignore [:unresolved-symbol]}
(alter-meta! #'set-char-mods-callback assoc :deprecated true)

(def-wnd-callback :mouse-button
  "Sets the `callback` to be called when a mouse button is pressed.

  The callback is a function of the window, the mouse button the event is
  connected with, and the event. The return value is ignored.

  When focus is lost on the `window`, synthetic mouse button release events are
  sent for all currently-pressed buttons. These events will be handled after the
  focus event.

  Returns the old callback if any was set."
  [::ffi/fn [::window ::mouse-button ::key-event] ::mem/void])

(def-wnd-callback :cursor-pos
  "Sets the `callback` to be called when the mouse moves.

  The callback is a function of the window and the new x and y position of the
  cursor. The return value is ignored.

  The position is provided in screen coordinates relative to the upper left of
  the window.

  Returns the old callback if any was set."
  [::ffi/fn [::window ::mem/double ::mem/double] ::mem/void])

(def-wnd-callback :cursor-enter
  "Sets the `callback` to be called when the mouse enters the content area of the `window`.

  The callback is a function of the window and a boolean, which will be true if
  the cursor is now in the window, and false if it is no longer in the window.
  The return value is ignored.

  Returns the old callback if any was set."
  [::ffi/fn [::window ::bool] ::mem/void])

(def-wnd-callback :scroll
  "Sets the `callback` to be called when the scroll wheel is rotated.

  The callback is a function of the window and the x and y offsets that were
  scrolled since the last time events were processed. The return value is
  ignored.

  Returns the old callback if any was set."
  [::ffi/fn [::window ::mem/double ::mem/double] ::mem/void])

(defalias ::drop-fn [::ffi/fn [::window ::mem/int ::mem/pointer] ::mem/void])

(defcfn set-drop-callback
  "Sets the `callback` to be called when a file is dropped into the window.

  The callback is a function of the window and a vector of file paths. The
  return value is ignored.

  Returns the old callback if any was set."
  "glfwSetDropCallback" [::window ::mem/pointer] ::mem/pointer
  glfw-set-drop-callback
  ([window callback]
   (set-drop-callback window callback (mem/global-arena)))
  ([window callback arena]
   (mem/deserialize*
    (glfw-set-drop-callback
     window
     (mem/serialize*
      (fn [window count paths]
        (try (let [paths (mem/deserialize* paths [::mem/pointer [::mem/array ::mem/c-string count]])]
               (callback window paths))
             (catch Throwable e
               (log/error e "Caught an exeption in callback drop")
               nil)))
      ::drop-fn arena))
    ::drop-fn)))

(defcfn joystick-present
  "Returns if a joystick with the given `jid` is present.

  All other functions that take a `jid` first check if it is present, so this
  need not be called before them."
  {:arglists '([jid])}
  "glfwJoystickPresent" [::mem/int] ::bool)

(defcfn get-joystick-axes
  "Gets a vector of all the joystick axis values for the joystick `jid`.

  All the returned values are floats from -1.0 to 1.0."
  "glfwGetJoystickAxes" [::mem/int ::mem/pointer] ::mem/pointer
  glfw-get-joystick-axes
  [jid]
  (with-open [arena (mem/confined-arena)]
    (let [count (mem/alloc-instance ::mem/int arena)
          axes (glfw-get-joystick-axes jid (mem/address-of count))
          count (mem/deserialize-from count ::mem/int)]
      (mem/deserialize* axes [::mem/pointer [::mem/array ::mem/float count]]))))

(defcfn get-joystick-buttons
  "Gets a vector of all the buttons for the joystick `jid`.

  All the returned values are `:glfw-clj.core/press` or
  `:glfw-clj.core/release`.

  By default all hats are also included as sets of four buttons in the order up,
  right, down, left, and they are returned in the same order
  as [[get-joystick-hats]].

  These can be elided from this list if the [[init-hint]]
  `:glfw-clj.core/joystick-hat-buttons` is set to false."
  "glfwGetJoystickButtons" [::mem/int ::mem/pointer] ::mem/pointer
  glfw-get-joystick-buttons
  [jid]
  (with-open [arena (mem/confined-arena)]
    (let [count (mem/alloc-instance ::mem/int arena)
          buttons (glfw-get-joystick-buttons jid (mem/address-of count))
          count (mem/deserialize-from count ::mem/int)]
      (mapv
       #_{:clj-kondo/ignore [:unresolved-symbol]}
       enum->key-event
       (mem/deserialize* buttons [::mem/pointer [::mem/array ::mem/byte count]])))))

(def ^:private hat->enum
  {::hat-up 1
   ::hat-right 2
   ::hat-down 4
   ::hat-left 8})
(def hats (set (map-keys hat->enum)))

(defmethod mem/primitive-type ::hat
  [_type]
  ::mem/int)

(defmethod mem/serialize* ::hat
  [obj _type _arena]
  (transduce (map hat->enum) (completing bit-or) 0 obj))

(defmethod mem/deserialize* ::hat
  [obj _type]
  (transduce
   (keep #(when-not (zero? (bit-and (second %) obj))
            (first %)))
   conj #{}
   hat->enum))

(defcfn get-joystick-hats
  "Gets a vector of the state of all hats on the joystick `jid`.

  The hats are sets of keywords representing the directions pressed.

  See [[hats]]."
  "glfwGetJoystickHats" [::mem/int ::mem/pointer] ::mem/pointer
  glfw-get-joystick-hats
  [jid]
  (with-open [arena (mem/confined-arena)]
    (let [count (mem/alloc-instance ::mem/int arena)
          hats (glfw-get-joystick-hats jid (mem/address-of count))
          count (mem/deserialize-from count ::mem/int)]
      (mapv
       #(mem/deserialize* % ::hat)
       (mem/deserialize* hats [::mem/pointer [::mem/array ::mem/byte count]])))))

(defcfn get-joystick-name
  "Get the name of the joystick `jid`.

  If the joystick is not present, returns nil."
  {:arglists '([jid])}
  "glfwGetJoystickName" [::mem/int] ::mem/c-string)

(defcfn get-joystick-guid
  "Get the SDL-compatible GUID as a hex string of the joystick `jid`.

  This GUID is what's used to identify a joystick as a gamepad. Each GUID
  identifies a model of joystick, but not is not unique to a particular
  connected joystick (e.g. all wired Xbox 360 controllers will have the same
  GUID). These GUIDs may also be different between platforms."
  {:arglists '([jid])}
  "glfwGetJoystickGUID" [::mem/int] ::mem/c-string)

(defcfn set-joystick-user-pointer
  "Set a user pointer attached to the joystick `jid`.

  This is used to enable not depending on global state inside the joystick
  callback.

  See [[get-joystick-user-pointer]]."
  {:arglists '([jid pointer])}
  "glfwGetJoystickUserPointer" [::mem/int ::mem/pointer] ::mem/void)

(defcfn get-joystick-user-pointer
  "Gets the user appointer attached to the joystick `jid`.

  This is used to fetch data attached to the joystick from inside the joystick
  callback without relying on global state.

  See [[set-joystick-user-pointer]]."
  {:arglists '([jid])}
  "glfwSetJoystickUserPointer" [::mem/int] ::mem/pointer)

(defcfn joystick-is-gamepad
  "Returns if the joystick `jid` is present and has a gamepad mapping."
  {:arglists '([jid])}
  "glfwJoystickIsGamepad" [::mem/int] ::bool)

(defcallback :joystick
  "Sets the `callback` to be called when a joystick connection event occurs.

  The callback is a function of the jid of the joystick and a keyword
  representing a connection event. The return value is ignored.

  For joystick connection events to be processed, one of the event processing
  functions like [[poll-events]] must be called.

  Returns the old callback if any was set.

  See [[connection-events]]."
  [::ffi/fn [::mem/int ::connection-event] ::mem/void])

(defcfn update-gamepad-mappings
  "Parse the passed mappings string and update internal list.

  The mappings string can contain either a single gamepad mapping, or many
  mappings separated by newlines. The parser supports the full format of
  `gamecontrollerdb.txt` source file including empty lines and comments.

  See the [Gamepad Mappings Guide](https://www.glfw.org/docs/latest/input_guide.html#gamepad_mapping)
  for a full description of the format.

  If there is already a mapping for a passed GUID, it will be overriden with the
  new mapping. If [[terminate]] is called, followed by a second [[init]], the
  internal mappings will be reset to their defaults."
  {:arglists '([mappings])}
  "glfwUpdateGamepadMappings" [::mem/c-string] ::bool)

(defcfn get-gamepad-name
  "Returns the human-readable name of the gamepad from the gamepad mapping assigned to the joystick `jid`.

  If the joystick does not exist or does not have a gamepad mapping, returns
  nil."
  {:arglists '([jid])}
  "glfwGetGamepadName" [::mem/int] ::mem/c-string)

(def ^:private gamepad-state-type
  [::mem/struct
   [[:buttons [::mem/array ::mem/byte 15]]
    [:axes [::mem/array ::mem/float 6]]]])

(defmethod mem/c-layout ::gamepad-state
  [_type]
  (mem/c-layout gamepad-state-type))

(defmethod mem/deserialize-from ::gamepad-state
  [segment _type]
  (let [struct (mem/deserialize-from
                segment
                gamepad-state-type)
        buttons (mapv #(mem/deserialize* % ::key-event) (:buttons struct))
        buttons (into #{}
                      (map #(when (#{::press} %2)
                              %1)
                           buttons
                           [:a :b :x :y :left-bumper :right-bumper
                            :back :start :guide :left-thumb :right-thumb
                            :dpad-up :dpad-right :dpad-down :dpad-left]))
        axes (:axes struct)
        left-stick (subvec axes 0 2)
        right-stick (subvec axes 2 4)
        left-trigger (nth axes 4)
        right-trigger (nth axes 5)]
    {:buttons buttons
     :axes {:left-stick left-stick
            :right-stick right-stick
            :left-trigger left-trigger
            :right-trigger right-trigger}}))

(defcfn get-gamepad-state
  "Gets the state of the joystick `jid` remapped to an Xbox-like gamepad.

  If the joystick does not exist, returns nil.

  Returns a map with the keys `:buttons` and `:axes`.

  `:buttons` is a set of currently pressed button names. The buttons are `:a`,
  `:b` `:x`, `:y`, `:left-bumper`, `:right-bumper`, `:back`, `:start`, `:guide`,
  `:left-thumb`, `:right-thumb`, `:dpad-up`, `:dpad-right`, `:dpad-down`, and
  `:dpad-left`.

  The `:guide` button may not be available as it is often hooked by the system
  or by Steam.

  `:axes` is a map from `:left-stick` and `:right-stick` to vectors of the x and
  y values of the axis, as doubles from -1.0 to 1.0, and from `:left-trigger`
  and `:right-trigger` to a number from 0 to 1.0.

  Unavailable buttons and axes will report as not pressed and 0.0 respectively."
  "glfwGetGamepadState" [::mem/int ::mem/pointer] ::bool
  glfw-get-gamepad-state
  [jid]
  (with-open [arena (mem/confined-arena)]
    (let [state (mem/alloc-instance ::gamepad-state arena)
          gamepad-exists? (glfw-get-gamepad-state jid (mem/address-of state))]
      (when gamepad-exists?
        (mem/deserialize-from state ::gamepad-state)))))

(defcfn set-clipboard-string
  "Sets the system clipboard to the specified, UTF-8 encoded string."
  {:arglists '([window s])}
  "glfwSetClipboardString" [::window ::mem/c-string] ::mem/void)

(defcfn get-clipboard-string
  "Gets the content of the system clipboard as a UTF-8 encoded string.

  If the clipboard is empty or is not encoded as a valid UTF-8 string then nil
  is returned and a `:glfw-clj.core/format-unavailable` error is generated."
  "glfwGetClipboardString" [::window] ::mem/c-string)

(defcfn get-time
  "Gets the current time in seconds.

  The returned time counts from the call to [[init]] by default, but can be
  altered with [[set-time]].

  This uses the highest-resolution monotonic time available on the current
  platform.

  This function may be called from any thread."
  "glfwGetTime" [] ::mem/double)

(defcfn set-time
  "Set the current time to be counted from.

  The number must be a positive, finite number less than 18446744073.0, which is
  approximately 584.5 years.

  This limit is based on the largest number of nanoseconds that can be stored in
  a 64-bit integer, and may be increased in the future.

  This function may be called from any thread, but is not atomic or synchronized
  in any way with calls to [[get-time]], and so must be externally
  synchronized."
  {:arglists '([time])}
  "glfwSetTime" [::mem/double] ::mem/void)

(defcfn get-timer-value
  "Gets the current raw timer value.

  The raw timer value is an integral value which does not have a consistent
  mapping to a particular length of time. To convert this to a consistent time,
  divide it by the timer frequency.

  This function may be called from any thread.

  See [[get-timer-frequency]]."
  "glfwGetTimerValue" [] ::mem/long)

(defcfn get-timer-frequency
  "Gets the frequency in Hz of the raw system timer.

  Returns 0 if there was an error.

  This function may be called from any thread.

  See [[get-timer-value]]."
  "glfwGetTimerFrequency" [] ::mem/long)
