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
