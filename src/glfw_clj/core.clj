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
