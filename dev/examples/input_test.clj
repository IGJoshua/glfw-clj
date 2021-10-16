(ns examples.input-test
  (:require
   [clojure.tools.logging :as log]
   [glfw-clj.core :as glfw]))

(def window (atom nil))

(defn print-error
  [error description]
  (log/error (ex-info description
                      {:type error})))

(defn print-key
  [_window key scancode action mods]
  (log/debug (if-not (#{::glfw/key-unknown} key)
               (name key)
               (str "#scancode " scancode))
             "was"
             (str (name action) "ed") "with mods" mods))

(defn connected-joystick
  [jid]
  (log/info "Joystick" jid "connected")
  (log/debug "Joystick name:" (glfw/get-joystick-name jid))
  (let [gamepad? (glfw/joystick-is-gamepad jid)]
    (log/debug "Is gamepad?" gamepad?)
    (when gamepad?
      (log/debug "Gamepad name:" (glfw/get-gamepad-name jid)))))

(defn run-loop
  []
  (glfw/wait-events-timeout 0.01)
  (glfw/swap-buffers @window))

(defn run
  []
  (reset! window (glfw/create-window 800 600 "Test Window"))
  (glfw/set-key-callback @window print-key)
  (glfw/set-cursor-pos-callback
   @window
   #(log/debug "Mouse moved to" %2 %3))
  (glfw/set-mouse-button-callback
   @window
   #(log/debug "Mouse button" %2 "was" (str (name %3) "ed")))
  (glfw/set-joystick-callback #(when (#{::glfw/connected} %2)
                                 (connected-joystick %1)))
  (glfw/make-context-current @window)
  (glfw/swap-interval 1)
  (loop []
    (run-loop)
    (when (not (glfw/window-should-close @window))
      (recur)))
  (swap! window glfw/destroy-window))

(defn pre-init
  []
  (glfw/set-error-callback print-error))

(defn -main
  []
  (pre-init)
  (glfw/init)
  (run)
  (glfw/terminate))
