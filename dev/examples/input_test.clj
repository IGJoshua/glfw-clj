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
  [_window key scancode action mode]
  (log/debug (if-not (#{::glfw/key-unknown} key)
               key
               (str "#scancode " scancode))
             action mode))

(defn run-loop
  []
  (glfw/poll-events))

(defn run
  []
  (reset! window (glfw/create-window 800 600 "Test Window"))
  (glfw/set-key-callback @window print-key)
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
