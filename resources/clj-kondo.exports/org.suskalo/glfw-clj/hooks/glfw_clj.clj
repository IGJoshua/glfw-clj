(ns hooks.glfw-clj
  (:require
   [clj-kondo.hooks-api :as api]))

(def callback-node (api/token-node 'callback))
(def scope-node (api/token-node 'scope))
(def window-node (api/token-node 'window))

(defn defcallback
  [{:keys [node]}]
  (let [[name-kw-node docstring _fn-vector] (rest (:children node))
        fn-name (symbol (str "set-" (name (api/sexpr name-kw-node)) "-callback"))
        new-node (api/list-node
                  (list
                   (api/token-node 'defn)
                   (api/token-node fn-name)
                   docstring
                   (api/list-node
                    (list
                     (api/vector-node
                      [callback-node
                       scope-node])
                     callback-node
                     scope-node
                     (api/token-node nil)))
                   (api/list-node
                    (list
                     (api/vector-node
                      [callback-node])
                     callback-node
                     (api/token-node nil)))))]
    {:node new-node}))

(defn def-wnd-callback
  [{:keys [node]}]
  (let [[name-kw-node docstring _fn-vector] (rest (:children node))
        fn-name (symbol (str "set-" (name (api/sexpr name-kw-node)) "-callback"))
        new-node (api/list-node
                  (list
                   (api/token-node 'defn)
                   (api/token-node fn-name)
                   docstring
                   (api/list-node
                    (list
                     (api/vector-node
                      [window-node
                       callback-node
                       scope-node])
                     window-node
                     callback-node
                     scope-node
                     (api/token-node nil)))
                   (api/list-node
                    (list
                     (api/vector-node
                      [window-node
                       callback-node])
                     window-node
                     callback-node
                     (api/token-node nil)))))]
    {:node new-node}))

(defn defenum
  [{:keys [node]}]
  (let [[enum-name enum-map] (rest (:children node))
        plural (if (= \s (last (name (api/sexpr enum-name))))
                 "es"
                 "s")
        set-name (symbol (str (name (api/sexpr enum-name)) plural))
        new-node (api/list-node
                  (list
                   (api/token-node 'def)
                   (api/token-node set-name)
                   enum-map))]
    {:node new-node}))
