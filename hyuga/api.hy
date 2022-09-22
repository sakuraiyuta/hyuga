(require hyrule * :readers *)
(import hyrule.iterables [butlast drop-last])

(import toolz.itertoolz *)
(import functools [partial])

(import hyuga.log [logger])
(import hyuga.global [$GLOBAL])
(import hyuga.sym.loader *)
(import hyuga.sym.helper *)

(defn parse-src!
  [src root-uri doc-uri]
  "TODO: doc"
  (logger.debug f"parse-src! $SYMS.count={(count ($GLOBAL.get-$SYMS))}")
  (for [loader-fn [load-builtin!
                   load-hy-special!
                   load-sys!
                   (partial load-src! src root-uri doc-uri)]]
    (loader-fn)))

(defn get-details
  [sym-hy]
  "TODO: doc"
  (logger.debug f"get-details sym-hy={sym-hy}" )
  ;; TODO: try get info directly if sym not found
  (-> ($GLOBAL.get-$SYMS) (get sym-hy)))

(defn get-candidates
  [prefix]
  "Get all candidates supposed by prefix from all scopes.
  (globals, locals, builtins, and macros)

  Example:
  ```hy
  (get-candidates \"de\")
  => #({\"scope\" \"builtin\"
  \"type\" <class 'builtin_function_or_method'>
  \"sym\" \"delattr\"})
  ```
  "
  (logger.debug "get-candidates: $SYMS.count={(count ($GLOBAL.get-$SYMS))}")
  (let [splitted (.split prefix ".")
        module-or-class (module-or-class? splitted)
        sym-prefix (if module-or-class
                     (last splitted)
                     prefix)]
    (logger.debug (.format "module-or-class={}" module-or-class))
    (when module-or-class
      (->> (get-module-attrs splitted)
           (map #%(+ [] [(as-> splitted it
                           (drop-last 1 it)
                           (list it)
                           (+ it [(first %1)])
                           (.join "." it))
                         (second %1)]))
           filter-add-targets
           (map #%(add-sym! %1 "module"))
           tuple))
    (->> ($GLOBAL.get-$SYMS) .items
         (filter #%(.startswith (first %1) module-or-class))
         (filter #%(.startswith (-> %1 first (.split ".") last) sym-prefix))
         ;; exclude duplicated module name(e.g. `sys.sys`)
         (filter #%(not (and module-or-class
                             (-> %1 first (= module-or-class)))))
         (map second)
         tuple)))
