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
  ;; TODO: try to get info directly if sym not found
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
  (logger.debug f"get-candidates: prefix={prefix}, $SYMS.count={(count ($GLOBAL.get-$SYMS))}")
  (let [splitted (.split prefix ".")
        module-or-class (module-or-class? splitted)
        sym-prefix (if module-or-class
                     (last splitted)
                     prefix)]
    (logger.debug f"module-or-class={module-or-class}, sym-prefix={sym-prefix}")
    (when module-or-class
      (->> (get-module-attrs module-or-class)
           (map #%(+ [] [(as-> splitted it
                           (drop-last 1 it)
                           (list it)
                           (+ it [(first %1)])
                           (.join "." it))
                         (second %1)]))
           (filter-add-targets module-or-class)
           (map #%(add-sym! %1 "module"))
           tuple))
    ;;FIXME
    (->> ($GLOBAL.get-$SYMS) .items
         (filter #%(.startswith (get-ns (first %1)) module-or-class))
         (filter #%(.startswith (get-sym (first %1)) sym-prefix))
         ;; exclude duplicated module name(e.g. `sys.sys`)
         ; (filter #%(not (and module-or-class
         ;                     (-> %1 get-ns (= module-or-class)))))
         (map second)
         tuple)))

(defn get-exact-matches
  [sym-tgt]
  (logger.debug f"get-exact-matches sym-tgt={sym-tgt}, $SYMS.count={(count ($GLOBAL.get-$SYMS))}")
  (->> ($GLOBAL.get-$SYMS) .items
       (filter #%(.endswith (first %1) sym-tgt))
       ;; TODO: jump by module name(e.g. sym-tgt=hyrule.collections)
       (filter #%(let [[scope full-sym] (get-scope/ns (first %1))
                       [ns sym] (get-ns/sym full-sym)]
                   (or (= scope sym-tgt)
                       (= sym sym-tgt))))
       tuple))
