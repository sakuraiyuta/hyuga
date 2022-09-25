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
  (logger.debug f"parse-src!: start. $SYMS.count={(count ($GLOBAL.get-$SYMS))}")
  (for [loader-fn [load-builtin!
                   load-hy-special!
                   (partial load-src! src root-uri doc-uri "hyuga.sym.dummy")
                   load-sys!]]
    (loader-fn)
    (logger.debug f"parse-src!: done. $SYMS.count={(count ($GLOBAL.get-$SYMS))}")))

(defn get-details
  [sym-hy [root-uri "file:///dummy"]]
  "TODO: doc"
  (logger.debug f"get-details: sym-hy={sym-hy}")
  ;; TODO: try to get info directly if sym not found
  (let [matches (->> ($GLOBAL.get-$SYMS) .values
                     (filter #%(= (-> (:sym %1) get-sym) sym-hy))
                     (sorted :key #%(not (.startswith (:uri %1) root-uri)))
                     (sorted :key #%(not (-> (:sym %1) get-scope (.startswith "hyuga.sym.dummy"))))
                     tuple)]
    (logger.debug f"get-details: matches={matches}")
    (if (> (count matches) 0)
      (get ($GLOBAL.get-$SYMS) (:sym (first matches)))
      None)))

(defn get-candidates
  [prefix]
  r"
  TODO: update doc
   Get all candidates supposed by prefix from all scopes.
   (globals, locals, builtins, and macros)

   Example:
   ```hy
   (get-candidates \"de\")
   ; => #({\"scope\" \"builtin\"
   ; \"type\" <class 'builtin_function_or_method'>
   ; \"sym\" \"delattr\"})
   ```
   "
  (logger.debug f"get-candidates: prefix={prefix}, $SYMS.count={(count ($GLOBAL.get-$SYMS))}")
  (let [splitted (.split prefix ".")
        module-or-class (module-or-class? splitted)
        sym-prefix (if module-or-class
                     (last splitted)
                     prefix)]
    (logger.debug f"module-or-class={module-or-class}, sym-prefix={sym-prefix}")
    (->> ($GLOBAL.get-$SYMS) .items
         (filter #%(let [key (first %1)]
                     (and (.startswith (get-ns key) module-or-class)
                          (.startswith (get-sym key) sym-prefix))))
         ;; exclude duplicated module name(e.g. `sys.sys`)
         (filter #%(not (and module-or-class
                             (-> %1 first get-sym (= module-or-class)))))
         tuple)))

(defn get-matches
  [tgt-full-sym]
  (logger.debug f"get-matches tgt-sym={tgt-full-sym}")
  (let [[tgt-ns tgt-sym] (get-ns/sym tgt-full-sym)]
    (->> ($GLOBAL.get-$SYMS) .items
         ;; TODO: jump by module name(e.g. sym-tgt=hyrule.collections)
         (filter #%(let [[loaded-scope loaded-full-sym]
                         (get-scope/ns (first %1))
                         [loaded-ns loaded-sym]
                         (get-ns/sym loaded-full-sym)]
                     (or (= loaded-scope tgt-full-sym)
                         (and (= loaded-ns tgt-ns)
                              (= loaded-sym tgt-sym))
                         (= loaded-sym tgt-full-sym))))
         tuple)))
