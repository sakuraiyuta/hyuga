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
  (logger.debug f"parse-src!: start. $SYMS.count={(count ($GLOBAL.get-$SYMS))}, root-uri={root-uri}, doc-uri={doc-uri}")
  (for [loader-fn [load-builtin!
                   load-hy-special!
                   load-sys!
                   (partial load-venv! root-uri doc-uri)
                   (partial load-src! src root-uri doc-uri)]]
    (loader-fn))
  (logger.debug f"parse-src!: finished. $SYMS.count={(count ($GLOBAL.get-$SYMS))}, root-uri={root-uri}, doc-uri={doc-uri}"))

(defn get-details
  [full-sym root-uri doc-uri]
  "TODO: doc"
  (logger.debug f"get-details: full-sym={full-sym} doc-uri={doc-uri}")
  ;; TODO: try to get info directly if sym not found
  (let [[tgt-scope tgt-ns tgt-sym] (get-scope/ns/sym full-sym)
        current-mod (uri->mod root-uri doc-uri)
        eq-sym-or-ns?
        #%(let [[load-scope load-ns load-sym]
                (get-scope/ns/sym (:sym %1))]
            (= tgt-sym load-sym))
        matches (->> ($GLOBAL.get-$SYMS) .values list
                     (filter eq-sym-or-ns?)
                     (sorted :key #%(if (= (:uri %1) doc-uri)
                                      1 2))
                     ; (sorted :key #%(if (-> (:sym %1)
                     ;                        get-scope
                     ;                        (= current-mod))
                     ;                  1 2))
                     tuple)]
    (if (> (count matches) 0)
      (get ($GLOBAL.get-$SYMS) (:sym (first matches)))
      None)))

(defn get-candidates
  [prefix root-uri doc-uri]
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
  (logger.debug f"get-candidates: prefix={prefix}")
  (let [editting-mod (uri->mod root-uri doc-uri)
        splitted (.split prefix ".")
        module-or-class (module-or-class? splitted)
        sym-prefix (if module-or-class
                     (last splitted)
                     prefix)
        filter-fn
        #%(let [key (first %1)]
            (and (in (get-scope key) [editting-mod
                                      "(builtin)"
                                      "(hykwd)"
                                      "(sysenv)"
                                      "(venv)"
                                      module-or-class])
                 (or (.startswith (get-ns key) module-or-class)
                     (.startswith (get-scope key) module-or-class))
                 (.startswith (get-sym key) sym-prefix)))]
    (logger.debug f"editting-mod={editting-mod}, module-or-class={module-or-class}, sym-prefix={sym-prefix}")
    (->> ($GLOBAL.get-$SYMS) .items
         (filter filter-fn)
         tuple)))

(defn get-matches
  [tgt-full-sym root-uri doc-uri]
  "TODO: doc"
  ;; TODO: bugfix
  (logger.debug f"get-matches tgt-sym={tgt-full-sym}")
  (let [tgt-scope (uri->mod root-uri doc-uri)
        [tgt-ns tgt-sym] (get-ns/sym tgt-full-sym)
        filter-fn
        #%(let [[loaded-scope loaded-full-ns]
                (get-scope/ns (first %1))
                [loaded-ns loaded-sym]
                (get-ns/sym loaded-full-ns)]
            (or (= loaded-scope tgt-full-sym)
                (and (= tgt-scope loaded-scope)
                     (= loaded-sym tgt-sym))
                (= loaded-sym tgt-sym)))]
    (->> ($GLOBAL.get-$SYMS) .items
         ;; TODO: jump by module name(e.g. sym-tgt=hyrule.collections)
         (filter filter-fn)
         tuple)))
