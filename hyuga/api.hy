(require hyrule * :readers *)
(import hyrule.iterables [butlast drop-last])

(import toolz.itertoolz *)
(import functools [partial])

(import hyuga.log [logger])
(import hyuga.global [$GLOBAL])
(import hyuga.sym.loader *)
(import hyuga.sym.helper *)

(defn in-scope?
  [load-ns search-names]
  (in load-ns (+ ["(builtin)" "(hykwd)" "(sysenv)"] search-names)))

(defn parse-src!
  [src root-uri doc-uri]
  "TODO: doc"
  (logger.debug f"parse-src!: start. $SYMS.count={(count ($GLOBAL.get-$SYMS))}, root-uri={root-uri}, doc-uri={doc-uri}")
  (when (= 0 (count ($GLOBAL.get-$SYMS)))
    (load-builtin!)
    (load-hy-kwd!)
    (load-sys!)
    (load-venv! root-uri))
  (load-src! src root-uri doc-uri (uri->mod root-uri doc-uri) True)
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
            (or (= tgt-sym load-sym)))
        matches (->> ($GLOBAL.get-$SYMS) .values list
                     (filter eq-sym-or-ns?)
                     ;; TODO: ???
                     (sorted :key #%(if (= (:uri %1) doc-uri)
                                      1 2))
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
          inputting-ns (module-or-class? prefix)
          splitted (.split prefix ".")
          tgt-sym-prefix (if inputting-ns
                       (last splitted)
                       prefix)
;          _ (when (and inputting-ns
;                       (not tgt-sym-prefix))
;              (load-pymodule-syms! {"name" inputting-ns}
;                                   doc-uri False editting-mod))
          filter-fn
          #%(let [load-full-sym (first %1)
                  load-sym (get-sym load-full-sym)
                  load-scope (-> %1 second (get "scope"))
                  load-ns (-> %1 second (get "ns"))]
              (and (or (and inputting-ns
                            (.startswith load-ns f"{inputting-ns}.{tgt-sym-prefix}"))
                       (and (not inputting-ns)
                            (in-scope? load-scope [editting-mod inputting-ns])))
                   (or (.startswith load-sym prefix)
                       (.startswith load-sym tgt-sym-prefix))))]
      (logger.debug f"editting-mod={editting-mod}, inputting-ns={inputting-ns}, tgt-sym-prefix={tgt-sym-prefix}")
      (->> ($GLOBAL.get-$SYMS) .items
           (filter filter-fn)
           tuple)))

(defn get-matches
  [tgt-full-sym root-uri doc-uri]
  "TODO: doc"
  ;; TODO: bugfix
  (logger.debug f"get-matches tgt-sym={tgt-full-sym}")
  (let [tgt-ns (uri->mod root-uri doc-uri)
        filter-fn
        #%(let [[load-scope load-ns load-sym]
                (get-scope/ns/sym (first %1))]
            (or (and (in-scope? load-ns [tgt-ns])
                     (= load-sym tgt-full-sym))
                (and (= tgt-ns load-ns)
                     (= load-sym tgt-full-sym))))]
    (->> ($GLOBAL.get-$SYMS) .items
         ;; TODO: jump by module name(e.g. sym-tgt=hyrule.collections)
         (filter filter-fn)
         tuple)))
