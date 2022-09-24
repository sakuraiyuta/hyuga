(require hyrule * :readers *)

(import toolz.itertoolz *)

(import hyuga.sym.helper *)

(defn not-exclude-sym?
  [sym-hy/val]
  (and (not (.startswith (first sym-hy/val) "_hy-"))
       (!= (first sym-hy/val) "-hyuga-eval-form")))

(defn not-in-$SYM?
  [mod uri sym-hy/val]
  (try
    (let+ [[sym-hy _] sym-hy/val
           tgt-full-sym (get-full-sym mod sym-hy)]
      (->> ($GLOBAL.get-$SYMS) .items
           (filter #%(and (= (first %1) tgt-full-sym)
                          (= (-> %1 second (get "uri")) uri)))
           count (= 0)))
    (except [e Exception]
      (log-warn "not-in-$SYM?" e)
      (logger.warning f"mod={mod}, uri={uri}, sym-hy/val={sym-hy/val}"))))

(defn filter-add-targets
  [mod uri update? sym-py/vals]
  "TODO: doc"
  (let [hy? #%(= (-> %1 first get-sym) "hy")
        pyattr? #%(and (.startswith (-> %1 first get-sym) "__")
                       (.endswith (-> %1 first get-sym) "__"))]
    (or (->> sym-py/vals
             (map sym-py/val->sym-hy/val)
             (filter #%(not (or (hy? %1)
                                (pyattr? %1))))
             (filter #%(or update?
                           (not-in-$SYM? mod uri %1)))
             (filter not-exclude-sym?))
        #())))

