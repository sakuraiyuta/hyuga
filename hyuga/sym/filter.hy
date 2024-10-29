(require hyrule * :readers *)
(require hyrule.argmove [-> ->>])

(import toolz.itertoolz *)

(import hyuga.sym.helper *)

(import builtins)
(import keyword)

(setv hy-specials (set (+ (list (.keys builtins._hy_macros)) keyword.kwlist)))

(defn not-exclude-sym?
  [sym-hy/val]
  (and (not (.startswith (first sym-hy/val) "_hy-"))
       (!= (first sym-hy/val) "-hyuga-eval-form")))

(defn not-in-$SYM?
  [mod scope uri sym-hy/val]
  (try
    (let+ [[sym-hy _] sym-hy/val
           tgt-full-sym (get-full-sym scope mod sym-hy)]
      (->> ($GLOBAL.get-$SYMS) .items
           (filter #%(and (= (first %1) tgt-full-sym)
                          (= (-> %1 second (get "scope")) scope)
                          (= (-> %1 second (get "uri")) uri)))
           count (= 0)))
    (except [e Exception]
      (log-warn "not-in-$SYM?" e)
      (logger.warning f"mod={mod}, uri={uri}, sym-hy/val={sym-hy/val}"))))

(defn filter-add-targets
  [mod scope uri update? sym-py/vals]
  "TODO: doc"
  (let [hy? #%(= (-> %1 first get-sym) "hy")
        pyattr? #%(and (.startswith (-> %1 first get-sym) "__")
                       (.endswith (-> %1 first get-sym) "__"))]
    (or (->> sym-py/vals
             (map sym-py/val->sym-hy/val)
             (filter #%(and (not (or (hy? %1)
                                     (pyattr? %1)))
                            (or update?
                                (not-in-$SYM? mod scope uri %1))
                            (not-exclude-sym? %1))))
        #())))

(defn filter-not-reserved
  [items]
  (->> items
       (filter #%(and (not (in (first %1)
                               (__builtins__.keys)))
                      (not (in (-> %1 first sym-py->hy)
                               (tuple hy-specials)))))))

