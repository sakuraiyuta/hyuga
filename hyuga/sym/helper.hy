(require hyrule * :readers *)
(import hyrule.iterables [drop-last])

(import toolz.itertoolz *)

(import hyuga.log *)
(import hyuga.global [$GLOBAL])

(defn fix-hy-symbol
  [form]
  (-> form hy.repr (.lstrip "'")))

(defn get-ns
  [symkey]
  (-> symkey get-scope/ns second get-ns/sym first))

(defn get-sym
  [symkey]
  (-> symkey get-scope/ns second get-ns/sym second))

(defn get-scope
  [symkey]
  (-> symkey get-scope/ns first))

(defn get-ns/sym
  [val]
  (let [splitted (.split val ".")]
    [(->> splitted
       (drop-last 1)
       tuple
       (.join "."))
     (last splitted)]))

(defn get-scope/ns/sym
  [full-sym]
  (let [[scope full-ns] (get-scope/ns full-sym)
        [ns sym] (get-ns/sym full-ns)]
    [scope ns sym]))

;; TODO: change name to get-scope/full-ns
(defn get-scope/ns
  [symkey]
  (let [splitted (.split symkey "\\")]
    (if (> (count splitted) 1)
      splitted
      ["" (first splitted)])))

(defn get-full-sym
  [prefix sym]
  (if prefix
    (+ prefix "\\" sym)
    (+ "(unknown)\\" sym)))

(defn sym-py/val->sym-hy/val
  [sym-py/val]
  [(->> sym-py/val first sym-py->hy)
   (second sym-py/val)])

(defn sym-py->hy
  [sym-py]
  (->> (.split sym-py ".")
       (map #%(if %1 (hy.unmangle %1) %1))
       (.join ".")))

(defn sym-hy->py
  [sym-hy]
  (->> (.split sym-hy ".")
       (map #%(if %1 (hy.mangle %1) %1))
       (.join ".")))

(defn module-or-class?
  [sym-splitted]
  "TODO: doc"
  (if (-> sym-splitted count (> 1))
    (->> sym-splitted (drop-last 1) (.join "."))
    ""))

(defn get-module-in-syms
  [sym-hy]
  "TODO: doc"
  (logger.debug f"get-module-in-syms: sym-hy={sym-hy}")
  (->> ($GLOBAL.get-$SYMS)
       .items
       (filter #%(= sym-hy (-> %1 first get-sym)))
       first second :type))

(defn get-module-attrs
  [module-name]
  "TODO: doc"
  (try
    (let [module (get-module-in-syms module-name)]
      (logger.debug f"get-module-attrs: module={module}")
      (module.__dict__.items))
    (except [e BaseException]
            (error-trace logger.warning "get-module-attrs" e))))
