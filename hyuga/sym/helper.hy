(require hyrule * :readers *)
(require hyrule.argmove [-> ->>])
(import hyrule.iterables [drop-last])

(import os.path [dirname])
(import inspect [getmodulename])
(import toolz.itertoolz *)

(import hyuga.log *)
(import hyuga.global [$GLOBAL])

(defn uri->mod
  [root-uri doc-uri]
  (let [submod (getmodulename doc-uri)]
    (-> doc-uri
        (.replace root-uri "")
        (.lstrip "/")
        dirname
        (.replace "/" ".")
        (+ f".{submod}")
        sym-py->hy)))

(defn fix-hy-symbol
  [form]
  (-> form hy.repr (.lstrip "'")))

(defn get-ns
  [symkey]
  (-> symkey get-scope/ns/sym second))

(defn get-sym
  [symkey]
  (-> symkey get-scope/ns/sym last))

(defn get-scope
  [symkey]
  (-> symkey get-scope/ns/sym first))

(defn get-scope/ns/sym
  [symkey]
  (let [splitted (.split symkey "\\")]
    (branch (= (count splitted) it)
            3 splitted
            1 ["" "" symkey])))

(defn get-full-sym
  [scope ns sym]
  (+ scope "\\" (if (isinstance ns str) ns (fix-hy-symbol ns)) "\\" sym))

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
