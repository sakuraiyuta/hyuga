(require hyrule * :readers *)
(import hyrule.iterables [drop-last distinct])

(import os.path [dirname])
(import inspect [getmodulename])
(import toolz.itertoolz *)
(import builtins)

(import hyuga.log *)
(import hyuga.global [$GLOBAL])

(defn uri->mod
  [root-uri doc-uri]
  ;; TODO: replace to package loader
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
  f"{scope}\\{ns}\\{sym}")

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
  [sym [ns ""]]
  "TODO: doc"
  (if (.startswith sym ".")
    ns
    (let [splitted (.split sym ".")]
      (if (-> splitted count (> 1))
        (->> splitted (drop-last 1) (.join "."))
        ""))))

(defn get-hy-macros
  []
  (builtins._hy_macros.items))
