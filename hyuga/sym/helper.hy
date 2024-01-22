(require hyrule * :readers *)
(import hyrule.iterables [drop-last distinct])

(import urllib.parse [urlparse])
(import pkgutil)
(import pathlib [Path])
(import inspect [getmodulename])
(import toolz.itertoolz *)
(import builtins)

(import hyuga.log *)
(import hyuga.global [$GLOBAL])
(import .eval [eval-in!])
(import hyuga.sym.module *)

(defn uri->mod
  [root-uri doc-uri]
  (let [root-path (-> root-uri urlparse (get 2))
        doc-path (-> doc-uri urlparse (get 2))
        doc-path-obj (Path doc-path)
        spec/parents (get-specs-recur root-path [] [])
        filtered (->> spec/parents
                      (filter #%(let [spec (first %1)]
                                  (= doc-path spec.origin)))
                      list)
        py-name (if (> (len filtered) 0)
                  (as-> filtered it
                    (first it)
                    (+ (-> "." (.join (second it)))
                       "."
                       (let [spec (first it)]
                         spec.name))
                    (.strip it "."))
                  (let [fname doc-path-obj.stem]
                    f"{fname}"))]
    (sym-py->hy py-name)))

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
