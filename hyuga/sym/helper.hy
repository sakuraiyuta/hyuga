(require hyrule * :readers *)
(import hyrule.iterables [butlast drop-last])

(import hy.models [List String])
(import toolz.itertoolz *)

(import hyuga.log *)
(import hyuga.global [$GLOBAL])

(defn not-exclude-sym?
  [sym-hy/val]
  (and (not (.startswith (first sym-hy/val) "_hy-"))
       (!= (first sym-hy/val) "-hyuga-eval-form")))

(defn judge-scope
  [val expect-scope]
  "TODO: doc"
  (branch (in (str val) it)
          "builtin" "builtin"
          else expect-scope))

(defn add-sym!
  [sym-hy/val scope
   [pos #(None None)]]
  "TODO: doc"
  (let [[sym-hy val] sym-hy/val
        docs (create-docs sym-hy val scope)]
    ($GLOBAL.add-$SYMS sym-hy val
                       (judge-scope val scope)
                       docs pos)))

(defn not-in-$SYM?
  [sym-hy/val]
  (not (in (first sym-hy/val) (.keys ($GLOBAL.get-$SYMS)))))

(defn sym-py/val->sym-hy/val
  [sym-py/val]
  [(-> sym-py/val first sym-py->hy)
   (second sym-py/val)])

(defn sym-py->hy
  [sym-py]
  (->> (.split sym-py ".")
       (map hy.unmangle)
       (.join ".")))

(defn sym-hy->py
  [sym-hy]
    (->> (.split sym-hy ".")
         (map hy.mangle)
         (.join ".")))

(defn -get-macro-doc
  [sym-hy symtype]
  "Get macro documents.
  FIXME: So dirty hack!"
  (->> sym-hy (.format
                "(do
                (import io)
                (import contextlib [redirect-stdout])
                (with [buf (io.StringIO)
                _ (redirect-stdout buf)]
                (doc {})
                (buf.getvalue)))")
       hy.read
       (hy.eval :locals {(sym-hy->py sym-hy) symtype})))

(defn create-docs
  [sym-hy symtype scope]
  "TODO: doc"
  (try
    (let [docstr (or symtype.__doc__ "No docs.")]
      f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{docstr}")
    (except
      [e BaseException]
      (logger.debug f"cannot read __doc__. try macro docs. e={e}")
      f"{sym-hy} [{scope}]\n\t{(str symtype)}\n\n{(-get-macro-doc sym-hy symtype)}")))

(defn module-or-class?
  [sym-splitted]
  "TODO: doc"
  (if (-> sym-splitted count (> 1))
    (->> sym-splitted (drop-last 1) (.join "."))
    ""))

(defn get-module-in-syms
  [sym-hy]
  "TODO: doc"
  (as-> ($GLOBAL.get-$SYMS) it
    (.items it)
    (filter #%(= (first %1) sym-hy) it)
    (first it)
    (get (second it) "type")))

(defn get-module-attrs
  [splitted]
  "TODO: doc"
  (try
    (let [module (->> splitted butlast tuple (.join ".")
                      get-module-in-syms)]
      (logger.debug f"get-module-attrs: module={module}")
      (module.__dict__.items))
    (except [e BaseException]
            (error-trace logger.warning "get-module-attrs" e))))

(defn hy-module?
  [module]
  "TODO: src"
  (->> module dir (in "hy")))

(defn get-module-hy-src
  [module]
  "TODO: src"
  (if (->> module dir (in "__file__"))
    (let [fpath (module.__file__)]
      (with [f (open fpath)]
        (hy.read-many f :filename fpath)))
    None))

(defn get-form-pos
  [form]
  (branch (.startswith (first form) it)
          "def" #((getattr form "start_line")
                  (getattr form "start_column"))
          "setv" #((getattr form "start_line")
                   (getattr form "start_column"))
          ;; TODO: implement
          "import" #(None None)
          else #(None None)))

(defn get-defn-summary
  [form]
  (setv ret {"name" None
             "decorator" None
             "args" None})
  (if (-> form second (isinstance List))
    (do
      (.update ret {"decorator" (second form)})
      (.update ret {"name" (nth 2 form)})
      (.update ret {"args" (nth 3 form)}))
    (do
      (.update ret {"name" (second form)})
      (.update ret {"args" (->> form (nth 2))})))
  ret)
