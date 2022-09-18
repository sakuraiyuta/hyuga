(require hyrule * :readers *)
(import hyrule.iterables [drop-last])

(import hyuga.log *)
(import hyuga.global [$GLOBAL])

(import toolz.itertoolz *)

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
  [sym-hy/val scope]
  "TODO: doc"
  (let [[sym-hy val] sym-hy/val
        docs (create-docs sym-hy val scope)]
    ($GLOBAL.add-$SYMS sym-hy
                       val
                       (judge-scope val scope)
                       docs)))

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

(defn -create-fn-docs
  [sym-hy symtype scope orig-docs]
  "TODO: doc"
  (.format "{} {}\n\t{}\n\n{}"
           sym-hy (str symtype) scope orig-docs))

(defn create-docs
  [sym-hy symtype scope]
  "TODO: doc"
  (try
    (-create-fn-docs
      sym-hy symtype scope (or symtype.__doc__ "No docs."))
    (except
      [e BaseException]
      (logger.debug
        (.format "cannot read __doc__. try macro docs. e={}"
                 e))
      (-create-fn-docs sym-hy
                       symtype
                       scope
                       (-get-macro-doc sym-hy symtype)))))

(defn module-or-class?
  [sym-splitted]
  "TODO: doc"
  (if (-> sym-splitted count (> 1))
    (->> sym-splitted (drop-last 1) (.join "."))
    ""))
