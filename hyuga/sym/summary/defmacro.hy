(require hyrule * :readers *)

(import toolz.itertoolz *)

(import hyuga.sym.helper *)

(defn get-summary
  [form]
  "TODO: doc"
  ;; TODO: implement defmacro form parser
  (setv ret {"name" (-> form second fix-hy-symbol)
             "type" "defmacro"
             "pos" #((getattr (second form) "start_line")
                     (getattr (second form) "start_column"))
             "includes" []})
  ret)
