(require hyrule * :readers *)

(import toolz.itertoolz *)

(import hyuga.sym.helper [fix-hy-symbol])

(defn get-summary
  [form]
  "TODO: doc"
  ;; TODO: implement require form parser
  (setv ret {"name" (-> form second fix-hy-symbol)
             "type" "require"
             "pos" #((getattr (second form) "start_line")
                     (getattr (second form) "start_column"))
             "includes" []})
  ret)

