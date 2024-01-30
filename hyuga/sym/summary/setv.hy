(require hyrule * :readers *)

(import toolz.itertoolz *)

(import hyuga.sym.helper [fix-hy-symbol])

(defn get-summary
  [form]
  "TODO: doc"
  {"name" (-> form second fix-hy-symbol)
   "type" "setv"
   "value" (nth 2 form)
   "docs" (->> form (nth 2) fix-hy-symbol)
   "pos" #((getattr (second form) "start_line")
           (getattr (second form) "start_column"))})
