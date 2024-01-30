(require hyrule * :readers *)
(import hyrule.collections [walk])

(import hy.models [List String])
(import toolz.itertoolz *)

(import hyuga.sym.helper [fix-hy-symbol])
(import hyuga.sym.summary.const [BASE-SUMMARY])

(defn get-summary
  [form]
  (setv ret {"name" ""
             "type" "defn"
             "docs" ""
             "decorators" None
             "args" ""
             "pos" None})
  (if (-> form second (isinstance List))
    (do
      (.update ret {"decorator" (second form)})
      (.update ret {"name" (->> form (nth 2) fix-hy-symbol)})
      (.update ret {"pos" #((getattr (nth 2 form) "start_line")
                            (getattr (nth 2 form) "start_column"))})
      (.update ret {"args" (nth 3 form)})
      (when (isinstance (nth 4 form) String)
        (.update ret {"docs" (->> (nth 4 form) str)})))
    (do
      (.update ret {"name" (-> form second fix-hy-symbol)})
      (.update ret {"pos" #((getattr (second form) "start_line")
                            (getattr (second form) "start_column"))})
      (.update ret {"args" (->> form (nth 2))})
      (when (isinstance (nth 3 form) String)
        (.update ret {"docs" (-> (nth 3 form) str)}))))
  ret)
