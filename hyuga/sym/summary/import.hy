(require hyrule * :readers *)
(import hyrule.collections [walk])

(import hy.models [List])
(import toolz.itertoolz *)

(import hyuga.sym.helper [fix-hy-symbol
                          sym-hy->py])

(defn get-summary
  [form]
  (setv ret {"name" (-> form second fix-hy-symbol)
             "type" "import"
             "pos" #((getattr (second form) "start_line")
                     (getattr (second form) "start_column"))
             "includes" None})
  (let [options (list (drop 2 form))]
    ;; TODO: multiple import support(e.g. (import a.b x.y))
    (when (-> options count (> 0))
      (let [option (first options)]
        (if (isinstance option List)
          (.update ret {"includes" (->> option
                                        (walk #%(-> %1 fix-hy-symbol)
                                          #%(return %1))
                                        hy.eval
                                        (filter #%(not (= ":as" %1)))
                                        (map sym-hy->py)
                                        list)})
          (.update ret {"includes" "*"})))))
  ret)

