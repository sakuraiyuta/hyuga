(require hyrule * :readers *)

(import hy.models [Expression List String])
(import toolz.itertoolz *)

(import hyuga.sym.helper [fix-hy-symbol])
(import hyuga.sym.summary [defn])

(defn get-defclass-methods
  [forms ret]
  "TODO: doc"
  ;; TODO: split static/object method and keep info
  (walk #%(when (isinstance %1 Expression)
            (let [summary (defn.get-summary %1)
                  methods (get ret "methods")]
              (.append methods summary)
              (.update ret {"methods" methods}))
            %1)
        #%(return %1)
        forms))

(defn get-summary
  [form]
  "TODO: doc"
  (setv ret {"name" (-> form second fix-hy-symbol)
             "type" "defclass"
             "docs" ""
             "inherits" (nth 2 form)
             "methods" []
             "pos" #((getattr (second form) "start_line")
                     (getattr (second form) "start_column"))})
  (let [doc-exists? (isinstance (nth 3 form) String)
        method-forms (if doc-exists?
                       (drop 3 form)
                       (drop 2 form))]
    (when doc-exists?
      (.update ret {"docs" (-> (nth 3 form) str)}))
    (get-defclass-methods method-forms ret))
  ;; TODO: get properties in __init__ if exists
  ret)
