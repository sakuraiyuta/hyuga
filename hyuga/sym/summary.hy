(require hyrule * :readers *)
(require hyrule.argmove [-> ->>])
(import hyrule.macrotools [map-model])

(import hy.models [Expression List String])
(import toolz.itertoolz *)

(import hyuga.log *)
(import hyuga.sym.helper *)

(defn get-defn-summary
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

(defn get-defclass-methods [forms ret]
  "Collect summaries of every (defn …) that appears inside `forms`.
   Results are accumulated into `ret[\"methods\"]` and `ret` is returned."
  (map-model
    (list forms)
    #%(do
        (when (and (isinstance %1 Expression)
                   (= (first %1) 'defn))
          (.append (get ret "methods")
                   (get-defn-summary %1)))
        None))
  forms)

(defn get-defclass-summary
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

(defn get-setv-summary
  [form]
  "TODO: doc"
  {"name" (-> form second fix-hy-symbol)
   "type" "setv"
   "value" (nth 2 form)
   "docs" (->> form (nth 2) fix-hy-symbol)
   "pos" #((getattr (second form) "start_line")
           (getattr (second form) "start_column"))})

(defn get-import-summary [form]
  (setv ret {"name" (-> form second fix-hy-symbol)
             "type" "import"
             "pos"  #((getattr (second form) "start_line")
                      (getattr (second form) "start_column"))
             "includes" None})

  (let [options (list (drop 2 form))]
    (when (> (count options) 0)
      (let [option (first options)]
        (if (isinstance option List)
          (do
            (setv transformed
              (map-model
                option
                #%(if (isinstance %1 HySymbol)
                    (fix-hy-symbol %1)
                    %1)))
            (setv includes (->> transformed
                                 hy.eval
                                 (filter #%(not (= ":as" %1)))
                                 (map sym-hy->py)
                                 list))
            (.update ret {"includes" includes}))
          (.update ret {"includes" "*"})))))
  ret)

(defn get-require-summary
  [form]
  "TODO: doc"
  ;; TODO: implement require form parser
  (setv ret {"name" (-> form second fix-hy-symbol)
             "type" "require"
             "pos" #((getattr (second form) "start_line")
                     (getattr (second form) "start_column"))
             "includes" []})
  ret)

(defn get-defmacro-summary
  [form]
  "TODO: doc"
  ;; TODO: implement defmacro form parser
  (setv ret {"name" (-> form second fix-hy-symbol)
             "type" "defmacro"
             "pos" #((getattr (second form) "start_line")
                     (getattr (second form) "start_column"))
             "includes" []})
  ret)

(defn get-unknown-def-summary
  [form]
  "TODO: doc"
  {"name" (-> form second fix-hy-symbol)
   "type" (-> form first fix-hy-symbol)
   "pos" #((getattr (second form) "start_line")
           (getattr (second form) "start_column"))})

(defn get-form-summary
  [form]
  (let [hytype (-> form first str)]
    (branch (= hytype it)
            "defn" (get-defn-summary form)
            "defclass" (get-defclass-summary form)
            "defmacro" (get-defmacro-summary form)
            "setv" (get-setv-summary form)
            "import" (get-import-summary form)
            "require" (get-require-summary form)
            else (cond
                   (.startswith hytype "def")
                   (get-unknown-def-summary form)
                   True None))))
