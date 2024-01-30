(require hyrule * :readers *)

(import toolz.itertoolz *)
(import hyuga.sym.helper [fix-hy-symbol])
(import hyuga.sym.summary [defn
                           defclass
                           defmacro
                           setv
                           import
                           require])

(defn get-form-summary
  [form]
  (let [hytype (-> form first str)]
    (branch (= hytype it)
            "defn" (defn.get-summary form)
            "defclass" (defclass.get-summary form)
            "defmacro" (defmacro.get-summary form)
            "setv" (setv.get-summary form)
            "import" (import.get-summary form)
            "require" (require.get-summary form)
            else (cond
                   (.startswith hytype "def")
                   (get-unknown-def-summary form)
                   True None))))

(defn get-unknown-def-summary
  [form]
  "TODO: doc"
  {"name" (-> form second fix-hy-symbol)
   "type" (-> form first fix-hy-symbol)
   "pos" #((getattr (second form) "start_line")
           (getattr (second form) "start_column"))})
