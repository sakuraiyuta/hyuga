(require hyrule * :readers *)
(import hyrule.collections [walk])

(import hy.models [List String Expression])
(import toolz.itertoolz *)

(import hyuga.sym.helper [fix-hy-symbol])
(import hyuga.sym.summary.const [BASE-SUMMARY])

(defn fix-defn-form
  [form]
  ;; (defn [decorator1 decorator2] :tp [T1 T2] #^ annotation name [params] …)
  ;; @see https://docs.hylang.org/en/stable/api.html#defn
  (setv ret (list `[~@form]))
  ;; index 1: decorator
  (when (-> ret second (isinstance List) not)
    (.insert ret 1 '[]))
  ;; index 2: :tp
  (when (->> ret (nth 1) (= ':tp) not)
    (.insert ret 2 ':tp))
  ;; index 3: type parameters
  (when (as-> ret it
          (nth 3 it) (isinstance it List) (not it))
    (.insert ret 3 (List)))
  ;; TODO: index 4, 5: check annotation -> #^ is macro so can't fix simply.
  ;; index 6: doc
  (when (as-> ret it
          (nth 6 it) (isinstance it String) (not it))
    (.insert ret 6 (String "")))
  (Expression ret))

;; sample:
;; hy.models.Expression([
;; 0 hy.models.Symbol('defn'),
;; 1 hy.models.List(),
;; 2 hy.models.Keyword('tp'),
;; 3 hy.models.List(),
;; 4 hy.models.Symbol('test'),
;; 5 hy.models.List(),
;; 6 hy.models.String('testdoc'),
;; 7 hy.models.Symbol('None')])
(defn get-summary
  [form]
  (setv ret {"type" "defn"})
  (let [fixed-form (fix-defn-form form)]
    (.update ret {"decorators" (second fixed-form)})
    (.update ret {"argtypes" (nth 3 fixed-form)})
    (.update ret {"name" (->> fixed-form (nth 4) fix-hy-symbol)})
    (.update ret {"args" (nth 5 fixed-form)})
    (.update ret {"docs" (->> (nth 6 fixed-form) str)})
    (.update ret {"pos" #((getattr (nth 4 fixed-form) "start_line")
                          (getattr (nth 4 fixed-form) "start_column"))}))
  ret)
