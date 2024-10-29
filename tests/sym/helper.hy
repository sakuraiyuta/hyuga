(require hyrule * :readers *)

(import pytest)
(import toolz.itertoolz [first])

(import hyuga.global [$GLOBAL])
(import hyuga.sym.doc [create-docs])
(import hyuga.sym.helper *)
(import fixture [fixture-syms])
(import misc *)

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#("a_b" "a-b")
          #("a>b" "a>b")
          #("a.__doc__" "a.__doc__")])]
  test_sym-py-hy
  [val expected]
  (assert (= (sym-py->hy val) expected)))

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#("a-b" "a_b")
          #("a>b" "hyx_aXgreaterHthan_signXb")
          #("a.__doc__" "a.__doc__")])]
  test_sym-hy-py
  [val expected]
  (assert (= (sym-hy->py val) expected)))

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#(["a_b" None] ["a-b" None])
          #(["hyx_aXsolidusXb" None] ["a/b" None])])]
  test_sym-py-val-sym-hy-val
  [val expected]
  (assert (= (sym-py/val->sym-hy/val val) expected)))

;; TODO: testcase for -get-macro-doc
;(eval-define! "")
;(defn [(let [val-doto (-> ($GLOBAL.get-$SYMS)
;                           (get "doto")
;                           (get "type"))]
;       (pytest.mark.parametrize
;         #("symhy" "symtype" "expected")
;         [#("doto"
;             val-doto
;             f"doto {val-doto}\n  macro\n\n{(doc val-doto)}")]))]
;  test_get-macro-doc
;  [symhy symtype expected fixture-syms]
;  (assert (= (-get-macro-doc symhy symtype) expected)))
;(-get-macro-doc "defmacro!" defmacro!)

(defn [(pytest.mark.parametrize
         #("symhy" "symtype" "scope" "expected")
         [#("create-docs"
             create-docs
             "global"
             (docs-str create-docs "global"))])]
  test_create-docs
  [symhy symtype scope expected]
  (assert (= (create-docs symhy symtype scope ".") expected)))

;(defn [(pytest.mark.parametrize
;         #("symsplitted" "expected")
;         [#(#("mesa" "ti") "mesa")
;          #(#("mesa" "") "mesa")
;          #(#("mesa") "")
;          #(#("hyuga" "sym" "helper") "hyuga.sym")])]
;  test_module-or-class
;  [symsplitted expected]
;  (assert (= (module-or-class? symsplitted) expected)))
