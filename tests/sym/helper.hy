(require hyrule * :readers *)

(import pytest)
(import toolz.itertoolz [first])

(import hyuga.global [$GLOBAL])
(import hyuga.sym.helper *)
(import fixture [fixture-syms])

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

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#(["first" None] False)
          #(["toolz" None] False)
          #(["!not-in-sym!" None] True)])]
  test_not-in-sym
  [val expected fixture-syms]
  (assert (= (not-in-$SYM? val) expected)))

(defn [(pytest.mark.parametrize
         #("val" "expected")
         [#(["eval" None] True)
          #(["first" None] True)
          #(["toolz" None] True)
          #(["_hy-let" None] False)
          #(["_hy-anon" None] False)
          #(["-hyuga-eval-form" None] False)])]
  test_not-exclude-sym
  [val expected fixture-syms]
  (assert (= (not-exclude-sym? val) expected)))

(defn [(pytest.mark.parametrize
         #("val" "scope" "expected")
         [#(["eval" eval]
            "local"
            {"sym" "eval"
             "type" eval
             "scope" "local"
             "docs" f"eval {eval}\n\tlocal\n\n{eval.__doc__}"})
          #(["first" first]
            "global"
            {"sym" "first"
             "type" first
             "scope" "global"
             "docs" f"first {first}\n\tglobal\n\n{first.__doc__}"})])]
  test_add-sym
  [val scope expected fixture-syms]
  (assert (= (add-sym! val scope) expected)))

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
         #("symhy" "symtype" "scope" "origdocs" "expected")
         [#("create-docs"
             create-docs
             "global"
             "TODO: doc"
             f"create-docs {create-docs}\n\tglobal\n\nTODO: doc")])]
  test_create-fn-docs
  [symhy symtype scope origdocs expected]
  (assert (= (-create-fn-docs symhy symtype scope origdocs) expected)))

(defn [(pytest.mark.parametrize
         #("symhy" "symtype" "scope" "expected")
         [#("create-docs"
             create-docs
             "global"
             f"create-docs {create-docs}\n\tglobal\n\nTODO: doc")])]
  test_create-docs
  [symhy symtype scope expected]
  (assert (= (create-docs symhy symtype scope) expected)))

;(defn [(pytest.mark.parametrize
;         #("symsplitted" "expected")
;         [#(#("mesa" "ti") "mesa")
;          #(#("mesa" "") "mesa")
;          #(#("mesa") "")
;          #(#("hyuga" "sym" "helper") "hyuga.sym")])]
;  test_module-or-class
;  [symsplitted expected]
;  (assert (= (module-or-class? symsplitted) expected)))
