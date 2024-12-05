(require hyrule * :readers *)

(import pytest)
(import toolz.itertoolz [first])
(import toolz)

(import lsprotocol.types [Location
                          Range
                          Position
                          Hover
                          MarkupContent
                          CompletionItem])

(import misc *)
(import hyuga.lspspec *)
(import fixture [fixture-syms])

(setv test-completion-item
  (CompletionItem :label "[] eval"
                  :label_details None
                  :kind 14
                  :tags None
                  :detail "eval [(builtin)]\n\t<built-in function eval>\n\nEvaluate the given source in the context of globals and locals.\n\nThe source may be a string representing a Python expression\nor a code object as returned by compile().\nThe globals must be a dictionary and locals can be any mapping,\ndefaulting to the current globals and locals.\nIf only globals is given, locals defaults to it."
                  :documentation None
                  :deprecated None
                  :preselect None
                  :sort-text None
                  :filter-text None
                  :insert-text "eval"
                  :insert-text-format None
                  :insert-text-mode None
                  :text-edit None
                  :text-edit-text None
                  :additional-text-edits None
                  :commit-characters None
                  :command None
                  :data None))

(defn [(pytest.mark.parametrize
        #("word" "full_sym" "expected")
        [#("eval"
          #("eval"
            {"sym" "\\(builtin)\\eval"
            "type" eval
            "uri" False
            "scope" ""
            "ns" "(builtin)"
            "docs" "eval [(builtin)]\n\t<built-in function eval>\n\nEvaluate the given source in the context of globals and locals.\n\nThe source may be a string representing a Python expression\nor a code object as returned by compile().\nThe globals must be a dictionary and locals can be any mapping,\ndefaulting to the current globals and locals.\nIf only globals is given, locals defaults to it."
            "pos" None}) 
           test-completion-item)])]
  test_create-item
  [word full-sym expected]
  (print f"expected: { expected }")
  (print f"got:      { (create-item word full-sym) }")
  (assert (= (create-item word full-sym) expected)))

(setv test-completion-items 
  [(CompletionItem :label "[(builtin)] eval" 
                    :label_details None
                    :kind 14 
                    :tags None
                    :detail "eval [(builtin)]\n\t<built-in function eval>\n\nEvaluate the given source in the context of globals and locals.\n\nThe source may be a string representing a Python expression\nor a code object as returned by compile().\nThe globals must be a dictionary and locals can be any mapping,\ndefaulting to the current globals and locals.\nIf only globals is given, locals defaults to it."
                    :documentation None
                    :deprecated None
                    :preselect None
                    :sort_text None
                    :filter_text None 
                    :insert_text "eval"
                    :insert_text_format None
                    :insert_text_mode None
                    :text_edit None
                    :text_edit_text None
                    :additional_text_edits None
                    :commit_characters None
                    :command None
                    :data None)
    (CompletionItem :label "[(hykwd)] eval-and-compile" 
                    :label_details None
                    :kind 3 
                    :tags None
                    :detail "eval-and-compile [(hykwd)]\n\t<function pattern_macro.<locals>.dec.<locals>.wrapper_maker.<locals>.wrapper at 0x7f8f88462520>\n\nNo docs." 
                    :documentation None
                    :deprecated None
                    :preselect None
                    :sort_text None
                    :filter_text None 
                    :insert_text "eval-and-compile"
                    :insert_text_format None
                    :insert_text_mode None
                    :text_edit None
                    :text_edit_text None
                    :additional_text_edits None
                    :commit_characters None
                    :command None
                    :data None)
    (CompletionItem :label "[(hykwd)] eval-when-compile" 
                    :label_details None
                    :kind 3 
                    :tags None
                    :detail "eval-when-compile [(hykwd)]\n\t<function pattern_macro.<locals>.dec.<locals>.wrapper_maker.<locals>.wrapper at 0x7f8f884625c0>\n\nNo docs."
                    :documentation None
                    :deprecated None
                    :preselect None
                    :sort_text None
                    :filter_text None 
                    :insert_text "eval-when-compile"
                    :insert_text_format None
                    :insert_text_mode None
                    :text_edit None
                    :text_edit_text None
                    :additional_text_edits None
                    :commit_characters None
                    :command None
                    :data None)])

(defn [(pytest.mark.parametrize
        #("word" "root_uri" "doc_uri" "expected")
        [#("eval"
           ""
           ""
           test-completion-items)])]
  test_create-items
  [word root-uri doc-uri expected] 
  (print f"expected: { expected }")
  (print f"got:      { (create-items word root-uri doc-uri) }")
  (assert (= (get (create-items word root-uri doc-uri) 0) 
             (get expected 0))))

(defn [(pytest.mark.parametrize
        #("items" "expected")
        [#(test-completion-items
           (CompletionList :is_incomplete False 
                           :items test-completion-items))])]
  test_create-completion-list
  [items expected] 
  (print f"expected: { expected }")
  (print f"got:      { (create-completion-list items) }")
  (assert (= (create-completion-list items) expected)))

(defn [(pytest.mark.parametrize
        #("docs" "expected")
        [#("test string" 
           (Hover
            :contents (MarkupContent 
                       :kind MarkupKind.PlainText
                       :value (fix-dummy "test string"))))])]
  test_create-hover
  [docs expected]
  (print f"expected: { expected }")
  (print f"got:      { (create-hover docs) }")
  (assert (= (create-hover docs) expected)))

(setv test-location
  (let [obj-pos (Position :line (-> #(1 1) first dec) 
                          :character (-> #(1 1) second dec)) 
        obj-range (Range :start obj-pos 
                         :end obj-pos)] 
    (Location :uri "file:///home/cdfig/projects/hy/hyuga/tests/misc.hy"  
              :range obj-range)))

(defn [(pytest.mark.parametrize
        #("pos" "uri" "expected")
        [#(#(1 1) "file:///home/cdfig/projects/hy/hyuga/tests/misc.hy" 
           test-location)])]
  test_create-location
  [pos uri expected]
  (print f"expected: { expected }")
  (print f"got:      { (create-location pos uri) }")
  (assert (= (create-location pos uri) expected)))

(defn [(pytest.mark.parametrize
        #("items" "expected")
        [#([test-location] 
           [test-location])])]
  test_distinct-locations
  [items expected]
  (print f"expected: { expected }")
  (print f"got:      { (distinct-locations items) }")
  (assert (= (distinct-locations items) expected)))

(defn [(pytest.mark.parametrize
         #("symvals" "expected")
         [#([["docs-str"
              {"pos" #(1 1)
               "uri" "file:///home/cdfig/projects/hy/hyuga/tests/misc.hy"}]]
            [test-location])])]
  test_create-location-list
  [symvals expected fixture-syms]
  (print f"expected: { expected }")
  (print f"got:      { (create-location-list symvals) }")
  (assert (= (create-location-list symvals) expected)))