(require hyrule * :readers *)
(import toolz.itertoolz *)
(import pygls.lsp.types [CompletionItem
                         CompletionList
                         CompletionOptions
                         CompletionParams
                         CompletionItemKind
                         Hover
                         MarkupContent
                         MarkupKind])
(import hyuga.log *)

(defn decide-kind
  [anno]
  "@see: https://docs.microsoft.com/en-us/dotnet/api/microsoft.visualstudio.languageserver.protocol.completionitemkind?view=visualstudiosdk-2022"
  (-> (branch (in it anno)
              "builtin" CompletionItemKind.Function
              ;; FIXME: pygls CompletionItemKind.Macro is not defined yet.
              ; "macro" 118115
              ; "macro" CompletionItemKind.Macro
              "macro" CompletionItemKind.Function
              "module" CompletionItemKind.Module
              "function" CompletionItemKind.Function
              "type" CompletionItemKind.Class
              else CompletionItemKind.Variable)
      int))

(defn create-items
  [candidates]
  "TODO: doc"
  (logger.debug (.format "candidates={}" (repr candidates)))
  (->> candidates
       (map #%(CompletionItem :label (:sym %1)
                              :insert_text (:sym %1)
                              :detail (:docs %1)
                              :kind (decide-kind (str (:type %1)))))
       list))

(defn create-completion-list
  [items [is-incomplete False]]
  "TODO: doc"
  (CompletionList :is_incomplete is-incomplete
                  :items items))
