(require hyrule * :readers *)
(import hyrule.collections [prewalk postwalk])
(import toolz.itertoolz *)
(import pygls.lsp.methods [COMPLETION
                           HOVER
                           TEXT_DOCUMENT_DID_CHANGE
                           TEXT_DOCUMENT_DID_CLOSE
                           TEXT_DOCUMENT_DID_OPEN])
(import pygls.lsp.types [CompletionItem
                         CompletionList
                         CompletionOptions
                         CompletionParams
                         Hover
                         MarkupContent
                         MarkupKind])
(import pygls.server [LanguageServer])
(import hyuga.inspect *)
(import hyuga.lspspec *)
(import hyuga.cursor *)
(import hyuga.log *)

(setv $SERVER (LanguageServer))

(defn [($SERVER.feature COMPLETION)] completion
  [params]
  "`textDocument/completion` request handler."
  (let [word (cursor-word $SERVER
                          params.text_document.uri
                          params.position.line
                          params.position.character)]
    (logger.debug (.format "completion word={}" (repr word)))
    (if (is-not word None)
      (->> (get-candidates word)
           create-items
           create-completion-list)
      (create-completion-list []))))

(defn [($SERVER.feature HOVER)] hover
  [params]
  "`textDocument/hover` request handler."
  (let [word (cursor-word-all $SERVER
                              params.text_document.uri
                              params.position.line
                              params.position.character)]
    (logger.debug (.format "hover word={}" (repr word)))
    (when (is-not word None)
      (let [docs (-> word get-details :docs)]
        (Hover
          :contents (MarkupContent
                      :kind MarkupKind.PlainText
                      :value docs))))))

(defn [($SERVER.feature TEXT_DOCUMENT_DID_OPEN)] did-open
  [params]
  (eval-define! (-> params.text_document.uri
                    $SERVER.workspace.get_document
                    (. source))))

(defn [($SERVER.feature TEXT_DOCUMENT_DID_CLOSE)] did-close
  [params]
  None)

(defn [($SERVER.feature TEXT_DOCUMENT_DID_CHANGE)] did-change
  [params]
  (did-open params))

(defn start
  []
  ($SERVER.start_io))
