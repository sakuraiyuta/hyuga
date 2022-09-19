(require hyrule * :readers *)
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
                         ConfigurationParams
                         ConfigurationItem
                         Hover
                         MarkupContent
                         MarkupKind])
(import pygls.server [LanguageServer])

(import hyuga.api *)
(import hyuga.cursor *)
(import hyuga.lspspec *)
(import hyuga.log [logger])

(setv $SERVER (LanguageServer))

(defn [($SERVER.feature COMPLETION)] completion
  [params]
  "`textDocument/completion` request handler."
  (let [word (cursor-word $SERVER
                          params.text_document.uri
                          params.position.line
                          params.position.character)]
    (logger.debug f"completion word={(repr word)}")
    (if (is-not word None)
      (->> (get-candidates word)
           (create-items word)
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
  (logger.debug f"did-open: workspace={$SERVER.workspace.root_uri}")
  (parse-src! (-> params.text_document.uri
                  $SERVER.workspace.get_document
                  (. source))
              $SERVER.workspace.root_uri))

(defn [($SERVER.feature TEXT_DOCUMENT_DID_CLOSE)] did-close
  [params]
  None)

(defn [($SERVER.feature TEXT_DOCUMENT_DID_CHANGE)] did-change
  [params]
  (did-open params))

(defn start
  []
  ($SERVER.start_io))
