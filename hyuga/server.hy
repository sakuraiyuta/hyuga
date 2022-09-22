(require hyrule * :readers *)
(import toolz.itertoolz *)
(import pygls.lsp.methods [COMPLETION
                           HOVER
                           DEFINITION
                           TEXT_DOCUMENT_DID_CHANGE
                           TEXT_DOCUMENT_DID_CLOSE
                           TEXT_DOCUMENT_DID_OPEN])
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
      (create-hover (-> word get-details :docs)))))

(defn [($SERVER.feature DEFINITION)] definition
  [params]
  (let [word (cursor-word-all $SERVER
                              params.text_document.uri
                              params.position.line
                              params.position.character)]
    (logger.debug (.format "definition word={}" (repr word)))
    (when (is-not word None)
      (let+ [{pos "pos" uri "uri"} (-> word get-details)]
        (when (and pos uri)
          (create-location pos uri))))))

(defn [($SERVER.feature TEXT_DOCUMENT_DID_OPEN)] did-open
  [params]
  (logger.debug f"did-open: workspace={$SERVER.workspace.root_uri}")
  (parse-src! (-> params.text_document.uri
                  $SERVER.workspace.get_document
                  (. source))
              $SERVER.workspace.root_uri
              params.text_document.uri))

(defn [($SERVER.feature TEXT_DOCUMENT_DID_CLOSE)] did-close
  [params]
  None)

(defn [($SERVER.feature TEXT_DOCUMENT_DID_CHANGE)] did-change
  [params]
  (did-open params))

(defn start
  []
  ($SERVER.start_io))
