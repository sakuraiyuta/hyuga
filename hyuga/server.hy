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
(import hyuga.sym.loader [load-src!])
(import hyuga.cursor *)
(import hyuga.lspspec *)
(import hyuga.log [logger])

(setv $SERVER (LanguageServer))

(defn [($SERVER.feature COMPLETION)] completion
  [params]
  "`textDocument/completion` request handler."
  (try
    (let [word (cursor-word $SERVER
                            params.text_document.uri
                            params.position.line
                            params.position.character)]
      (logger.info f"completion word={(repr word)}")
      (if (is-not word None)
        (->> (create-items word)
             create-completion-list)
        (create-completion-list [])))
    (except [e Exception]
            (log-error "completion" e)
            (raise e))))

(defn [($SERVER.feature HOVER)] hover
  [params]
  "`textDocument/hover` request handler."
  (try
    (let [word (cursor-word-all $SERVER
                                params.text_document.uri
                                params.position.line
                                params.position.character)]
      (logger.info f"hover: word={word}")
      (when (is-not word None)
        (let [details (-> word (get-details params.text_document.uri))]
          (when details
            (create-hover (:docs details))))))
    (except
      [e Exception]
      (log-error "hover" e)
      (raise e))))

(defn [($SERVER.feature DEFINITION)] definition
  [params]
  (try
    (let [word (cursor-word-all $SERVER
                                params.text_document.uri
                                params.position.line
                                params.position.character)]
      (logger.info f"definition: word={word}")
      (when (is-not word None)
        (let [matches (get-matches word)
              locations (create-location-list matches)]
          (logger.debug f"locations={locations}")
          locations)))
    (except [e Exception]
            (log-error "definition" e)
            (raise e))))

(defn [($SERVER.feature TEXT_DOCUMENT_DID_OPEN)] did-open
  [params]
  (try
    (logger.info f"did-open: uri={params.text_document.uri}")
    (parse-src! (-> params.text_document.uri
                    $SERVER.workspace.get_document
                    (. source))
                $SERVER.workspace.root_uri
                params.text_document.uri)
    (except [e Exception]
            (log-error "did-open" e)
            (raise e))))

(defn [($SERVER.feature TEXT_DOCUMENT_DID_CLOSE)] did-close
  [params]
  None)

(defn [($SERVER.feature TEXT_DOCUMENT_DID_CHANGE)] did-change
  [params]
  (try
    (logger.info f"did-change: uri={params.text_document.uri}")
    (load-src! (-> params.text_document.uri
                   $SERVER.workspace.get_document
                   (. source))
               $SERVER.workspace.root_uri
               params.text_document.uri
               "hyuga.sym.dummy"
               True)
    (except [e Exception]
            (log-error "did-open" e)
            (raise e))))

(defn start
  []
  ($SERVER.start_io))
