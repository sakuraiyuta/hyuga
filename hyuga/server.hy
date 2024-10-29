(require hyrule * :readers *)
(require hyrule.argmove [-> ->>])
(import toolz.itertoolz *)
(import lsprotocol.types [TEXT_DOCUMENT_COMPLETION
                          TEXT_DOCUMENT_HOVER
                          TEXT_DOCUMENT_DEFINITION
                          TEXT_DOCUMENT_DID_CHANGE
                          TEXT_DOCUMENT_DID_CLOSE
                          TEXT_DOCUMENT_DID_OPEN
                          CompletionOptions])
(import pygls.server [LanguageServer])

(import hyuga.api *)
(import hyuga.version [get-version])
(import hyuga.sym.loader [load-src!])
(import hyuga.cursor *)
(import hyuga.lspspec *)
(import hyuga.log [logger])
(import hyuga.version [get-version])

(setv $SERVER (LanguageServer :name __package__ :version (get-version)))

(defn [($SERVER.feature
         TEXT_DOCUMENT_COMPLETION
         :options (CompletionOptions :trigger-characters ["." " "]))] completion
  [params]
  "`textDocument/completion` request handler."
  (try
    (let [word (cursor-word $SERVER
                            params.text_document.uri
                            params.position.line
                            params.position.character)]
      (logger.info f"completion word={(repr word)}")
      (if (is-not word None)
        (->> (create-items word
                           $SERVER.workspace.root_uri
                           params.text_document.uri)
             create-completion-list)
        (create-completion-list [])))
    (except [e Exception]
      (log-error "completion" e)
      (raise e))))

(defn [($SERVER.feature TEXT_DOCUMENT_HOVER)] hover
  [params]
  "`textDocument/hover` request handler."
  ;; FIXME: only match in context
  (try
    (let [word (cursor-word-all $SERVER
                                params.text_document.uri
                                params.position.line
                                params.position.character)]
      (logger.info f"hover: word={word}")
      (when (is-not word None)
        (let [details (-> word (get-details
                                 $SERVER.workspace.root_uri
                                 params.text_document.uri))]
          (when details
            (create-hover (:docs details))))))
    (except
      [e Exception]
      (log-error "hover" e)
      (raise e))))

(defn [($SERVER.feature TEXT_DOCUMENT_DEFINITION)] definition
  [params]
  (try
    (let [word (cursor-word-all $SERVER
                                params.text_document.uri
                                params.position.line
                                params.position.character)]
      (logger.info f"definition: word={word}")
      (when (is-not word None)
        (let [doc-uri params.text_document.uri
              root-uri $SERVER.workspace.root_uri
              matches (get-matches word root-uri doc-uri)
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
    (parse-src! (. ($SERVER.workspace.get_text_document params.text_document.uri) source)
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
    (load-src! (. ($SERVER.workspace.get_text_document params.text_document.uri) source)
               $SERVER.workspace.root_uri
               params.text_document.uri
               (uri->mod
                 $SERVER.workspace.root_uri
                 params.text_document.uri)
               True)
    (except [e Exception]
      (log-error "did-open" e)
      (raise e))))

(defn start
  []
  (logger.info f"----- hyuga {(get-version)} start -----")
  ($SERVER.start_io))
