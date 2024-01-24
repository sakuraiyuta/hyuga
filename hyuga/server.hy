(require hyrule * :readers *)
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

(defn get-params-dict
  [$SERVER params]
  (let [uri params.text_document.uri
        src (-> uri
                $SERVER.workspace.get_document
                (getattr "source"))
        pos #(params.position.line params.position.character)]
    {"root-uri" $SERVER.workspace.root-uri
     "doc-uri" params.text_document.uri
     "src" src
     "pos" pos}))

(defn [($SERVER.feature
         TEXT_DOCUMENT_COMPLETION
         :options (CompletionOptions :trigger-characters ["."]))] completion
  [params]
  "`textDocument/completion` request handler."
  (try
    (let+ [{src "src" [ln cn] "pos" root-uri "root-uri" doc-uri "doc-uri"}
           (get-params-dict $SERVER params)
           word (cursor-word src ln cn)]
      (logger.info f"completion word={(repr word)} pos=#({ln}, {cn})")
      (if (is-not word None)
        (->> (create-items word ln cn root-uri doc-uri)
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
    (let+ [{src "src" [ln cn] "pos" root-uri "root-uri" doc-uri "doc-uri"}
           (get-params-dict $SERVER params)
           word (cursor-word-all src ln cn)]
      (logger.info f"hover: word={word}")
      (when (is-not word None)
        (let [details (-> word (get-details root-uri doc-uri))]
          (when details
            (create-hover (:docs details))))))
    (except
      [e Exception]
      (log-error "hover" e)
      (raise e))))

(defn [($SERVER.feature TEXT_DOCUMENT_DEFINITION)] definition
  [params]
  (try
    (let+ [{src "src" [ln cn] "pos" root-uri "root-uri" doc-uri "doc-uri"}
           (get-params-dict $SERVER params)
           word (cursor-word-all src ln cn)]
      (logger.info f"definition: word={word}")
      (when (is-not word None)
        (let [matches (get-matches word root-uri doc-uri)
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
    (let [text-document (-> params.text_document.uri
                            $SERVER.workspace.get_text_document)]
      (parse-src! text-document.source
                  $SERVER.workspace.root_uri
                  params.text_document.uri))
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
    (let [text-document (-> params.text_document.uri
                            $SERVER.workspace.get_text_document)]
      (load-src! text-document.source
                 $SERVER.workspace.root_uri
                 params.text_document.uri
                 (uri->mod
                   $SERVER.workspace.root_uri
                   params.text_document.uri)
                 True))
    (except [e Exception]
      (log-error "did-change" e))
    (finally
      None)))

(defn start
  []
  (logger.info f"----- hyuga {(get-version)} start -----")
  ($SERVER.start_io))
