(import logging)
(import traceback)
(import os.path)
(import os [environ])
(import tempfile)

(setv logger
      (let [logger (logging.getLogger "hyuga")
            handler (logging.FileHandler
                      :filename (os.path.join (tempfile.gettempdir)
                                              "hyuga.log"))
            formatter (logging.Formatter
                        "%(levelname)-9s %(asctime)s [%(name)s] %(message)s")]
        (logger.setLevel (.upper (.get environ "HYUGA_LOGLEVEL" "INFO")))
        (handler.setFormatter formatter)
        (logger.addHandler handler)
        logger))

(defn error-trace
  [logfn msg e]
  "TODO: doc"
  (let [tb (.join "" (traceback.format_exc))]
    (logfn f"{msg}\n----- stacktrace -----\n{tb}\n----- stacktrace end -----")))

(defn log-error
  [msg e]
  (error-trace logger.error msg e))

(defn log-warn
  [msg e]
  (error-trace logger.warning msg e))
