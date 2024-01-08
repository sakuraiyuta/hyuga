(import logging)
(import traceback)
(import os.path)
(import tempfile)

(setv logger (logging.getLogger "hyuga"))
(let [handler (logging.FileHandler :filename (os.path.join (tempfile.gettempdir) "hyuga.log"))]
  (.setLevel logger logging.DEBUG)
  (.setFormatter handler (logging.Formatter "%(levelname)-9s %(asctime)s [%(name)s] %(message)s"))
  (logger.addHandler handler))

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
