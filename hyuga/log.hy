(import logging)
(import traceback)
(import os.path)
(import tempfile)

(logging.basicConfig :level logging.INFO
                     :format "%(levelname)-9s %(asctime)s [%(name)s] %(message)s"
                     :filename (os.path.join (tempfile.gettempdir) "hyuga.log"))
(setv logger (logging.getLogger "hyuga"))

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
