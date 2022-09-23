(import logging)
(import traceback)

(setv logger (logging.getLogger "hyuga"))
(let [handler (logging.FileHandler :filename "/tmp/hyuga.log")]
  (.setLevel logger logging.DEBUG)
  (.setFormatter handler (logging.Formatter "%(levelname)-9s %(asctime)s [%(name)s] %(message)s"))
  (logger.addHandler handler))

(defn error-trace
  [logfn fname e]
  "TODO: doc"
  (let [tb (.join "" (traceback.format_exc))]
    (logfn f"{fname}: error e={e}\n----- stacktrace -----\n{tb}\n----- stacktrace end -----")))

(defn log-error
  [fname e]
  (error-trace logger.error fname e))

(defn log-warn
  [fname e]
  (error-trace logger.warning fname e))
