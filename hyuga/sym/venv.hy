(require hyrule * :readers *)

(import os [listdir])
(import re [sub])
(import pathlib [Path PurePath PureWindowsPath])
(import subprocess)
(import shutil [which])
(import toolz.itertoolz *)
(import urllib)

(import hyuga.log [logger])

(defn remove-uri-prefix
  [uri]
  (let [windows? (isinstance PurePath PureWindowsPath)
        unquoted (-> uri
                     urllib.parse.urlparse
                     (getattr "path")
                     urllib.parse.unquote)]
    (if (and windows?
             (-> unquoted str
                 (.startswith "/")))
      (get (str unquoted) #s 1:)
      (str unquoted))))

(defn get-poetry-venv
  []
  (let [result
        (subprocess.run
          ["poetry" "env" "info" "-p"]
          :stdout subprocess.PIPE)]
    (.rstrip (result.stdout.decode "utf-8"))))

(defn get-py-venv
  [root-path]
  (let [path (Path root-path)
        glob (tuple (path.rglob "pyvenv.cfg"))]
    (if (< 0 (len glob))
      (getattr (first glob) "path")
      None)))

(defn get-venv
  [root-path]
  (let [py-venv-path (get-py-venv root-path)
        base-venv-path
        (cond
          (which "poetry") f"{(get-poetry-venv)}/lib"
          py-venv-path f"{py-venv-path}/lib"
          True f"{root-path}/.venv/lib")
        venv-python-dir (->> (listdir base-venv-path) first)]
    f"{base-venv-path}/{venv-python-dir}/site-packages"))
