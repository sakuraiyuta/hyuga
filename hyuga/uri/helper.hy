(require hyrule * :readers *)
(require hyrule.argmove [-> ->>])
(import os [listdir])
(import re [sub])
(import subprocess)
(import shutil [which])
(import toolz.itertoolz *)
(import hyuga.log [logger])

(defn remove-uri-prefix
  [uri]
  (sub "^[a-z]+://" "" uri))

(defn get-poetry-venv
  []
  (let [result
        (subprocess.run
          ["poetry" "env" "info" "-p"]
          :stdout subprocess.PIPE)]
    (.rstrip (result.stdout.decode "utf-8"))))

(defn get-venv
  [root-uri]
  (let [root-path (remove-uri-prefix root-uri)
        base-venv-path (if (which "poetry")
                         f"{(get-poetry-venv)}/lib"
                         f"{root-path}/.venv/lib")
        venv-python-dir (->> (listdir base-venv-path) first)]
    f"{base-venv-path}/{venv-python-dir}/site-packages"))
