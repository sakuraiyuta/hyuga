(require hyrule * :readers *)

(import builtins)
(import toolz.itertoolz *)
(import pathlib [Path])

(defn path->uri
  [path [joins []]]
  (let [joined (hy.eval `(.joinpath path ~@joins))]
    (->> joined str (+ "file://"))))

(setv test-src-root-path
      (let [path (Path __file__)]
        (-> (nth 2 path.parents)
            (.joinpath "test_src"))))

(setv test-src-doc-path
      (-> test-src-root-path
          (.joinpath "doc.hy")))

(setv test-src-root-uri
      (path->uri test-src-root-path))

(setv test-src-doc-uri
      (path->uri test-src-doc-path))
