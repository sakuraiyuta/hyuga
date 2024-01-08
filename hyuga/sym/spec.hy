(require hyrule * :readers *)
(import hyrule.iterables [distinct])
(import importlib)
(import pkgutil)
(import os)

(defn get-module-specs
  [path]
  ;; FIXME: importing pip causes distutils AssertionError.
  ;; @see https://github.com/pypa/setuptools/issues/3297
  (setv (get os.environ "SETUPTOOLS_USE_DISTUTILS") "stdlib")
  (->> (pkgutil.iter-modules [path])
       (map #%(return %1.module-finder.path))
       distinct
       pkgutil.walk-packages
       (map #%(%1.module-finder.find-spec %1.name))
       tuple))

(defn get-builtins-spec
  []
  (importlib.util.find-spec "builtins"))
