(require hyrule * :readers *)
(import hyrule.iterables [distinct])
(import importlib)
(import pkgutil)
(import sys)
(import os)

(defn get-modules
  [path]
  (->> (pkgutil.iter-modules [path])
       tuple))

(defn get-module-specs
  [paths]
  ;; FIXME: importing pip causes distutils AssertionError.
  ;; @see https://github.com/pypa/setuptools/issues/3297
  (setv (get os.environ "SETUPTOOLS_USE_DISTUTILS") "stdlib")
  (->> (pkgutil.iter-modules (list paths))
       (map #%(return %1.module-finder.path))
       distinct
       pkgutil.walk-packages
       (map #%(%1.module-finder.find-spec %1.name))
       tuple))

(defn get-builtins-spec
  []
  (importlib.util.find-spec "builtins"))

(defn get-sys-module-specs
  []
  (->> sys.path
       get-module-specs))
