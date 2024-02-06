(require hyrule * :readers *)

(import hyrule.iterables [flatten])
(import urllib.parse [urlparse])
(import pkgutil)
(import importlib.util [find-spec])
(import importlib.machinery [ModuleSpec])
(import toolz.itertoolz *)
(import ast [parse])
(import re [search])
(import functools [reduce])
(import hy.pyops *)

(import hyuga.sym.eval [eval-in!])
(import hyuga.global [$GLOBAL])
(import hyuga.log *)

(defn get-module-infos-recur
  [paths]
  (->> (pkgutil.walk-packages :path paths)
       tuple))

(defn get-finder
  [module-info]
  module-info.module-finder)

(defn get-loader
  [finder name]
  (finder.find-loader name))

(defn get-spec
  [finder name]
  (finder.find-spec name))

(defn get-spec-by-name
  [name]
  (find-spec name))

(defn get-specs
  [paths]
  (->> (get-module-infos-recur paths)
       (map #%(return #(%1.name (get-finder %1))))
       (map #%(get-spec (second %1) (first %1)))
       list))

(defn get-specs-recur
  [path ret parents]
  ;; TODO: cannot find namespace package
  ;; @see https://github.com/python/cpython/issues/73444
  (let [specs (get-specs [path])
        cur-result (->> specs
                        (map #%(return [%1 parents]))
                        list)
        specs-has-submodule
        (->> specs
             (filter #%(getattr %1 "submodule_search_locations" False)))
        recur-result
        (try
          (as-> specs-has-submodule it
               (map #%(as-> %1.submodule_search_locations that
                           (map (fn [p]
                                  (get-specs-recur
                                    p
                                    cur-result
                                    (+ parents [%1.name])))
                                that)
                           (reduce + that [])
                           (list that))
                    it)
               (reduce + that [])
               (list it))
          (except [e BaseException]
            (log-error "get-specs-recur" e)
            []))]
    (print f"path={path}, ret={(len ret)}, cur-result={(len cur-result)}, recur-result={(len recur-result)}")
    (->> (+ ret cur-result recur-result)
         (filter #%(< 0 (len %1)))
         list)))

(defn get-ast
  [spec]
  (with [file (open spec.origin)]
    (parse (file.read))))

(defn get-module
  [spec]
  (spec.loader.load-module))

(defn spec-is-hy?
  [spec]
  (when (search r".hy[c]*$" spec.origin)
    spec.origin))

(defn spec-is-py?
  [spec]
  (when (search r".py[c]*$" spec.origin)
    spec.origin))

(defn mod-is-hy?
  [mod]
  (when (search r".hy[c]*$" mod.__file__)
    mod.__file__))

(defn -find-loaded-spec
  [filter-fns]
  (let [modules (->> ($GLOBAL.get-$SYMS)
                     .values
                     (filter #%(and (reduce (fn [v f] (and v (f %1))) filter-fns True)
                                    (isinstance (get %1 "type") ModuleSpec)))
                     tuple)]
    (if (< 0 (len modules))
      (-> modules first (get "type"))
      None)))

(defn find-loaded-spec
  [kwargs]
  (->> kwargs .items
       (map #%(let [k (-> %1 first (getattr "name"))
                    v (second %1)]
                (fn [d] (= (get d k) v))))
       tuple
       -find-loaded-spec))
