(require hyrule * :readers *)

(import hyrule.iterables [flatten])
(import urllib.parse [urlparse])
(import pkgutil)
(import pathlib [Path])
(import toolz.itertoolz *)
(import hy.models [reduce])

(import hyuga.sym.eval [eval-in!])
(import hyuga.log *)

(defn get-module-infos-recur
  [paths]
  (->> (pkgutil.walk-packages :path paths)
       tuple))

(defn get-module-infos
  [paths]
  (->> (pkgutil.iter-modules :path paths)
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

(defn get-specs
  [paths]
  (->> (get-module-infos paths)
       (map #%(return #(%1.name (get-finder %1))))
       (map #%(get-spec (second %1) (first %1)))
       tuple))

(defn get-specs-recur
  [path ret parents]
  ;; TODO: cannot find namespace package
  ;; @see https://github.com/python/cpython/issues/73444
  (let [specs (->> (get-module-infos-recur [path])
                   (map #%(return #(%1.name (get-finder %1))))
                   (map #%(get-spec (second %1) (first %1)))
                   list)
        cur-result (->> specs
                        (map #%(return [%1 parents]))
                        list)
        recur-result
        (as-> specs it
          (filter #%(return %1.submodule_search_locations) it)
          (map #%(let [ps %1.submodule_search_locations]
                   (->> ps
                        (map (fn [p]
                               (get-specs-recur
                                 p
                                 cur-result
                                 (+ parents [%1.name]))))
                        (reduce (fn [x y]
                                  (+ x y)))
                        list)) it)
          (reduce #%(+ %1 %2) it [[]])
          (list it))]
    (->> (+ cur-result (or recur-result [[]]) ret)
         (filter #%(< 0 (len %1)))
         list)))
