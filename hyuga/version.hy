(import
  importlib.metadata [version :as libversion packages_distributions PackageNotFoundError]
  sys
  platform
  pathlib [Path]
  toolz.itertoolz [first])

(import hyuga.log [logger])

(try
  (import tomllib)
  (except [ModuleNotFoundError]
    (import tomli :as tomllib)))

(defn pyproject-path []
  (for [p (iterate (fn [q] (.parent q)) (.resolve (Path __file__)))]
    (let [pp (Path p "pyproject.toml")]
      (when (.exists pp) (return pp)))))

(defn version-from-pyproject []
  (when-let [pp (pyproject-path)]
    (with [f (.open pp "rb")]
      (let [data (tomllib.load f)]
        (or (get-in data ["project" "version"])
            (get-in data ["tool" "poetry" "version"]))))))

(defn get-version []
  "Return this package’s version string, or None."
  (or
   (try (libversion "hyuga")
        (except [PackageNotFoundError] None))
   (let [mod (get sys.modules "hyuga")]
     (when (and mod (hasattr mod "__version__"))
       (getattr mod "__version__")))
   (version-from-pyproject)))

(defn get-package-version [name]
  "Return the version string for the *distribution* called DIST-NAME,
   or nil if it is not installed."
   (if (= name "python")
     sys.version
     (try (libversion name)
       (except [PackageNotFoundError] None))))
