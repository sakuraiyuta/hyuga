(require hyrule * :readers *)
(import hyrule.iterables [butlast drop-last])
(import toolz.itertoolz *)

(import hyuga.log [logger])
(import hyuga.global [$GLOBAL])
(import hyuga.inspect *)

(defn get-details
  [sym-hy]
  "TODO: doc"
  (logger.debug (.format "get-details sym-hy={}" sym-hy))
  ;; TODO: try get info directly if sym not found
  (-> ($GLOBAL.get-$SYMS) (get sym-hy)))

(defn get-candidates
  [prefix]
  "Get all candidates supposed by prefix from all scopes.
  (globals, locals, builtins, and macros)

  Example:
  ```hy
  (get-candidates \"de\")
  => #({\"scope\" \"builtin\"
  \"type\" <class 'builtin_function_or_method'>
  \"sym\" \"delattr\"})
  ```
  "
  (logger.debug
    (.format "get-candidates: $SYMS.count={}"
             (count ($GLOBAL.get-$SYMS))))
  (let [splitted-by-dot (.split prefix ".")
        module-or-class (if (-> splitted-by-dot count (> 1))
                          (->> splitted-by-dot (drop-last 1) (.join "."))
                          "")
        sym-prefix (if module-or-class
                     (last splitted-by-dot)
                     prefix)]
    (logger.debug (.format "module-or-class={}" module-or-class))
    (when module-or-class
      (->> (get-module-attrs splitted-by-dot)
           (map #%(+ [] [(as-> splitted-by-dot it
                           (drop-last 1 it)
                           (list it)
                           (+ it [(first %1)])
                           (.join "." it))
                         (second %1)]))
           filter-add-targets
           (map #%(add-sym! %1 "module"))
           tuple))
    (->> ($GLOBAL.get-$SYMS) .items
         (filter #%(.startswith (first %1) module-or-class))
         (filter #%(.startswith (-> %1 first (.split ".") last) sym-prefix))
         (map #%(get-details (first %1)))
         (map #%(do (.update %1 {"sym" (-> (get %1 "sym") (.split ".") last)})
                    %1))
         tuple)))
