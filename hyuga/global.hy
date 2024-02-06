(require hyrule * :readers *)
(require hyrule.collections [assoc])
(import hyuga.log *)

(import toolz.itertoolz *)
(import importlib.machinery [ModuleSpec])

;; TODO: refactor(remove Global)
(defclass Global [object]
  (defn __init__
    [self]
    (setv self.$SYMS {}))

  (defn add-$SYMS
    [self data]
    ;; f"{scope}\\{ns}\\{sym}" {"sym"
    ;;                          "type" v
    ;;                          "scope" scope
    ;;                          "ns" ns
    ;;                          "pos" pos
    ;;                          "uri" uri
    ;;                          "docs" docs}
    (let+ [{sym "sym" v "type" scope "scope" ns "ns" pos "pos" docs "docs" uri "uri"} data
           key f"{scope}\\{ns}\\{sym}"]
      (logger.debug f"add-$SYMS: key={key}, pos={pos}, uri={uri}")
      (assoc self.$SYMS key data))
    data)

  (defn clean-$SYMS
    [self]
    (setv self.$SYMS {}))

  (defn get-$SYMS
    [self]
    self.$SYMS))

(setv $GLOBAL (Global))
