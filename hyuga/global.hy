(defclass Global [object]
  (defn __init__
    [self]
    (setv self.$SYMS {})
    (setv self.$SCOPES {"global" {}
                        "local" {}
                        "builtin" {}
                        "macro" {}}))
  (defn set-$SYMS
    [self v]
    (setv self.$SYMS v))
  (defn get-$SYMS
    [self]
    self.$SYMS)
  (defn set-$SCOPES
    [self v]
    (setv self.$SCOPES v))
  (defn get-$SCOPES
    [self]
    self.$SCOPES))

(setv $GLOBAL (Global))
