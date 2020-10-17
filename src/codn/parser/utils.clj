(ns codn.parser.utils
  (:refer-clojure :exclude [char]))

(defn char [x]
  (when x
    (clojure.core/char x)))

(defn ex-info? [ex]
  (instance? clojure.lang.ExceptionInfo ex))

(defn whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (when ch
    (or (Character/isWhitespace ^Character ch)
        (identical? \,  ch))))

(defn numeric?
  "Checks whether a given character is numeric"
  [^Character ch]
  (when ch
    (Character/isDigit ch)))

(defn comment-prefix?
  "Checks whether the character begins a comment."
  [ch]
  (identical? \;  ch))

(defn newline?
  "Checks whether the character is a newline"
  [c]
  (or (identical? \newline c)
      (nil? c)))

(defn desugar-meta
  "Resolves syntactical sugar in metadata" ;; could be combined with some other desugar?
  [f]
  (cond
    (keyword? f) {f true}
    (symbol? f)  {:tag f}
    (string? f)  {:tag f}
    :else        f))

(defn make-var
  "Returns an anonymous unbound Var"
  []
  (with-local-vars [x nil] x))

(defn namespace-keys [ns keys]
  (for [key keys]
    (if (or (symbol? key)
            (keyword? key))
      (let [[key-ns key-name] ((juxt namespace name) key)
            ->key (if (symbol? key) symbol keyword)]
        (cond
          (nil? key-ns)
          (->key ns key-name)

          (= "_" key-ns)
          (->key key-name)

          :else
          key))
      key)))

(defn second' [[a b]]
  (when-not a b))
