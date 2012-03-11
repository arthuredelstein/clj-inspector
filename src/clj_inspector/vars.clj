; Copyright Arthur Edelstein, 2012
; Eclipse Public License 1.0, same as Clojure

(ns clj-inspector.vars
  ^{:doc "Library for inspecting clojure source code."
    :author "Arthur Edelstein"}
 (:import [clojure.lang LineNumberingPushbackReader]
          [java.io BufferedReader File StringReader]
          [java.lang StringBuilder])
 (:use [clojure.pprint :only (pprint)]
       [clj-inspector.jars :only (clj-sources-from-jar)]))

(def failed-to-process (atom []))

(defn has?
  "If x is in collection, returns x, else nil."
  [coll x]
  (some #{x} coll))

(defn read-clojure-source
  "Takes the text of clojure source code and returns a sequence
   s-expressions with metadata including the line number, the
   file name and the source text."
  [text]
  (let [code-reader (LineNumberingPushbackReader.
                      (StringReader. (str \newline text)))
        lines (vec (.split text "\n"))
        total-lines (count lines)]
    (loop [sexprs [] line 0]
      (let [sexpr (try (read code-reader) (catch Exception e nil))
            next-line (.getLineNumber code-reader)]
        (if (and sexpr (instance? clojure.lang.IObj sexpr))
          (let [sexpr-m
                (with-meta
                  sexpr
                  {:line (inc line)
                   :source (.trim (apply str
                                  (interpose "\n"
                                             (subvec
                                               lines
                                               line
                                               (min next-line total-lines)))))})]
            (recur (conj sexprs sexpr-m) next-line))
          sexprs)))))
    
;; namespace: { :full-name :short-name :doc :author :members :subspaces :see-also}
;; vars: {:name :doc :arglists :var-type :file :line :added :deprecated :dynamic}

(defn get-meta-deflike
  [sexpr]
  (meta (second sexpr)))

(defn get-arg-lists [sexpr]
  (let [tail (drop-while #(or (map? %) (string? %)) (drop 2 sexpr))
        exp1 (first tail)]
    (cond (vector? exp1) (list exp1)
          (list? exp1) (map first tail))))

(defn get-meta-defnlike
  [sexpr]
  (let [[t1 t2 t3 t4] sexpr
        d (if (string? t3) t3)]
    ;(println t2 t3 t4)
    (merge
      (meta t2)
      (if (map? t3) t3)
      (if (and d (map? t4)) t4)
      (if d {:doc d})
      (if (not= t1 'defmulti) {:arglists (get-arg-lists sexpr)}))))

(defn get-meta-tail-doc
  [sexpr n]
  (merge
    (get-meta-deflike sexpr)
    (let [[_ _ t3 t4] sexpr]
      {:doc
        (str (condp = n
          3 t3
          4 t4))})))

;; TODO: 'ns (namespaces)
(defn analyze-sexpr
  "Analyze the s-expression for docs and metadata."
  [sexpr]
  (when (seq? sexpr)
    (condp has? (first sexpr)
      '(ns)
      (get-meta-deflike sexpr)
      '(def defhinted defonce defstruct)
      (get-meta-deflike sexpr)
      '(defn definline defmacro defmulti defn-memo defnk)
      (get-meta-defnlike sexpr)
      '(defprotocol defunbound)
      (get-meta-tail-doc sexpr 3)
      '(defalias defvar)
      (get-meta-tail-doc sexpr 4)
      nil)))

(defn get-var-type [sexpr]
    ({'def       "var"
      'defn      "function"
      'definline "function"
      'defmacro  "macro"
      'defmulti  "multimethod"
      'defnmemo  "function"
      'defnk     "function"}
     (first sexpr)))

(defn drop-quotes [sexpr]
  (if (and (seq? sexpr)
           (= (first sexpr) 'quote))
    (drop-quotes (second sexpr))
    sexpr))

(defn arglist-as-str [expr-info-map]
  (try
  (update-in expr-info-map [:arglists]
             #(when % (-> % drop-quotes pr-str)))
    (catch Throwable e (do 
                         (def x expr-info-map)
                         (prn "arglist-as-str" expr-info-map e)
                         (throw e)))))

(defn build-expr-info [sexpr]
  (when (seq? sexpr)
    (let [analysis (arglist-as-str (analyze-sexpr sexpr))]
      (with-meta
        (if (has? ['ns 'in-ns] (first sexpr))
          (merge
            (select-keys analysis [:doc :author :subspaces :see-also])
            {:full-name (name (drop-quotes (second sexpr)))
             :short-name (name (drop-quotes (second sexpr)))})
          (merge
            (meta sexpr)
            (select-keys analysis [:arglists :doc :added :deprecated :dynamic])
            {:name (try (name (second sexpr)) (catch Exception e nil))
             :var-type (get-var-type sexpr)}))
        {:expr-type (first sexpr)}))))

(defn create-var-entries [sexprs]
  (filter :var-type
          (let [exprs-info (map build-expr-info sexprs)]
            (let [the-ns (first exprs-info)
                  ns-info {:ns (:full-name the-ns)
                           :author (:author the-ns)
                           :ns-doc (:doc the-ns)}]
              (if (has? ['ns 'in-ns] (:expr-type (meta the-ns)))
                (map #(merge ns-info %) (rest exprs-info))
                (throw (Exception. "First element is not a namespace declaration.")))))))

(defn analyze-clojure-source [source-text]
  ; Important to turn off the EvalReader
  ; because we are reading untrusted code!!!
  (binding [*read-eval* false] 
    (try
      (create-var-entries (read-clojure-source source-text)))
      (catch Throwable e (do (swap! failed-to-process conj [source-text e]) nil))))

;; tests

(def test-file "sample/code_sample.clj")

(defn test-read []
  (read-clojure-source (slurp test-file)))

(defn test-collect []
  (analyze-clojure-source (slurp test-file)))

(defn test-process []
  (time (map count
             (map analyze-clojure-source
                  (map second
                       (clj-sources-from-jar (File. "lib/clojure-1.3.0.jar")))))))

(defn core-code []
  (second (first (clj-sources-from-jar (File. "lib/clojure-1.3.0.jar")))))
  

