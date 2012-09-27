; Copyright Arthur Edelstein, 2012
; Eclipse Public License 1.0, same as Clojure

(ns clj-inspector.vars
  ^{:doc "Library for inspecting clojure source code."
    :author "Arthur Edelstein"}
 (:import [clojure.lang LineNumberingPushbackReader]
          [java.io BufferedReader File StringReader]
          [java.lang StringBuilder])
 (:use [clojure.pprint :only (pprint)]
       [clj-inspector.jars :only (clj-sources-from-jar
                                  suffix)]))

(def failed-to-process (atom []))

(defonce ns-names (atom nil))

(defn has?
  "If x is in collection, returns x, else nil."
  [coll x]
  (some #{x} coll))

(defn read-clojure-source
  "Takes the text of clojure source code and returns a sequence
   of s-expressions with metadata including the line number and
   the source text."
  [text]
  (let [code-reader (LineNumberingPushbackReader.
                      (StringReader. text))
        lines (vec (.split text "\n"))
        total-lines (count lines)]
    (loop [sexprs []]
      (let [sexpr (try (read code-reader) (catch Exception e nil))
            last-line (.getLineNumber code-reader)]
        (if (and sexpr (instance? clojure.lang.IObj sexpr))
          (let [line (:line (meta sexpr))
                sexpr-m
                (vary-meta
                  sexpr merge
                  {:source (.trim (apply str
                                         (interpose "\n"
                                                    (subvec
                                                      lines
                                                      (dec line)
                                                      (min last-line total-lines)))))})]
            (recur (conj sexprs sexpr-m)))
          sexprs)))))
    
;; namespace: { :full-name :short-name :doc :author :members :subspaces :see-also}
;; vars: {:name :doc :arglists :var-type :file :line :added :deprecated :dynamic}

(defn drop-quotes [sexpr]
  (if (and (seq? sexpr)
           (= (first sexpr) 'quote))
    (drop-quotes (second sexpr))
    sexpr))

(defn get-arg-lists [sexpr]
  (let [tail (drop-while #(or (map? %) (string? %)) (drop 2 sexpr))
        exp1 (first tail)]
    (cond (vector? exp1) (list exp1)
          (list? exp1) (map first tail)
          :else nil)))

(defn arglist-expr-to-str [arglist-expr]
  (when arglist-expr
    (pr-str (drop-quotes arglist-expr))))

(defn get-meta-deflike
  [sexpr]
  (meta (second sexpr)))

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
      (if (not= t1 'defmulti)
        {:arglists (arglist-expr-to-str (get-arg-lists sexpr))}))))

(defn get-meta-tail-doc
  [sexpr n]
  (merge
    (get-meta-deflike sexpr)
    (let [[_ _ t3 t4] sexpr]
      {:doc
        (str (condp = n
          3 t3
          4 t4))})))

(defn analyze-sexpr
  "Analyze the s-expression for docs and metadata."
  [sexpr]
  (when (seq? sexpr)
    (condp has? (first sexpr)
      '(ns def defhinted defonce defstruct)
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

(defn build-expr-info [sexpr]
  (when (seq? sexpr)
    (let [analysis (analyze-sexpr sexpr)]
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

(defn create-var-entries [language sexprs]
  (filter :var-type
          (let [exprs-info (map build-expr-info sexprs)]
            (let [the-ns (first exprs-info)
                  ns-info {:ns (:full-name the-ns)
                           :author (:author the-ns)
                           :ns-doc (:doc the-ns)
                           :language language}]
              (if (has? ['ns 'in-ns] (:expr-type (meta the-ns)))
                (map #(merge ns-info %) (rest exprs-info))
                (throw (Exception. "First element is not a namespace declaration.")))))))

(defn save-names [var-entries]
  (doseq [entry var-entries]
  (swap! ns-names update-in [(:ns entry)] conj (:name entry))))

(defn analyze-clojure-source [language source-text]
  ; Important to turn off the EvalReader
  ; because we are reading untrusted code!!!
  (binding [*read-eval* false] 
    (try
      (doto
        (create-var-entries language (read-clojure-source source-text))
        save-names)
      (catch Throwable e (do (swap! failed-to-process conj [source-text e]) nil)))))

;; parsing the ns form for referred and resolved names

(defn ns-sections [sexpr]
  (apply merge-with concat
         (for [sub-expr sexpr]
           (when (sequential? sub-expr)
             (let [tag (first sub-expr)]
               (when (#{:import :require :use} tag)
                 {tag (rest sub-expr)}))))))

(defn make-name-map [path names]
  (into {}
        (for [name names]
          [(str name) [(str path) (last (.split (str name) "/"))]])))
  
(defn parse-use-section [ns-sections]
  (apply merge
         (for [piece (cons ['clojure.core] (:use ns-sections))]
           (let [ns (str (if (sequential? piece)
                           (first piece)
                           piece))]
             (make-name-map ns
                            (if (and (sequential? piece)
                                     (< 1 (count piece)))
                              (nth piece 2)
                              (get @ns-names ns)))))))

(defn parse-require-section [ns-sections]
  (apply merge
         (for [piece (cons ['clojure.core] (:require ns-sections))]
           (when (sequential? piece)
             ;(println piece)
             (let [ns (str (first piece))
                   prefix (if (< 1 (count piece)) (nth piece 2) ns)
                   names (map #(str prefix "/" %) (get @ns-names ns))]
               (make-name-map ns names))))))

(defn parse-import-section [ns-sections]
  (apply merge
         (for [piece (:import ns-sections)]
           (if (sequential? piece)
             (make-name-map (first piece) (rest piece))))))

(defn parse-ns-form [sexpr]
  (let [ns-sections (ns-sections sexpr)]
    (merge
      (parse-use-section ns-sections)
      (parse-require-section ns-sections)
      (parse-import-section ns-sections))))

;; tests

(def test-file "sample/code_sample.clj")

(defn test-read []
  (read-clojure-source (slurp test-file)))

(defn test-collect []
  (analyze-clojure-source "clj" (slurp test-file)))

(defn test-process []
  (time (map count
             (for [[path code]
                   (clj-sources-from-jar (File. "lib/clojure-1.4.0.jar"))]
               (analyze-clojure-source (suffix path) code)))))

(defn core-code []
  (second (first (clj-sources-from-jar (File. "lib/clojure-1.4.0.jar")))))


