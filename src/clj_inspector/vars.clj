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

(defn ns-sections [sexpr]
  (apply merge-with concat
         (for [sub-expr sexpr]
           (when (sequential? sub-expr)
             (let [tag (first sub-expr)]
               (when (#{:import :require :use} tag)
                 {tag (rest sub-expr)}))))))

(defn concat-or-all [& args]
  (if ((set args) :all)
    :all
    (apply concat args)))

(defn scrape-ns-parts [clause mapping-fn]
  (apply merge-with concat-or-all
         (map mapping-fn clause)))
  
(defn analyze-use-section [ns-sections]
  (scrape-ns-parts (:use ns-sections)
                   (fn [piece]
                     (if (sequential? piece)
                       {(first piece)
                        (if (rest piece)
                          (nth piece 2)
                          :all)}
                       {piece :all}))))
             
(defn analyze-require-section [ns-sections]
  (scrape-ns-parts (:require ns-sections)
                   (fn [piece]
                     (if (sequential? piece)
                       {(first piece) [(nth piece 2)]}
                       {piece nil}))))

(defn analyze-import-section [ns-sections]
  (scrape-ns-parts (:import ns-sections)
                   (fn [piece]
                     (if (sequential? piece)
                       {(first piece) (rest piece)}
                       nil))))

(defn analyze-ns-form [sexpr]
  (let [ns-sections (ns-sections sexpr)]
    (merge (get-meta-deflike sexpr)
           {:use (analyze-use-section ns-sections)
            :require (analyze-require-section ns-sections)
            :import (analyze-import-section ns-sections)})))

(defn analyze-sexpr
  "Analyze the s-expression for docs and metadata."
  [sexpr]
  (when (seq? sexpr)
    (condp has? (first sexpr)
      '(ns)
      (analyze-ns-form sexpr)
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
  

