; Copyright Arthur Edelstein, 2012
; Eclipse Public License 1.0, same as Clojure

(ns clj-inspector.jars
  ^{:doc "Library for reading clojure source files from jars."
    :author "Arthur Edelstein"}
  (:import (java.util.zip ZipFile)
           (java.io BufferedReader File))
  (:use [clojure.java.io :only (reader)]))

(def failed-jars (atom []))

(defn #^String slurp*
  "Like clojure.core/slurp but opens f with reader."
  [f]
  (with-open [^BufferedReader r (reader f)]
             (let [sb (StringBuilder.)]
               (loop [c (.read r)]
                 (if (neg? c)
                   (str sb)
                   (do (.append sb (char c))
                       (recur (.read r))))))))

(defn jar-files
  "List all jar files located in hierarchy under top-folder."
  [top-folder]
  (->> top-folder
       File.
       file-seq
       (filter #(.endsWith (.getAbsolutePath %) ".jar"))))

(defn get-entries-in-jar
  "Get a list of entries in a jar."
  [file]
  (enumeration-seq (.entries (ZipFile. file))))

(defn select-clj-jar-entries
  "Select *.clj files from a list of jar entries."
  [entries]
  (filter #(.endsWith (.getName %) ".clj") entries))

(defn clj-sources-from-jar
  "Read the text of clj source files from a jar file
   and return in a map of path to source text."
  [jar-file]
  (try
    (let [entries (select-clj-jar-entries (get-entries-in-jar jar-file))]
      (into (sorted-map)
            (for [entry entries]
              (let [path (str (.getAbsolutePath jar-file)
                              "!" File/separator
                              (.getName entry))]
                [path
                 (slurp* (.getInputStream (ZipFile. jar-file) entry))]))))
    (catch Exception e 
           (do (swap! failed-jars conj [jar-file e]) nil))))

;; test

(defn first-test []
  (map count (vals (clj-sources-from-jar (File. "lib/clojure-1.3.0.jar")))))

