; Copyright Arthur Edelstein, 2012
; Eclipse Public License 1.0, same as Clojure

(ns clj-inspector.jars
  ^{:doc "Library for reading clojure source files from jars."
    :author "Arthur Edelstein"}
  (:import (java.util.zip ZipFile)
           (java.io BufferedReader File))
  (:require [clojure.zip :as zip]
            [clojure.xml :as xml])
  (:use [clojure.java.io :only (reader)]
        [clojure.data.zip.xml :only (attr text xml->)]
        [clojure.data.zip.xml :only [xml-> xml1-> text]]))

(def failed-jars (atom []))

(defn suffix [path]
  (re-find #"[^\.]+\z" (.trim path)))

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

(defn slurp-entry [jar-file entry]
  (slurp* (.getInputStream (ZipFile. jar-file) entry)))

(defn jar-pom-xml [jar-file]
  "Get the pom.xml file from a jar, if it exists."
  (->> jar-file
       get-entries-in-jar
       (filter #(.endsWith (.getName %) "pom.xml"))
       first
       (.getInputStream (ZipFile. jar-file))
       xml/parse))
       
;; scavenged from ClojureSphere (and stripped down)
(defn parse-pom-xml
  "Get information from the xml structure generated from a POM file."
  [xml]
  (let [z (zip/xml-zip xml)
        group-id (xml1-> z :groupId text)
        artifact-id (xml1-> z :artifactId text)
        name (if (and group-id (not= group-id artifact-id))
                (str group-id "/" artifact-id)
                artifact-id)
        version (xml1-> z :version text)]
    {:group-id group-id
     :artifact-id artifact-id
     :name name
     :version version
     :description (xml1-> z :description text)
     :url (xml1-> z :url text)
     :lein-specifier (str "[" name " \"" version "\"]")}))
  
(defn jar-pom-info 
  "Get the POM information from a jar-file"
  [jar-file]
  (parse-pom-xml (jar-pom-xml jar-file)))

(defn select-clj-jar-entries
  "Select *.clj files from a list of jar entries."
  [entries]
  (filter #(#{"clj" "cljs"} (suffix (.getName %))) entries))

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
                 (slurp-entry jar-file entry)]))))
    (catch Exception e 
           (do (swap! failed-jars conj [jar-file e]) nil))))

;; test

(defn first-test []
  (map count (vals (clj-sources-from-jar (File. "lib/clojure-1.4.0.jar")))))

