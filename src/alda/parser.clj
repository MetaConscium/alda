(ns alda.parser
  (:require [instaparse.core :as insta]
            [clojure.string  :as str]
            [clojure.java.io :as io]
            [alda.util       :as util]
            [taoensso.timbre :as log]))

; sets log level to TIMBRE_LEVEL (if set) or :warn
(util/set-timbre-level!)

(declare alda-parser parse-input)
(require '[alda.lisp :as lisp])

(defn- parser-from-grammar
  [grammar]
  (insta/parser (io/resource (str grammar ".bnf"))))

(defn- parser-from-grammars
  "Builds a parser from any number of BNF grammars, concatenated together."
  [& grammars]
  (insta/parser (str/join \newline
                          (map #(slurp (io/resource (str % ".bnf"))) grammars))))

(def ^:private alda-parser    (parser-from-grammar  "alda"))

(def ^:private comment-parser (parser-from-grammar  "comments"))

(def ^:private score-parser   (parser-from-grammars "score"
                                                    "names"
                                                    "ows"))

(def ^:private header-parser  (parser-from-grammars "header"
                                                    "clojure"
                                                    "ows"))

(def ^:private group-parser   (parser-from-grammars "groups"
                                                    "clojure"
                                                    "voices"
                                                    "event-sequence"
                                                    "cram"
                                                    "duration"
                                                    "barline"
                                                    "numbers"
                                                    "music-data"
                                                    "ows"))

(def ^:private event-parser   (parser-from-grammars "events"
                                                    "clojure"
                                                    "voices"
                                                    "event-sequence"
                                                    "cram"
                                                    "duration"
                                                    "barline"
                                                    "names"
                                                    "numbers"
                                                    "music-data"
                                                    "ows"))

(defn- read-clj-expr
  "Reads an inline Clojure expression within Alda code.

   This expression will be evaluated within the `boot.user` context, which has
   the vars in `alda.lisp` referred in.

   Returns ready-to-evaluate Clojure code."
  [expr]
  (read-string (str \( (apply str expr) \))))

(def ^:private number-transforms
  {:positive-number #(Integer/parseInt %)
   :negative-number #(Integer/parseInt %)
   :voice-number    #(Integer/parseInt %)})

(def ^:private name-transforms
  {:name     #(hash-map :name %)
   :nickname #(hash-map :nickname %)})

(def ^:private clj-expr-transforms
  {:clj-character #(str \\ %)
   :clj-string    #(str \" (apply str %&) \")
   :clj-expr      #(read-clj-expr %&)})

(defn parse-tree
  "Returns the intermediate parse tree resulting from parsing a string of Alda
   code."
  [alda-code]
  (alda-parser alda-code))

(defn- check-for-failure
  "Determines whether its input is an Instaparse failure, throwing an exception
   if it is. If it isn't, passes it through so we can continue parsing."
  [x]
  (if (insta/failure? x)
    (throw (Exception. (pr-str x)))
    x))

(defn- remove-comments
  "Strips comments from a string of Alda code."
  [input]
  (->> input
       comment-parser
       check-for-failure
       (insta/transform {:score str})))

(defn- separate-parts
  "Separates out instrument parts (including subsequent calls to existing
   parts)."
  [input]
  (->> input
       score-parser
       check-for-failure
       (insta/transform
         (merge name-transforms
                {:calls (fn [& calls]
                          (let [names    (vec (keep :name calls))
                                nickname (some :nickname calls)]
                            (if nickname
                              {:names names, :nickname nickname}
                              {:names names})))}))))

(defn- parse-header
  "Parses the (optional) string of non-instrument-specific events that may
   occur at the beginning of an Alda score (e.g. setting variables, global
   attributes, inline Clojure code)."
  [header]
  (->> header
       header-parser
       check-for-failure
       (insta/transform
         (merge clj-expr-transforms
                {:header #(list* %&)}))))

(defn- parse-events
  [events]
  (->> events
       event-parser
       check-for-failure
       (insta/transform
         (merge name-transforms
                number-transforms
                clj-expr-transforms
                {:events          #(list* %&)
                 :repeat          (fn [event n]
                                    (list 'alda.lisp/times n event))
                 :tie             (constantly :tie)
                 :slur            (constantly :slur)
                 :flat            (constantly :flat)
                 :sharp           (constantly :sharp)
                 :natural         (constantly :natural)
                 :dots            #(hash-map :dots (count %))
                 :note-length     #(list* 'alda.lisp/note-length %&)
                 :milliseconds    #(list 'alda.lisp/ms %)
                 :seconds         #(list 'alda.lisp/ms (* % 1000))
                 :duration        #(list* 'alda.lisp/duration %&)
                 :pitch           (fn [letter & accidentals]
                                    (list* 'alda.lisp/pitch
                                           (keyword letter)
                                           accidentals))
                 :note            #(list* 'alda.lisp/note %&)
                 :rest            #(list* 'alda.lisp/pause %&)
                 :chord           #(list* 'alda.lisp/chord %&)
                 :octave-set      #(list 'alda.lisp/octave %)
                 :octave-up       #(list 'alda.lisp/octave :up)
                 :octave-down     #(list 'alda.lisp/octave :down)
                 :marker          #(list 'alda.lisp/marker (:name %))
                 :at-marker       #(list 'alda.lisp/at-marker (:name %))
                 :barline         #(list 'alda.lisp/barline)}))))

(defn- parse-groupings
  "Separates an instrument's music data into:
     - voice groups
     - inline Clojure expressions
     - event sequences
     - CRAMs
     - variable definitions
     - all other events"
  [part-data]
  (->> part-data
       group-parser
       check-for-failure
       (insta/transform
         (merge number-transforms
                clj-expr-transforms
                {:part            #(->> %&
                                        (partition-by string?)
                                        (map (fn [[x & xs :as thing]]
                                               (if (string? x)
                                                 (parse-events (apply str thing))
                                                 thing)))
                                        (apply concat))
                 :event-sequence  #(list* 'do
                                          (parse-events (apply str %&)))
                 :cram            #(list* 'alda.lisp/cram
                                          (parse-events (apply str %&)))
                 :voice           (fn [voice-number & music-data]
                                    (list* 'alda.lisp/voice
                                           voice-number
                                           (parse-events (apply str music-data))))
                 :voices          #(list* 'alda.lisp/voices %&)}))))

(defn- parse-part
  "Parses an instrument part into a list of Clojure expressions."
  [part]
  (->> part
       parse-groupings))

(defn parse-input
  "Parses a string of Alda code and turns it into Clojure code."
  [alda-code]
  (->> alda-code
       remove-comments
       separate-parts
       (insta/transform
         {:score  #(apply concat '(alda.lisp/score) %&)
          :header (comp parse-header str)
          :part   (fn [names & music-data]
                    (list
                      (list* 'alda.lisp/part
                             names
                             (parse-part (apply str music-data)))))})))

