(ns visugit.git
  (:require [visugit.shell-out :as shell]
            [org.satta.glob :as glob]
            [clojure.java.io :as jio]
            [clojure.string :as cstr]
            [clojure.contrib.string :as ccstr])
  (:import [java.io File]))

(defonce dir (ref nil))

(defn heads []
  (glob/glob (str @dir ".git/refs/heads/*")))

(defn get-branches []
  (let [branch-files (glob/glob (str @dir ".git/refs/heads/*"))]
    (reduce conj {} (map (fn [^File f]
                           [(.getName f) (cstr/trim-newline (slurp f))])
                         branch-files))))

(defn simple-name [^String ref-name]
  (let [[fs & res] (.split ref-name "/")]
    (cond
     (= fs "HEAD") fs
     (= fs "tags") ref-name
     :else (apply str (interpose "/" (rest res))))))

(defn get-refs []
  (into {} (for [^String line (-> (shell/sh "git" "show-ref" "--head" {:dir @dir})
                                  deref
                                  :out
                                  cstr/split-lines)]
             (let [[sha1 nam] (.split line "\\s+")]
               [(simple-name nam) sha1]))))

(defn get-type [obj-id]
  (-> (shell/sh "git" "cat-file" "-t" obj-id {:dir @dir})
      deref :out cstr/trim-newline))

(defn get-type-batch [obj-ids]
  (let [m (into {}
                (map (fn [^String line]
                       (let [[id type] (.split line "\\s")]
                         [id (keyword type)]))
                     (-> (shell/sh "git" "cat-file" "--batch-check"
                                   {:dir @dir, :encode "utf-8",
                                    :in (apply str (interleave obj-ids (repeat \newline)))})
                         deref :out cstr/split-lines)))]
    (group-by m obj-ids)))

(defn get-tree-id [line]
  (let [[_ id] (re-find #"tree (.*)$" line)]
    id))
;; (get-tree-id "tree 47975e7102c8933c2b0ddf62bc9f92055a50167e")
;; "47975e7102c8933c2b0ddf62bc9f92055a50167e"

(defn get-parent-ids [lines]
  (map (fn [line] (second (re-find #"parent (.*)$" line)))
       (take-while (fn [^String line] (.startsWith line "parent")) lines)))

(defn commit-obj [obj-id]
  (let [[tree-line & lines] (-> (shell/sh "git" "cat-file" "-p" obj-id {:dir @dir})
                                deref :out cstr/split-lines)
        tree-id (get-tree-id tree-line)
        parent-ids (get-parent-ids lines)]
    {:tree tree-id, :parents parent-ids :id obj-id}))

(defn commit-map* [obj-id-queue acc]
  (if-let [id (first obj-id-queue)]
    (if (acc id)
      (recur (subvec obj-id-queue 1) acc)
      (let [co (commit-obj id)
            added-acc (assoc acc id co)]
        (recur (into (subvec obj-id-queue 1) (:parents co))
               added-acc)))
    acc))

(defn commit-map [refs]
  (-> refs vals get-type-batch :commit (commit-map* {})))

(defn tree-obj [obj-id]
  (let [lines (-> (shell/sh "git" "cat-file" "-p" obj-id {:dir @dir})
                  deref :out cstr/split-lines)]
    (into {}
          (map (fn [^String line]
                 (let [[typenum type id file] (.split line "\\s+")]
                   [id {:type type :file file}]))
               lines))))
;;(catch Exception e (.printStackTrace e))))

(defn tree? [{type :type}]
  (= "tree" type))

(defn tree-map [obj-id-queue acc]
  (if-let [id (first obj-id-queue)]
    (if (acc id)
      (recur (subvec obj-id-queue 1) acc)
      (let [t (tree-obj id)
            added-acc (assoc acc id t)]
        (recur (into (subvec obj-id-queue 1)
                     (keys (filter #(-> % val tree?) t)))
               added-acc)))
    acc))

(defn tree-map-by-comits [commit-map]
  (tree-map (into [] (map :tree (vals commit-map))) {}))

;;(defn commit-map [acc commit-objs]
;;  (

;;(get-type "67025a4d86c6fc87c4c5c8f9ce38c22b3019d79a" "../clj-http/")
;;"commit"

