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
  (let [[fs sec thir four] (.split ref-name "/")]
    (cond
     (= fs "HEAD") fs
     (= sec "tags") (str sec "/" thir)
     (= sec "heads") thir
     (= sec "remotes") (str thir "/" four))))

(defn get-head-ref []
  (let [^String line (-> (shell/sh "git" "show-ref" "--head" {:dir @dir})
                         deref
                         :out
                         cstr/split-lines
                         first)
        [sha1 nam] (.split line "\\s+")]
    [nam {:name nam, :type "commit" :id sha1}]))

(defn get-refs []
  (into {}
        (cons (get-head-ref)
              (for [^String line (-> (shell/sh "git" "for-each-ref" {:dir @dir})
                                     deref
                                     :out
                                     cstr/split-lines)]
                (let [[sha1 type nam] (.split line "\\s+")
                      n (simple-name nam)]
                  [n {:name n, :id sha1, :type type}])))))

(defn run-update-refs [refs-ref polling-ms]
  (loop []
    (let [new-refs (get-refs)]
      (dosync (ref-set refs-ref new-refs))
      (Thread/sleep polling-ms)
      (recur))))

(defn is-type-commit? [[_ r]]
  (= (:type r) "commit"))

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

(defn commit-map* [queue acc]
  (if-let [id (first queue)]
    (if (acc id)
      (recur (subvec queue 1) acc)
      (let [co (commit-obj id)
            new-queue (into (subvec queue 1) (:parents co))]
        {:queue new-queue, :commit-entry [id co]}))
    nil))

(defn run-update-commit-map [refs commits-ref stop-switch-ref polling-ms]
  (let [initial-commits (vec (map :id (vals (filter is-type-commit? refs))))]
    (loop [queue initial-commits
           acc @commits-ref]
      (if @stop-switch-ref
        (do (Thread/sleep polling-ms)
            (recur queue acc))
        (if-let [{:keys [queue commit-entry]} (commit-map* queue acc)]
          (do (dosync (alter commits-ref conj commit-entry))
              (recur queue @commits-ref))
          :finish)))))

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

(defn get-staged-files []
  (let [output (-> (shell/sh "git" "diff" "--cached" "--name-only" {:dir @dir})
                  deref :out)]
    (if output
      (cstr/split-lines output)
      [])))

(defn get-modified-files []
  (if-let [out (-> (shell/sh "git" "ls-files" "-m" {:dir @dir})
                   deref :out)]
    (cstr/split-lines out)
    []))

(defn get-untracked-files []
  (let [resp (-> (shell/sh "git" "ls-files" "-o" "--exclude-from=.gitignore" {:dir @dir})
                 deref)]
    (if (= 128 (:exit resp))
      (if-let [out (-> (shell/sh "git" "ls-files" "-o" {:dir @dir})
                       deref :out)]
        (cstr/split-lines out)
        [])
      (if-let [out (:out resp)]
        (cstr/split-lines out)
        []))))

