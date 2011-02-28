(ns visugit.git
  (:require [visugit.shell-out :as shell]
            [org.satta.glob :as glob]
            [clojure.java.io :as jio]
            [clojure.string :as cstr]
            [clojure.contrib.string :as ccstr])
  (:import [java.io File]
           [java.util Date]))

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
     (= sec "stash") sec
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

(defn commit-obj [^String obj-line acc]
  (let [[hash parents tree date author subject]
        (.split obj-line "::::")]
    (if-let [co (acc hash)]
      co
      (let [parents (seq (.split ^String parents " "))
            parents (if (= parents '("")) () parents)]
        {:id hash :parents parents :tree tree :date (-> date read-string long Date.)
         :author author :subject subject}))))

(defn commit-map [start acc]
  (reduce (fn [m co] (assoc m (:id co) co)) {}
          (map #(commit-obj % acc)
               (-> (shell/sh "git" "log" "--format=%H::::%P::::%T::::%at::::%an::::%s"
                             start {:dir @dir})
                   deref :out cstr/split-lines))))

(defn run-update-commit-map [starts digged-commits-ref total-commits]
  (doseq [start starts]
    (let [commits (commit-map start (merge @digged-commits-ref total-commits))]
      (dosync (alter digged-commits-ref merge commits)))))

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

