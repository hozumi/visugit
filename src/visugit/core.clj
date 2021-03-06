(ns visugit.core
  (:use [rosado.processing])

  (:require [visugit.shell-out :as shell]
            [visugit.git :as git]
            [org.satta.glob :as glob]
            [clojure.java.io :as jio]
            [clojure.string :as cstr]
            [clojure.set :as set]
            [clojure.contrib.string :as ccstr]
            [clojure.contrib.seq-utils :as seq-utils]
            [clojure.contrib.combinatorics :as comb]
            [rosado.processing.applet :as proapp])
  (:import [toxi.physics2d VerletMinDistanceSpring2D
            VerletConstrainedSpring2D
            VerletSpring2D
            VerletPhysics2D
            VerletParticle2D]
           [toxi.geom Rect Vec2D]))

(defonce refs (ref {}))
(defonce dug-refs (ref {}))
(defonce tags (ref {}))
(defonce commits (ref {}))
(defonce dug-commits (ref {}))
(defonce search-commit-queue (ref []))
(defonce parent->child-commits (ref {}))
(defonce id->particle-map (ref {}))
(defonce springs (ref {}))
(defonce ^VerletPhysics2D physics (VerletPhysics2D.))

(defonce staged-files (ref []))
(defonce modified-files (ref []))
(defonce untracked-files (ref []))

(def text-height 20)
(def row-max 10)

(defn draw-column-word [words base-x base-y]
  (text-align LEFT)
  (doseq [[i part] (seq-utils/indexed (partition-all row-max words))]
    (doseq [[n s] (seq-utils/indexed part)]
      (string->text s
                    (+ base-x (* 100 i))
                    (+ base-y (* text-height n))))))

(defn draw-stage&wt []
  (let [staged-y (+ (/ (height) 2) 20)
        modified-y (+ staged-y (* text-height (min row-max (max 1 (count @staged-files)))))
        untracked-y (+ modified-y (* text-height (min row-max (max 1 (count @modified-files)))))]
    (fill 0)
    (text-align LEFT)
    (string->text "staged files" 10 staged-y)
    (fill 50 200 50)
    (draw-column-word @staged-files 100 staged-y)
    (fill 0)
    (string->text "modified files" 10 modified-y)
    (fill 250 50 50)
    (draw-column-word @modified-files 100 modified-y)
    (fill 0)
    (string->text "untracked files" 10 untracked-y)
    (fill 50 50 50)
    (draw-column-word @untracked-files 100 untracked-y)
    ))

(defn update-stage&wt [polling-ms]
  (loop []
    (let [staged (git/get-staged-files)
          modified (git/get-modified-files)
          untracked (git/get-untracked-files)]
      (dosync
       (ref-set staged-files staged)
       (ref-set modified-files modified)
       (ref-set untracked-files untracked))
      (Thread/sleep polling-ms)
      (recur))))

(defn create-particle
  ([id]
     (create-particle id (rand-int 1000) (rand-int 1000)))
  ([id parent-ids]
     (if-let [^VerletParticle2D parent-p (first (keep @id->particle-map parent-ids))]
       (create-particle id (.x parent-p) (.y parent-p))
       (create-particle id)))
  ([id x y]
     (let [p (VerletParticle2D. (float x)
                                (float y))]
       (dosync
        (alter id->particle-map assoc id p))
       (.addParticle physics p)
       p)))

(defn create-springs [{:keys [constrained min]} me-id other-ids]
  ;;(println :create-springs constrained me-id other-ids)
  (let [me-p (@id->particle-map me-id)]
    (doseq [other-id (filter #(not= me-id %) other-ids)]
      (when-let [other-p (@id->particle-map other-id)]
        (when constrained
          (let [k #{me-id other-id :constrained}]
            (when-not (@springs k)
              (let [s (VerletConstrainedSpring2D. me-p other-p (:len constrained) (:str constrained))]
                (dosync (alter springs assoc k s))
                (.addSpring physics s)))))
        (when min
          (let [k #{me-id other-id :min}]
            (when-not (@springs k)
              (let [s (VerletMinDistanceSpring2D. me-p other-p (:len min) (:str min))]
                (dosync (alter springs assoc k s))
                (.addSpring physics s)))))))))

(defn add-commit [{:keys [id parents] :as co}]
  (create-particle id parents)
  (dosync
   (doseq [parent-id parents]
     (alter parent->child-commits update-in [parent-id]
            (fn [child-ids] (if child-ids (conj child-ids id) [id])))))
  ;;attraction to parents
  (create-springs {:constrained {:len 30 :str 0.01}} id parents)
  ;;attraction to childs
  (create-springs {:constrained {:len 30 :str 0.01}} id (@parent->child-commits id))
  ;;repulsion
  (create-springs {:min {:len 50 :str 0.001}} id (keys @commits))
  ;;attraction to corresponding refs
  (create-springs {:constrained {:len 5 :str 0.001}}
                  id (map :name (filter (fn [r] (= id (:id r))) (vals @refs))))
  ;;attraction to corresponding tags
  (create-springs {:constrained {:len 5 :str 0.001}}
                  id (map :name (filter (fn [t] (= id (:refer t))) (vals @tags)))))

(defn update-commits []
  (when-not (empty? @dug-commits)
    (let [cache (ref nil)]
      (dosync
       (alter commits merge @dug-commits)
       (ref-set cache @dug-commits)
       (ref-set dug-commits {}))
      (doseq [co (vals @cache)]
        (add-commit co)))))

(defn digging-commits [polling-ms]
  (loop []
    (when-not (empty? @search-commit-queue)
      (let [fs (first @search-commit-queue)
            new-commits (git/get-commit-map-diff fs (merge @dug-commits @commits))]
        (dosync (alter search-commit-queue subvec 1)
                (alter dug-commits merge new-commits))))
    (Thread/sleep polling-ms)
    (recur)))

(defn bind-commit-or-background-search [{:keys [name id]}]
  (if (@id->particle-map id)
      (create-springs {:constrained {:len 5 :str 0.001}}
                      name [id])
      (dosync (alter search-commit-queue conj id))))

(defn add-tag [{:keys [id name] :as r}]
  (let [t-name (str "tag:" name)
        t (assoc r :name t-name :refer (git/get-tag-refer id))]
    (dosync (alter tags conj [t-name t]))
    (create-particle t-name)
    (create-springs {:constrained {:len 5 :str 0.05}, :min {:len 20 :str 0.001}}
                    t-name [name])
    ;;attraction to corresponding commit
    (bind-commit-or-background-search t)))

(defn add-ref [{:keys [id name type] :as r}]
  (let [label-id (str "label:" name)]
    (create-particle name (:id r))
    (create-particle label-id (:id r))
    (create-springs {:constrained {:len 5 :str 0.05}, :min {:len 20 :str 0.001}}
                    name [label-id])
    ;;label - label repulsion
    (create-springs {:min {:len 25 :str 0.6}}
                    name (map #(str "label:" %)
                              (keys (filter git/is-type-commit? @refs))))
    (if (= type "tag")
      (add-tag r)
      ;;attraction to corresponding commit
      (bind-commit-or-background-search r))))

(defn remove-particle&springs [id]
  (let [p (@id->particle-map id)
        my-spring-keys (filter (fn [s] (s id))
                               (keys @springs))
        my-springs (map @springs my-spring-keys)]
    (dosync
     (alter id->particle-map dissoc id)
     (alter springs #(apply dissoc % my-spring-keys)))
    (.removeParticle physics p)
    (doseq [s my-springs]
      (.removeSpring physics s))))

(defn remove-spring [k]
  (when-let [sp (@springs k)]
    (.removeSpring physics sp)
    (dosync
     (alter springs dissoc k))))

(defn modify-ref [old-ref new-ref]
  (remove-spring #{(:name old-ref) (:id old-ref) :constrained})
  (bind-commit-or-background-search new-ref))

(defn delete-ref [{:keys [type name id]}]
  (remove-particle&springs id)
  (remove-particle&springs (str "label:" id))
  (when (= "tag" type)
    (remove-particle&springs (str "tag:" name))))

(defn update-refs []
  (when (not= @refs @dug-refs)
    (let [refs-set (-> @refs keys set)
          dug-refs-set (-> @dug-refs keys set)
          removed-ref-ids (set/difference refs-set dug-refs-set)
          removed-refs (map @refs removed-ref-ids)
          added-ref-ids (set/difference dug-refs-set refs-set)
          modified-ids (filter (fn [k] (not= (@refs k) (@dug-refs k)))
                               (set/intersection refs-set dug-refs-set))]
      (doall (map modify-ref (map @refs modified-ids) (map @dug-refs modified-ids)))
      (dosync
       (ref-set refs @dug-refs))
      (doseq [{:keys [id] :as r} (map @refs added-ref-ids)]
        (add-ref r))
      (doall (map delete-ref removed-refs)))))

(defn digging-refs [polling-ms]
  (loop []
    (dosync (ref-set dug-refs (git/get-refs)))
    (Thread/sleep polling-ms)
    (recur)))

(defn draw-tags []
  (doseq [[k] @tags]
    (let [^VerletParticle2D p (@id->particle-map k)]
      (fill-float 255 255 20 200)
      (ellipse (.x p) (.y p) 10 10))))

(defn draw-refs []
  (doseq [[^String k] @refs];;(filter @id->particle-map (keys @refs))]
    (let [^VerletParticle2D p (@id->particle-map k)]
      (cond
       (= "HEAD" k) (fill-float 250 50 50 150)
       (.startsWith k "tags/") (fill-float 220 220 50 150)
       (.contains k "/") (fill-float 0 250 250 150)
       :else (fill-float 30 255 30 200))
      (ellipse (.x p) (.y p) 10 10))
    (let [^VerletParticle2D p (@id->particle-map (str "label:" k))]
      (text-align CENTER)
      (fill 0)
      (string->text k (.x p) (.y p)))))

(defn draw-commit []
  (fill-float 100 100 250 250)
  (doseq [entr @commits]
    (let [^VerletParticle2D p (@id->particle-map (key entr))]
      (ellipse (.x p) (.y p) 10 10)
      ;;(text-align CENTER)
      ;;(string->text (key entr) (.x p) (+ (.y p) 20))
      ))
  (doseq [[& [me you]] (map #(disj % :constrained) (filter :constrained (keys @springs)))]
    (let [^VerletParticle2D me-p (@id->particle-map me)
          ^VerletParticle2D you-p (@id->particle-map you)]
      (line (.x me-p) (.y me-p) (.x you-p) (.y you-p))))
  #_(doseq [co (vals @commits)
          parent-id (:parents co)]
    (when-let [^VerletParticle2D parent-p (@id->particle-map parent-id)]
      (let [^VerletParticle2D me-p (@id->particle-map (:id co))]
        (line (.x me-p) (.y me-p) (.x parent-p) (.y parent-p))))))

(defn draw []
  (size (width) (height))
  (.setWorldBounds physics
                   (Rect. (Vec2D. 10 10)
                          (Vec2D. (- (width) 10) (- (/ (height) 2) 10))))
  (background-int 220)
  (.update physics)
  ;;(framerate 1)
  (stroke-float 150)
  (color-mode RGB)
  (fill-float 50 50 200 250)
  (update-commits)
  (update-refs)
  (draw-commit)
  (draw-tags)
  (draw-refs)
  (draw-stage&wt))

(defn setup []
  (let [f (create-font "Arial" 11 true)]
    (text-font f))
  (.setWorldBounds physics
                   (Rect. (Vec2D. 20 10)
                          (Vec2D. (- (width) 20) (- (/ (height) 2) 10))))
  (dosync (ref-set dug-refs (git/get-refs)))
  (update-refs)
  (future (update-stage&wt 1000))
  (future (digging-commits 1000))
  (future (digging-refs 1000))
  (smooth)
  (no-stroke)
  (fill 226)
  (framerate 10))

(proapp/defapplet example2 :title "visugit"
  :setup setup :draw draw :size [800 800])

(defn -main [& [target]]
  (dosync
   (ref-set git/dir target))
  (proapp/run example2))
