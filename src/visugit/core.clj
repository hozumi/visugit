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
(defonce commits (ref {}))
(defonce dug-commits (ref {}))
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

(defn create-particle [id]
  (let [p (VerletParticle2D. (float (rand-int 1000))
                             (float (rand-int 1000)))]
    (dosync
     (alter id->particle-map assoc id p))
    (.addParticle physics p)
    p))

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

(defn add-commit [{:keys [id] :as co}]
  (create-particle id)
  (dosync
   (doseq [parent-id (:parents co)]
     (alter parent->child-commits update-in [parent-id]
            (fn [child-ids] (if child-ids (conj child-ids id) [id])))))
  ;;attraction to parents
  (create-springs {:constrained {:len 30 :str 0.01}} id (:parents co))
  ;;attraction to childs
  (create-springs {:constrained {:len 30 :str 0.01}} id (@parent->child-commits id))
  ;;repulsion
  (create-springs {:min {:len 50 :str 0.001}} id (keys @commits))
  ;;attraction to corresponding refs
  (create-springs {:constrained {:len 5 :str 0.01}}
                  id (map :name (filter (fn [r] (= id (:id r))) (vals @refs)))))

(defn update-commits [commits dug-commits]
  (when-not (empty? @dug-commits)
    (let [cache (ref nil)]
    (dosync
     (alter commits merge @dug-commits)
     (ref-set cache @dug-commits)
     (ref-set dug-commits {}))
    (doseq [co (vals @cache)]
      (add-commit co)))))

(defn digging-commits [starts commits dug-commits]
  (doseq [start starts]
    (let [new-commits (git/get-commit-map-diff start (merge @dug-commits @commits))]
      (when-not (empty? new-commits)
        (dosync (alter dug-commits merge new-commits))))))

(defn add-ref [{name :name :as r}]
  (let [label-id (str "label:" name)]
    (create-particle name)
    (create-particle label-id)
    (create-springs {:constrained {:len 5 :str 0.05}, :min {:len 20 :str 0.001}}
                    name [label-id])
    ;;label - label repulsion
    (create-springs {:min {:len 25 :str 0.6}}
                    name (map #(str "label:" %)
                              (keys (filter git/is-type-commit? @refs))))))

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
  (create-springs {:constrained {:len 5 :str 0.01}}
                  (:name new-ref) [(:id new-ref)]))

(defn update-refs [refs dug-refs]
  (when (not= @refs @dug-refs)
    (let [refs-set (-> @refs keys set)
          dug-refs-set (-> @dug-refs keys set)
          removed-ref-ids (set/difference refs-set dug-refs-set)
          added-ref-ids (set/difference dug-refs-set refs-set)
          modified-ids (filter (fn [k] (not= (@refs k) (@dug-refs k)))
                               (set/intersection refs-set dug-refs-set))]
      (doall (map modify-ref (map @refs modified-ids) (map @dug-refs modified-ids)))
      (dosync
       (ref-set refs @dug-refs))
      (doseq [r (map @refs added-ref-ids)]
        (add-ref r))
      (doseq [id removed-ref-ids]
        (remove-particle&springs id)
        (remove-particle&springs (str "label:" id))))))

(defn digging-refs [dug-refs polling-ms]
  (loop []
    (dosync (ref-set dug-refs (git/get-refs)))
    (Thread/sleep polling-ms)
    (recur)))

(defn draw-refs []
  (doseq [k (filter @id->particle-map (keys (filter git/is-type-commit? @refs)))]
    (let [^VerletParticle2D p (@id->particle-map k)]
      (if (= "HEAD" k)
        (fill-float 250 50 50 150)
        (fill-float 100 250 200 150))
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
  (update-commits commits dug-commits)
  (update-refs refs dug-refs)
  (draw-commit)
  (draw-refs)
  (draw-stage&wt))

(defn setup []
  (let [f (create-font "Arial" 11 true)]
    (text-font f))
  (dosync
   (ref-set git/dir "../git_tutorial/work/hello/")
   (ref-set dug-refs (git/get-refs)))
  (.setWorldBounds physics
                   (Rect. (Vec2D. 20 10)
                          (Vec2D. (- (width) 20) (- (/ (height) 2) 10))))
  (update-refs refs dug-refs)
  (future (update-stage&wt 1000))
  (future (digging-commits (keys @dug-refs) commits dug-commits))
  (future (digging-refs dug-refs 1000))
  (smooth)
  (no-stroke)
  (fill 226)
  (framerate 10))


(proapp/defapplet example2 :title "An example."
  :setup setup :draw draw :size [800 800])


