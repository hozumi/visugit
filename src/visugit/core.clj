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
(defonce digged-refs (ref {}))
(defonce commits (ref {}))
(defonce digged-commits (ref {}))
(defonce parent->child-commits (ref {}))
(defonce id->particle-map (ref {}))
(defonce springs (ref {}))
(defonce stop-digg-commits-flag (ref false))
(defonce ^VerletPhysics2D physics (VerletPhysics2D.))

(defn create-particle [id]
  (let [p (VerletParticle2D. (float (rand-int 1000))
                             (float (rand-int 1000)))]
    (dosync
     (alter id->particle-map assoc id p))
    (.addParticle physics p)
    p))

(defn create-springs [{:keys [constrained min]} me-id other-ids]
  (let [me-p (@id->particle-map me-id)]
    (doseq [other-id other-ids]
      (when (not= me-id other-id)
        (when-let [other-p (@id->particle-map other-id)]
          (when constrained
            (let [s (VerletConstrainedSpring2D. me-p other-p (:len constrained) (:str constrained))]
              (dosync (alter springs assoc #{me-id other-id :constrained} s))
              (.addSpring physics s)))
          (when min
            (let [s (VerletMinDistanceSpring2D. me-p other-p (:len min) (:str min))]
              (dosync (alter springs assoc #{me-id other-id :min} s))
              (.addSpring physics s))))))))

(defn add-commit [{:keys [id] :as co}]
  (create-particle id)
  (dosync
   (doseq [parent-id (:parents co)]
     (alter parent->child-commits update-in [parent-id]
            (fn [child-ids] (if child-ids [id] (conj child-ids id))))))
  ;;attraction to parents
  (create-springs {:constrained {:len 30 :str 0.01}} id (:parents co))
  ;;attraction to childs
  (create-springs {:constrained {:len 30 :str 0.01}} id (@parent->child-commits id))
  ;;repulsion
  (create-springs {:min {:len 50 :str 0.001}} id (keys @commits))
  ;;attraction to corresponding refs
  (create-springs {:constrained {:len 5 :str 0.01}};;, :min {:len 2 :str 0.001}}
                  id (map :name (filter (fn [r] (= id (:id r))) (vals @refs)))))

(defn update-commits []
  (when-not (empty? @digged-commits)
    (let [cache (ref nil)]
      (dosync
       (alter commits conj @digged-commits)
       (ref-set cache @digged-commits)
       (ref-set digged-commits {}))
      (doseq [co @cache]
        (add-commit (val co))))))

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
  
(defn update-refs []
  (when (not= @refs @digged-refs)
    (let [refs-set (-> @refs keys set)
          digged-set (-> @digged-refs keys set)
          removed-ref-ids (set/difference refs-set digged-set)
          added-ref-ids (set/difference digged-set refs-set)
          modified-ids (filter (fn [k] (not= (@refs k) (@digged-refs k)))
                               (set/intersection refs-set digged-set))]
      (doall (map modify-ref (map @refs modified-ids) (map @digged-refs modified-ids)))
      (dosync
       (ref-set refs @digged-refs))
      (doseq [r (map @refs added-ref-ids)]
        (add-ref r))
      (doseq [id removed-ref-ids]
        (remove-particle&springs id)
        (remove-particle&springs (str "label:" id)))
      )))

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
  (doseq [co (vals @commits)
          parent-id (:parents co)]
    (when-let [^VerletParticle2D parent-p (@id->particle-map parent-id)]
      (let [^VerletParticle2D me-p (@id->particle-map (:id co))]
        (line (.x me-p) (.y me-p) (.x parent-p) (.y parent-p))))))

(defn draw []
  (background-int 220)
  (.update physics)
  ;;(framerate 1)
  (stroke-float 150)
  (color-mode RGB)
  (fill-float 50 50 200 250)
  (update-refs)
  (update-commits)
  (draw-commit)
  (draw-refs))

(defn setup []
  (let [f (create-font "Arial" 11 true)]
    (text-font f))
  (dosync
   (ref-set git/dir "../git_tutorial/work/hello/")
   (ref-set digged-refs (git/get-refs)))
  (update-refs)
  (.setWorldBounds physics
                   (Rect. (Vec2D. 10 10)
                          (Vec2D. (- (width) 10) (- (/ (height) 2) 10))))
  (future (git/run-update-refs digged-refs 1000))
  (future (git/run-update-commit-map @refs digged-commits stop-digg-commits-flag 1000))
  (smooth)
  (no-stroke)
  (fill 226)
  (framerate 10))


(proapp/defapplet example2 :title "An example."
  :setup setup :draw draw :size [800 800])


