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

(defn roundrect [x y w h r]
  (no-stroke)
  (rect-mode CORNER)
  (with-translation [x y]
    (let [ax (- w 1)
          ay (- h 1)
          hr (/ r 2)]
      (rect 0 0 w h)
      (arc 0 0 r r (radians 180) (radians 270))
      (arc ax 0 r r (radians 270) (radians 360))
      (arc 0 ay r r (radians 90) (radians 180))
      (arc ax ay r r (radians 0) (radians 90))
      (rect 0 (- hr) w hr)
      (rect (- hr) 0 hr h)
      (rect 0 h w hr)
      (rect w 0 hr h))))

(defonce refs (ref {}))
(defonce digged-refs (ref {}))
(defonce commits (ref {}))
(defonce digged-commits (ref {}))
(defonce parent->child-commits (ref {}))
(defonce id->particle-map (ref {}))
(defonce springs (ref {}))
(defonce stop-digg-commits-flag (ref false))
(defonce staged-files (ref []))
(defonce ^VerletPhysics2D physics (VerletPhysics2D.))

#_(defn sort-commit* [acc rated-queue commits]
  (if-let [[co rate :as e] (first rated-queue)]
    (let [rated-parents (map (fn [c] [c (inc rate)])
                             (map commits (:parents co)))]
      (recur (update-in acc [co] (fn [lis] (if lis (conj lis rate) [rate])))
             (into (rest rated-queue) rated-parents) commits))
    acc))

#_(defn sort-commit [commits refs]
  (let [rated (sort-commit* {}
                            (for [[i refed-commit-id] (seq-utils/indexed (filter commits (vals refs)))]
                              [(commits refed-commit-id) (* 100 i)])
                            commits)]
    (map first
         (sort (fn [c1 c2] (< (second c1) (second c2)))
               (map (fn [[co rates]] [co (apply + rates)]) rated)))))

(defn create-particle []
  (VerletParticle2D. (float (rand-int 1000))
                     (float (rand-int 1000))))

(defn init-obj []
  (dosync
   (ref-set git/dir "../clj-http/")
   (ref-set refs (git/get-refs))
   ;;(ref-set commits (git/commit-map @refs))
   (alter id->particle-map
          conj (zipmap (keys @refs)
                       (repeatedly create-particle)))
   (alter id->particle-map
          conj (zipmap (map #(str "label:" %) (keys @refs))
                       (repeatedly create-particle))))
  (.setWorldBounds physics
                   (Rect. (Vec2D. 10 10)
                          (Vec2D. (- (width) 10) (- (/ (height) 2) 10))))
  ;; label - ref
  (doseq [r (vals (filter git/is-type-commit? @refs))]
    (let [ref-p (@id->particle-map (:name r))
          label-p (@id->particle-map (str "label:" (:name r)))
          attract (VerletConstrainedSpring2D. ref-p label-p 5 0.0005)
          min (VerletMinDistanceSpring2D. ref-p label-p 20 0.001)
          ]
      (dosync
       (alter springs assoc #{(:name r) (str "label:" (:name r)) :attraction} attract)
       (alter springs assoc #{(:name r) (str "label:" (:name r)) :repulsion} min))
      (.addSpring physics attract)
      (.addSpring physics min)
      ))
  ;;label - label repulsion
  (let [label-keys (map #(str "label:" %)
                    (keys (filter git/is-type-commit? @refs)))]
    (doseq [[la1-key la2-key] (comb/combinations label-keys 2)]
      (let [la1 (@id->particle-map la1-key)
            la2 (@id->particle-map la2-key)
            min (VerletMinDistanceSpring2D. la1 la2 25 0.8)]
        (dosync (alter springs assoc #{la1-key la2-key :repulsion} min))
        (.addSpring physics min)
        )))
  (doseq [p (vals @id->particle-map)]
    (.addParticle physics p))
  )

(defn add-commit [{:keys [id] :as co}]
  (let [me-p (create-particle)]
    (dosync
     (alter id->particle-map assoc id me-p)
     (doseq [parent-id (:parents co)]
       (alter parent->child-commits update-in [parent-id]
              (fn [child-ids] (if child-ids [id] (conj child-ids id))))))
    (.addParticle physics me-p)
    ;;attraction to parents
    (doseq [parent-id (:parents co)]
      (when-let [parent-p (@id->particle-map parent-id)]
        (let [s (VerletConstrainedSpring2D. me-p parent-p 30 0.01)]
          (dosync (alter springs assoc #{id parent-id :attraction} s))
          (.addSpring physics s))))
    ;;attraction to childs
    (doseq [child-id (@parent->child-commits id)]
      (when-let [child-p (@id->particle-map child-id)]
        (let [s (VerletConstrainedSpring2D. me-p child-p 30 0.01)]
          (dosync (alter springs assoc #{child-id id :attraction} s))
          (.addSpring physics s))))
    ;;repulsion
    (doseq [you-id (filter @id->particle-map (keys @commits))]
      (let [you-p (@id->particle-map you-id)]
        (when-not (= id you-id)
          (let [s (VerletMinDistanceSpring2D. me-p you-p 50 0.001)]
            (dosync (alter springs assoc #{id you-id :repulsion} s))
            (.addSpring physics s)))))
    ;;attraction to corresponding refs
    (doseq [ref-id (map :name (filter (fn [r] (= id (:id r))) (vals @refs)))]
      (let [ref-p (@id->particle-map ref-id)
            attract (VerletConstrainedSpring2D. me-p ref-p 5 0.0005)
            min (VerletMinDistanceSpring2D. me-p ref-p 1 0.0001)]
        (dosync
         (alter springs assoc #{id ref-id :attraction} attract)
         (alter springs assoc #{id ref-id :repulsion} min))
        (.addSpring physics attract)
        (.addSpring physics min)))
    ))

(defn update-commits []
  (when-not (empty? @digged-commits)
    (let [cache (ref nil)]
      (dosync
       (alter commits conj @digged-commits)
       (ref-set cache @digged-commits)
       (ref-set digged-commits {}))
      (doseq [co @cache]
        (add-commit (val co))))))

#_(defn update-refs []
  (when (not= @refs @digged-refs)
    (let [removed-refs (set/difference @refs @digged-refs)
          removed-ref-ps (map #(@id->particle-map (:name %)) removed)
          removed-label-ps (map #(@id->particle-map (str "label:" (:name %))) removed)
          removed-springs (map #(@repulsive-ref->commit-springs ))
          added (set/difference @digged-refs @refs)]
    (dosync
     (ref-set refs @digged-refs)
     (doseq [{name :name :as r} added]
       (alter id->particle-map conj
              [name (create-particle)])
       (alter id->particle-map conj
              [(str "label:" name) (create-particle)]))
     (doseq [{name :name :as r} removed]
       (alter id->particle-map dissoc name)
       (alter id->particle-map dissoc (str "label:" name))))
     (let [removed]))))
     
(defn draw-refs []
  (doseq [k (keys (filter git/is-type-commit? @refs))]
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
  (update-commits)
  (draw-commit)
  (draw-refs))

(defn setup []
  (let [f (create-font "Arial" 11 true)]
    (text-font f))
  (init-obj)
  (future (git/run-update-commit-map @refs digged-commits stop-digg-commits-flag))
  (smooth)
  (no-stroke)
  (fill 226)
  (framerate 10))


(proapp/defapplet example2 :title "An example."
  :setup setup :draw draw :size [800 800])


