(ns visugit.core
  (:use [rosado.processing])

  (:require [visugit.shell-out :as shell]
            [visugit.git :as git]
            [org.satta.glob :as glob]
            [clojure.java.io :as jio]
            [clojure.string :as cstr]
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

(defonce refs (ref nil))
(defonce commits (ref nil))
(defonce sorted-commits (ref nil))
(defonce id->particle-map (ref {}))
(defonce attractive-springs (ref {}))
(defonce repulsive-springs (ref {}))
(defonce ^VerletPhysics2D physics (VerletPhysics2D.))

(defn sort-commit* [acc rated-queue commits]
  (if-let [[co rate :as e] (first rated-queue)]
    (let [rated-parents (map (fn [c] [c (inc rate)])
                             (map commits (:parents co)))]
      (recur (update-in acc [co] (fn [lis] (if lis (conj lis rate) [rate])))
             (into (rest rated-queue) rated-parents) commits))
    acc))

(defn sort-commit [commits refs]
  (let [rated (sort-commit* {}
                            (for [[i refed-commit-id] (seq-utils/indexed (filter commits (vals refs)))]
                              [(commits refed-commit-id) (* 100 i)])
                            commits)]
    (map first
         (sort (fn [c1 c2] (< (second c1) (second c2)))
               (map (fn [[co rates]] [co (apply + rates)]) rated)))))

(defn only-commit [[_ id]]
  (@commits id))

(defn init-obj []
  (dosync
   (ref-set git/dir "../clj-http/")
   (ref-set refs (git/get-refs))
   (ref-set commits (git/commit-map @refs))
   (ref-set sorted-commits (sort-commit @commits @refs))
   (ref-set id->particle-map
            (into {}
                  (map (fn [[n {id :id}]]
                         [id (VerletParticle2D. (float (* n 10))
                                                (float (+ (rand-int 10) (* n 5))))])
                       (seq-utils/indexed @sorted-commits))))
   (alter id->particle-map
          conj (zipmap (keys @refs)
                       (repeatedly #(VerletParticle2D. (float 20)
                                                       (float 20)))))
   (alter id->particle-map
          conj (zipmap (map #(str "label:" %) (keys @refs))
                       (repeatedly #(VerletParticle2D. (float 20)
                                                       (float 20))))))
  (.setWorldBounds physics
                   (Rect. (Vec2D. 10 10)
                          (Vec2D. (- (width) 10) (- (/ (height) 2) 10))))
    
  (doseq [c (vals @commits)
          parent-id (:parents c)]
    (let [me-p (@id->particle-map (:id c))
          you-p (@id->particle-map parent-id)
          s (VerletConstrainedSpring2D. me-p you-p 30 0.01)]
      (dosync (alter attractive-springs conj [[me-p you-p] s]))
      (.addSpring physics s)))
  (doseq [[me you] (comb/combinations (map @id->particle-map (keys @commits)) 2)]
    (let [s (VerletMinDistanceSpring2D. me you 50 0.001)]
      (dosync (alter repulsive-springs conj [[me you] s]))
      (.addSpring physics s)))
  ;;refs
  (doseq [r (filter only-commit @refs)]
    (let [me (@id->particle-map (key r))
          target (@id->particle-map (val r))
          attract (VerletConstrainedSpring2D. me target 5 0.0005)
          min (VerletMinDistanceSpring2D. me target 1 0.0001)
          ]
      (dosync
       (alter attractive-springs conj [[me target] attract])
       (alter repulsive-springs conj [[me target] min]))
      (.addSpring physics attract)
      (.addSpring physics min)
      ))
  ;; label attraction
  (doseq [r (filter only-commit @refs)]
    (let [me-p (@id->particle-map (key r))
          label-p (@id->particle-map (str "label:" (key r)))
          attract (VerletConstrainedSpring2D. me-p label-p 5 0.0005)
          min (VerletMinDistanceSpring2D. me-p label-p 20 0.001)
          ]
      (dosync
       (alter attractive-springs conj [[me-p label-p] attract])
       (alter repulsive-springs conj [[me-p label-p] min]))
      (.addSpring physics attract)
      (.addSpring physics min)
      ))
  ;;label of ref repulsion
  (let [labels (map (fn [id] (@id->particle-map (str "label:" id)))
                    (keys (filter only-commit @refs)))]
    (doseq [[la1 la2] (comb/combinations labels 2)]
      (let [min (VerletMinDistanceSpring2D. la1 la2 25 0.8)]
        (dosync (alter repulsive-springs conj [[la1 la2] min]))
        (.addSpring physics min)
        )))
  (doseq [p (vals @id->particle-map)]
    (.addParticle physics p))
  )

(defn draw-refs []
  (doseq [k (keys (filter only-commit @refs))]
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
  (doseq [entr  @commits]
    (let [^VerletParticle2D p (@id->particle-map (key entr))]
      (ellipse (.x p) (.y p) 10 10)
      ;;(text-align CENTER)
      ;;(string->text (key entr) (.x p) (+ (.y p) 20))
      ))
  (doseq [c (vals @commits)
          parent-id (:parents c)]
    (let [^VerletParticle2D p1 (@id->particle-map (:id c))
          ^VerletParticle2D p2 (@id->particle-map parent-id)]
      (line (.x p1) (.y p1) (.x p2) (.y p2)))))

(defn draw []
  (background-int 220)
  (.update physics)
  ;;(framerate 1)
  (stroke-float 150)
  (color-mode RGB)
  (fill-float 50 50 200 250)
  (draw-commit)
  (draw-refs))

(defn setup []
  (let [f (create-font "Arial" 11 true)]
    (text-font f))
  (init-obj)
  (smooth)
  (no-stroke)
  (fill 226)
  (framerate 10))


(proapp/defapplet example2 :title "An example."
  :setup setup :draw draw :size [800 800])


