(ns visugit.core
  (:use [rosado.processing])

  (:require [visugit.shell-out :as shell]
            [visugit.git :as git]
            [org.satta.glob :as glob]
            [clojure.java.io :as jio]
            [clojure.string :as cstr]
            [clojure.contrib.string :as ccstr]
            [clojure.contrib.seq-utils :as seq-utils]
            [rosado.processing.applet :as proapp])
  (:import [java.io File]))

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

(defonce commits (ref nil))
(defonce sorted-commits (ref nil))
(defonce id->address-map (ref {}))
(defonce refs (ref nil))

(defn draw-refs [refs]
  (fill-float 100 250 200 250)
  (doseq [[n lines] (seq-utils/indexed (partition-all 20 refs))
          [i c] (seq-utils/indexed lines)]
    (ellipse (+ 20 (* i 20)) (+ 80 (* n 20)) 10 10)))

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

(defn init-obj []
  (dosync
   (ref-set git/dir "../clj-http/")
   (ref-set refs (git/get-refs))
   (ref-set commits (git/commit-map @refs))
   (ref-set sorted-commits (sort-commit @commits @refs))))

(defn draw-commit [s-commits]
  (fill-float 100 100 250 250)
  (let [num 10]
    (doseq [[n lines] (seq-utils/indexed (partition-all num s-commits))
            [i c] (seq-utils/indexed lines)]
      (let [x (+ 20 (if (even? n) (* i 20) (- (* num 20) (* (inc i) 20))))
            y (+ 200 (* n 20))]
        (dosync (alter id->address-map assoc (:id c) [x y]))
        (ellipse x y 10 10)))
    (stroke 20 20 240 255)
    (stroke-weight 2)
    (doseq [c s-commits
            p (:parents c)]
      (line (@id->address-map (:id c))
            (@id->address-map p)))))

(defn draw []
  (background-int 220)
  (framerate 1)
  (stroke-float 150)
  (color-mode RGB)
  (fill-float 50 50 200 250)
  (draw-refs @refs)
  (draw-commit @sorted-commits)
  ;;(roundrect 200 200 50 50 20)
  ;;(filter-kind INVERT)
  )

(defn setup []
  (init-obj)
  (smooth)
  (no-stroke)
  (fill 226)
  (framerate 1))

(proapp/defapplet example2 :title "An example."
  :setup setup :draw draw :size [800 800])


