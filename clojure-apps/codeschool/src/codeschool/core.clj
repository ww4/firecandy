(ns codeschool.core)

(defn dec-maker  [x]
  #(- % x)

  )

(def dec9 (dec-maker 9))


(defn mapset
  [func vec]
  (set (map func vec))
  )  

(defn mycounter
  [seq accum]
  (if (empty? seq)
    accum
    (mycounter (rest seq) (+ 1 accum))
    )
  )

(defn cmap
  [fn seq]
  (if (empty? seq)
    '()
    (cons (fn (first seq)) (cmap fn (rest seq)))))


































;;================= Revisit this problem later... ====================
;; trying to build a symmetrizer for multi-symmetric bodies, ie.
;; spiders or aliens. Should be able to pass a list of parts, and
;; a number of symmetries (5, or 8 maybe) and get back a symmetrized
;; parts list. This exersise is at the end of chapter 3, Brave & True.





(def asym-alien-parts [{:name "head" :size "3"}
                       {:name "eye#1" :size "1"}
                       {:name "ear#1" :size "1"}
                       {:name "shoulder#1" :size "1"}
                       {:name "arm#1" :size "1"}
                       {:name "leg#1" :size "1"}
                       {:name "abdomen" :size "1"}
                       {:name "chest" :size "1"}
                       {:name "nose" :size "1"}
                       {:name "foot#1" :size "1"}
                       ]
  
  )

(defn matching-part
  [part ptnum]
  {:name (clojure.string/replace (:name part) #^"1" ptnum)
   :size (:size part)})
  

(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts num]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part num)])))
          asym-body-parts))




