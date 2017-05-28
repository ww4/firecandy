(ns summarywriter.core
  (:gen-class))

(require '[clojure.java.io :as io]
         '[clojure-csv.core :as csv]
         '[semantic-csv.core :as sc]
         '[hiccup.core :as hic])

(def choices-1 '("Xpos" "Vpos" "Pos" "Neg" "VNeg"))
(def col-titles-1 '("Xpos" "Vpos" "Pos" "Neg" "VNeg"))

(def choices-2 '("Oft2f" "2fast" "Right" "2slow" "Oft2s"))
(def col-titles-2 '("Often<br> too fast" "Sometimes<br>too fast" "Just right" "Sometimes<br>too slow" "Often<br>too slow"))

(def choices-3 '("Alway" "Usual" "Somet" "Rarel"))
(def col-titles-3 '("Always" "Usually" "Sometimes" "Rarely"))

(def choices-4 '("EX" "VG" "ADQ" "SP"))
(def col-titles-4 '("Excellent" "Very good" "Adequate" "Sub-par"))

(def choices-5 '("EX" "VG" "ADQ" "SP" "NM" "Ns"))
(def col-titles-5 '("Excellent" "Very good" "Adequate" "Sub-par" "Needed more" "Not sure what this is"))

(def choices-6 '("5" "4" "3" "2" "1"))
(def col-titles-6 '("Strongly agree" "Agree" "Neutral" "Disagree" "Strongly disagree"))

(def choices-7 '("A1" "A2" "A2" "A4"))
(def col-titles-7 '("Excellent" "Very good" "Adequate" "Sub-par"))

;;;
;;; Your functions go here ...
;;;


(defn count-occurences [coll]
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} coll)
  )

(defn collect-answers [responses q-code]
  (filter not-empty (map #(get % q-code) responses))) 

(defn collect-answers-multi [responses q-codes]
  (mapcat #(collect-answers responses %) q-codes)
)

(defn choice-totals [responses q-code]
  (count-occurences (collect-answers responses q-code))
)
 
(defn choice-row [responses label choices q-code]
  [:tr 
   [:td label]
   (let [totals (choice-totals responses q-code)]
     (for [choice choices] 
       [:td (get totals choice 0)]))]
   
)

(defn choice-matrix [responses titles choices rowspecs]
  [:tr
   (for [title titles]
     [:td title])]
  [:tr
  (choice-row responses (:label rowspecs) choices (:q-code rowspecs))
]
)

    ;; (defn survey-report-css []
     ;;   "table {width: 600px} {text-align:center; min-width:75px} td {text-align:center} td:first-child {text-align:right;width:200px}")

     ;; (defn summary-boomer-2016 [responses header]
     ;;   [:html
     ;;    [:head
     ;;     [:style (survey-report-css)]]
     ;;    [:body
     ;;     [:h1 header]
     ;;     (choice-matrix
     ;;      responses col-titles-1 choices-1
     ;; [{:label "Worth of class" :q-code :Q1S1}])
     ;;     [:hr]
     ;;     (choice-list
     ;;      responses "Which section of the camp were you mostly in?" :Q1a
     ;;      [{:label "Novice/Basic: " :choice-code "Nov"}
     ;;       {:label "Intermediate/Advanced: " :choice-code "Int"}
     ;;       ])
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-2 choices-2
     ;;      [{:label "Pace of learning" :q-code :Q2S1}])
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-3 choices-3
     ;;      [{:label "Explanations clear" :q-code :Q3S2}
     ;;       {:label "Engaged in your group" :q-code :Q3S3}
     ;;       {:label "Overwhelmed in your group" :q-code :Q3S4}
     ;;       {:label "Encouraged in your group" :q-code :Q3S5}
     ;;       {:label "Well-coached in your group" :q-code :Q3S6}]
     ;;      )
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-4 choices-4
     ;;      [{:label "In-class instruction" :q-code :Q8S1}
     ;;       {:label "Productive use of time" :q-code :Q8S2}
     ;;       {:label "Time in jam groups" :q-code :Q8S3}
     ;;       {:label "Pre-class materials" :q-code :Q8S4}]
     ;;      )
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-1 choices-1
     ;;      [{:label "Good learning experience" :q-code :Q10S2}
     ;;       {:label "Fun" :q-code :Q10S3}
     ;;       {:label "Worth the money" :q-code :Q10S4}
     ;;       {:label "Makeup of your jam groups" :q-code :Q10S5}
     ;;       {:label "Jam coaching" :q-code :Q10S6}]
     ;;      )
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-5 choices-5
     ;;      [{:label "P-holder solos instr" :q-code :Q11S2}
     ;;       {:label "Practice finding melodies" :q-code :Q11S3}
     ;;       {:label "Transposing" :q-code :Q11S4}
     ;;       {:label "Leading song in jam" :q-code :Q11S5}
     ;;       {:label "Harmony singing instruction" :q-code :Q11S6}
     ;;       {:label "Other singing instruction" :q-code :Q11S7}]
     ;;      )
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-6 choices-6
     ;;      [{:label "Location convenient" :q-code :Q12S1}
     ;;       {:label "Location comfortable" :q-code :Q12S2}]
     ;;      )
     ;;     [:hr]
     ;;     (answer-list responses "Things liked best" [:Q4S1 :Q4S2 :Q4S3])
     ;;     (answer-list responses "Things to change" [:Q5S1 :Q5S2 :Q5S3])
     ;;     (answer-list responses "Add or emphasize" :Q6)
     ;;     (answer-list responses "Subtract or de-emphasize" :Q7)
     ;;     (answer-list responses "Improving pre-class materials" :Q9)
     ;;     (answer-list responses "Food" :Q105)
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-4 choices-4
     ;;      [{:label "Tuesday afternoon improvisation" :q-code :Q103SQ001}
     ;;       {:label "Tuesday afternoon music Theory" :q-code :Q103SQ002}
     ;;       {:label "Tuesday afternoon history" :q-code :Q103SQ003}]
     ;;      )
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-4 choices-4
     ;;      [{:label "Tuesday open mic" :q-code :Q104SQ001}
     ;;       {:label "Wednesday jamboree" :q-code :Q104SQ002}
     ;;       {:label "Thursday cabin stage" :q-code :Q104SQ003}]
     ;;      )
     ;;     (choice-list
     ;;      responses "First heard about class" :Q14
     ;;      [{:label "Email announcement: " :choice-code "A1"}
     ;;       {:label "Merlefest website: " :choice-code "A2"}
     ;;       {:label "DrBanjo.com or bgJAM.com: " :choice-code "A3"}
     ;;       {:label "MandolinCafe.com: " :choice-code "A4"}
     ;;       {:label "BanjoHangout.com: " :choice-code "A5"}
     ;;       {:label "Other website: " :choice-code "A6"}
     ;;       {:label "Fiddler magazine: " :choice-code "A7"}
     ;;       {:label "Flatpicking Guitar magazine: " :choice-code "A8"}
     ;;       {:label "Other magazine: " :choice-code "A9"}
     ;;       {:label "Teacher: " :choice-code "A10"}
     ;;       {:label "Saw Cabin Stage performance or workshop at Merlefest: " :choice-code "A11"}
     ;;       {:label "Wilkes Acoustic Folk Society: " :choice-code "A12"}
     ;;       {:label "From a previous camper: " :choice-code "A13"}
     ;;       {:label "Other: " :choice-code "A14"}
     ;;       ])
     ;;     (answer-list responses "Comments--first heard about class" :Q14comment)
     ;;     (answer-list responses "To tell the WM office" :Q16)
     ;;     (answer-list responses "Other comments" :Q17)
     ;;     (teacher-comments responses "Scott Freeman" :Q202 )
     ;;     (teacher-comments responses "Steve Lewis" :Q203 )
     ;;     (teacher-comments responses "Chris Saenz" :Q204 )
     ;;     (teacher-comments responses "Keith Yoder" :Q207 )
     ;;     (teacher-comments responses "Gilbert Nelson" :Q205 )
     ;;     (teacher-comments responses "Leigh Nelson" :Q206 )
     ;;     (teacher-comments responses "Dee Rosser" :Q209 )
     ;;     (teacher-comments responses "Pete Wernick" :Q200 )
     ;;     (teacher-comments responses "Joan Wernick" :Q208 )
     ;;     (teacher-comments responses "Alan Epstein" :Q201 )
     ;;     ]
     ;;   ]

     ;;   )

     ;; (defn summary-boomer-2017 [responses header]
     ;;   [:html
     ;;    [:head
     ;;     [:style (survey-report-css)]]
     ;;    [:body
     ;;     [:h1 header]
     ;;     (choice-matrix
     ;;      responses col-titles-1 choices-1
     ;;      [{:label "Worth of camp" :q-code :Q1S1}])
     ;;     [:hr]
     ;;     (choice-list
     ;;      responses "Which section of the camp were you mostly in?" :Q1a
     ;;      [{:label "Novice/Basic: " :choice-code "A1"}
     ;;       {:label "Intermediate/Advanced: " :choice-code "A2"}
     ;;       ])
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-2 choices-2
     ;;      [{:label "Pace of learning" :q-code :Q2S1}])
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-3 choices-3
     ;;      [{:label "Explanations clear" :q-code :Q3S2}
     ;;       {:label "Engaged in your group" :q-code :Q3S3}
     ;;       {:label "Overwhelmed in your group" :q-code :Q3S4}
     ;;       {:label "Encouraged in your group" :q-code :Q3S5}
     ;;       {:label "Well-coached in your group" :q-code :Q3S6}]
     ;;      )
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-4 choices-4
     ;;      [{:label "In-class instruction" :q-code :Q8S1}
     ;;       {:label "Productive use of time" :q-code :Q8S2}
     ;;       {:label "Time in jam groups" :q-code :Q8S3}
     ;;       {:label "Pre-class materials" :q-code :Q8S4}]
     ;;      )
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-1 choices-1
     ;;      [{:label "Good learning experience" :q-code :Q10S2}
     ;;       {:label "Fun" :q-code :Q10S3}
     ;;       {:label "Worth the money" :q-code :Q10S4}
     ;;       {:label "Makeup of your jam groups" :q-code :Q10S5}
     ;;       {:label "Jam coaching" :q-code :Q10S6}]
     ;;      )
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-5 choices-5
     ;;      [{:label "P-holder solos instr" :q-code :Q11S2}
     ;;       {:label "Practice finding melodies" :q-code :Q11S3}
     ;;       {:label "Transposing" :q-code :Q11S4}
     ;;       {:label "Leading song in jam" :q-code :Q11S5}
     ;;       {:label "Harmony singing instruction" :q-code :Q11S6}
     ;;       {:label "Other singing instruction" :q-code :Q11S7}]
     ;;      )
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-6 choices-6
     ;;      [{:label "Location convenient" :q-code :Q12S1}
     ;;       {:label "Location comfortable" :q-code :Q12S2}]
     ;;      )
     ;;     [:hr]
     ;;     (answer-list responses "Things liked best" [:Q4S1 :Q4S2 :Q4S3])
     ;;     (answer-list responses "Things to change" [:Q5S1 :Q5S2 :Q5S3])
     ;;     (answer-list responses "Add or emphasize" :Q6)
     ;;     (answer-list responses "Subtract or de-emphasize" :Q7)
     ;;     (answer-list responses "Improving pre-class materials" :Q9)
     ;;     (answer-list responses "Food" :Q105)
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-7 choices-7
     ;;      [{:label "Tuesday afternoon improvisation" :q-code :Q103SQ001}
     ;;       {:label "Tuesday afternoon music Theory" :q-code :Q103SQ002}
     ;;       {:label "Tuesday afternoon history" :q-code :Q103SQ003}]
     ;;      )
     ;;     [:hr]
     ;;     (choice-matrix
     ;;      responses col-titles-7 choices-7
     ;;      [{:label "Tuesday open mic" :q-code :Q104SQ001}
     ;;       {:label "Wednesday jamboree" :q-code :Q104SQ002}
     ;;       {:label "Thursday cabin stage" :q-code :Q104SQ003}
     ;;       {:label "Fiddle tunes jam" :q-code :Q104SQ004}]
     ;;      )
     ;;     (choice-list
     ;;      responses "First heard about class" :Q14
     ;;      [{:label "Email announcement: " :choice-code "A1"}
     ;;       {:label "Merlefest website: " :choice-code "A2"}
     ;;       {:label "DrBanjo.com or bgJAM.com: " :choice-code "A3"}
     ;;       {:label "MandolinCafe.com: " :choice-code "A4"}
     ;;       {:label "BanjoHangout.com: " :choice-code "A5"}
     ;;       {:label "Other website: " :choice-code "A6"}
     ;;       {:label "Fiddler magazine: " :choice-code "A7"}
     ;;       {:label "Flatpicking Guitar magazine: " :choice-code "A8"}
     ;;       {:label "Other magazine: " :choice-code "A9"}
     ;;       {:label "Teacher: " :choice-code "A10"}
     ;;       {:label "Saw Cabin Stage performance or workshop at Merlefest: " :choice-code "A11"}
     ;;       {:label "Wilkes Acoustic Folk Society: " :choice-code "A12"}
     ;;       {:label "From a previous camper: " :choice-code "A13"}
     ;;       {:label "Other: " :choice-code "A14"}
     ;;       ])
     ;;     (answer-list responses "Comments--first heard about class" :Q14comment)
     ;;     (answer-list responses "To tell the WM office" :Q16)
     ;;     (answer-list responses "Other comments" :Q17)
     ;;     (teacher-comments responses "Scott Freeman" :Q202 )
     ;;     (teacher-comments responses "Steve Lewis" :Q203 )
     ;;     (teacher-comments responses "Scott Manring (Scooter)" :Q204 )
     ;;     (teacher-comments responses "Keith Yoder" :Q207 )
     ;;     (teacher-comments responses "Gilbert Nelson" :Q205 )
     ;;     (teacher-comments responses "Leigh Nelson" :Q206 )
     ;;     (teacher-comments responses "Dee Rosser" :Q209 )
     ;;     (teacher-comments responses "Pete Wernick" :Q200 )
     ;;     (teacher-comments responses "Joan Wernick" :Q208 )
     ;;     (teacher-comments responses "Cecil Gurganus" :Q201 )
     ;;     (teacher-comments responses "Julie Chiles" :Q210 )
     ;;     (teacher-comments responses "Zeb Gambill" :Q211 )
     ;;     (teacher-comments responses "Brandon Holder" :Q212 )
     ;;     (teacher-comments responses "Liam Purcell" :Q213 )
     ;;     ]
     ;;   ]

     ;;   )

     ;; (defn survey-novices-only [responses summary-fn header]
     ;;   (let [novice-responses (filter #(= "A1" (get %1 :Q1a)) responses)]
     ;;     (summary-fn novice-responses (str header " (basic)")))
     ;;   )

     ;; (defn survey-intermediates-only [responses summary-fn header]
     ;;   (let [int-responses (filter #(= "A2" (get %1 :Q1a)) responses)]
     ;;     (summary-fn int-responses (str header " (advanced)")))
     ;;   )

     ;; (defn survey-all [responses summary-fn header]
     ;;   (summary-fn responses header))

     ;; (defn make-survey-reports [survey-name]
     ;;   (let [responses (sc/slurp-csv (str survey-name ".csv"))
     ;;         title "Student comments Apr 2017 Boomer"]
     ;;     (with-open [w (io/writer (str survey-name " (all).html"))]
     ;;       (.write w (hic/html (survey-all responses summary-boomer-2017 title))))
     ;;     (with-open [w (io/writer (str survey-name " (basic).html"))]
     ;;       (.write w (hic/html (survey-novices-only responses summary-boomer-2017 title))))
     ;;     (with-open [w (io/writer (str survey-name " (advanced).html"))]
     ;;       (.write w (hic/html (survey-intermediates-only responses summary-boomer-2017 title))))
     ;;     )
     ;;   )
     
