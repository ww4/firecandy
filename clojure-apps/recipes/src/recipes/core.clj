(ns recipes.core
  (:gen-class))

(require '[clojure.java.io :as io]

          '[clojure.string :as str]

          '[clojure.java.jdbc :as sql]

          '[clojure-csv.core :as csv]

          '[semantic-csv.core :as sc]

          '[hiccup.core :as hic]

          '[inflections.core :as inf])

(def db {
         :dbtype "postgresql"
         :dbname "test"
         :user "chris"
         :password "recipe"})

(def sample-recipe  ;;recipe data structure
  {:title "Skillet Chicken Macaroni"
   :notes "Serves 14-16. This is tripled from the original recipe."
   :ingredients
   [{:ing_name "butter" :unit "stick" :qty 1}
    {:ing_name "elbow macaroni" :unit "cup" :qty 6}
    {:ing_name "grated cheddar cheese" :unit "handful" :qty 1}
    {:ing_name "onion" :unit "" :qty 2}
    {:ing_name "diced cooked chicken" :unit "cup" :qty 2}
    {:ing_name "tomato sauce" :unit "15 oz can" :qty 3}]
   :instructions
   ["Sauté onion in butter in very large skillet." "Mix dry macaroni in butter and onion till coated and yellowed." "Add tomato sauce and water and seasonings." "Cover and simmer 15 minutes, stirring occasionally. Add water if needed." "Add chicken that has been pulled apart.  Cook 5 minutes more." "Cover with grated cheese and simmer until cheese melts."]})



(def sample-inglist ;;ingredient list from text file
  "1 stick of butter
6 cups of elbow macaroni
1 handful of grated cheese
2 onions
2 cups of diced cooked chicken
3 15 oz cans of tomato sauce" )

(defn initialize-db []
  (if (zero? (:count (first (sql/query db
                     ["SELECT COUNT (*) FROM information_schema.tables WHERE table_schema = 'public'"]))))
    (do
      (sql/execute! db [(sql/create-table-ddl  :recipe [[:id :serial "PRIMARY KEY"]
                                                        [:title "VARCHAR"]
                                                        [:notes "VARCHAR"]
                                                        [:instructions "VARCHAR"]])])
      (sql/execute! db [(sql/create-table-ddl :ingredients [[:id :serial "PRIMARY KEY"]                                                                                                     [:ing_name "VARCHAR" ]])])
      (sql/execute! db  (sql/create-table-ddl :ing_qty [[:recipe_id :int "REFERENCES recipe"]
                                                        [:ing_id :int "REFERENCES ingredients"]
                                                        [:unit "VARCHAR"]
                                                        [:qty :int]])))))

(defn write-sample-file []
  (spit "/home/chris/recipes.txt" (prn-str sample-recipe)))
(defn save-recipe [recipe]
  ;; takes a data structure like sample-recipe
  ;; check for ingredient, add to ingredient table
  ;; increment last recipe id
  (let [{:keys [title notes ingredients instructions]} recipe]
    (sql/insert! db :recipe {:title title
                             :notes notes
                             :instructions (prn-str instructions)})
    (let [newid (:max (first (sql/query db ["SELECT MAX(id) FROM recipe WHERE title=?" title])))]
      (for [ingredient ingredients]
        (do
          (if (empty? (sql/query db ["SELECT 1 FROM ingredients WHERE ing_name=?" (:ing_name ingredient)]))
            (sql/insert! db :ingredients {:ing_name (:ing_name ingredient)}))
          (let [ing_id (:id (first (sql/query db ["SELECT id FROM ingredients WHERE ing_name=?" (:ing_name ingredient)])))]
            (sql/insert! db :ing_qty {:recipe_id newid
                                      :ing_id ing_id
                                      :unit (:unit ingredient)
                                      :qty (:qty ingredient)})))))))


(defn get-recipe [id]
  (merge
   (first (sql/query db ["SELECT title, notes FROM recipe WHERE id=?" id]))
         {:ingredients (into [] (sql/query db ["SELECT ing_name, unit, qty FROM ing_qty JOIN ingredients ON ing_qty.ing_id=ingredients.id WHERE recipe_id=?" id]))}
         {:instructions (read-string (:instructions (first (sql/query db ["SELECT instructions FROM recipe WHERE id=?" id]))))}))

(defn print-recipe [id]
  (let [rp (get-recipe id)]
    [:div
     [:h3 (:title rp)]
     [:table
      (for [ingredient (:ingredients rp)]
        [:tr
         [:td (:qty ingredient)]
         [:td (if (empty? (:unit ingredient))
                [:td (inf/pluralize (:qty ingredient)(:ing_name ingredient))]

                [:td (str  (inf/pluralize (:qty ingredient) (:unit ingredient)) " "(:ing_name ingredient))]) ]
         ])]
     [:ul
      (for [instruction (:instructions rp)]
        [:li instruction])]
     [:p (:notes rp)]]))

(defn print-all []
  (for [id (map :id (sql/query db ["SELECT id FROM recipe"]))]
    (print-recipe id db)))

(defn initialize-db []
  (if (zero? (:count (first (sql/query db
                     ["SELECT COUNT (*) FROM information_schema.tables WHERE table_schema = 'public'"]))))
    (do
      (sql/execute! db [(sql/create-table-ddl  :recipe [[:id :serial "PRIMARY KEY"]
                                                        [:title "VARCHAR"]
                                                        [:notes "VARCHAR"]
                                                        [:instructions "VARCHAR"]])])
      (sql/execute! db [(sql/create-table-ddl :ingredients [[:id :serial "PRIMARY KEY"]                                                                                                     [:ing_name "VARCHAR" ]])])
      (sql/execute! db  (sql/create-table-ddl :ing_qty [[:recipe_id :int "REFERENCES recipe"]
                                                        [:ing_id :int "REFERENCES ingredients"]
                                                        [:unit "VARCHAR"]
                                                        [:qty :int]])))))

(defn write-sample-file []
  (spit "/home/chris/recipes2.txt" (prn-str sample-recipe)))
(defn save-recipe [recipe]
  ;; takes a data structure like sample-recipe
  ;; check for ingredient, add to ingredient table
  ;; increment last recipe id
  (let [{:keys [title notes ingredients instructions]} recipe]
    (sql/insert! db :recipe {:title title
                             :notes notes
                             :instructions (prn-str instructions)})
    (let [newid (:max (first (sql/query db ["SELECT MAX(id) FROM recipe WHERE title=?" title])))]
      (for [ingredient ingredients]
        (do
          (if (empty? (sql/query db ["SELECT 1 FROM ingredients WHERE ing_name=?" (:ing_name ingredient)]))
            (sql/insert! db :ingredients {:ing_name (:ing_name ingredient)}))
          (let [ing_id (:id (first (sql/query db ["SELECT id FROM ingredients WHERE ing_name=?" (:ing_name ingredient)])))]
            (sql/insert! db :ing_qty {:recipe_id newid
                                      :ing_id ing_id
                                      :unit (:unit ingredient)
                                      :qty (:qty ingredient)})))))))


(defn get-recipe [id]
  (merge
   (first (sql/query db ["SELECT title, notes FROM recipe WHERE id=?" id]))
         {:ingredients (into [] (sql/query db ["SELECT ing_name, unit, qty FROM ing_qty JOIN ingredients ON ing_qty.ing_id=ingredients.id WHERE recipe_id=?" id]))}
         {:instructions (read-string (:instructions (first (sql/query db ["SELECT instructions FROM recipe WHERE id=?" id]))))}))

(defn pluralize [unit qty]
  (if (not (or (= qty 1) (nil? qty)))
    (str unit "s")
    unit))

(defn print-recipe [rp]
  [:div
   [:h3 (:title rp)]
   [:table
    (for [ingredient (:ingredients rp)]
      [:tr
       [:td (:qty ingredient)]
       [:td (if (empty? (:unit ingredient))
              [:td (:ing_name ingredient)]
              [:td (str  (pluralize (:unit ingredient) (:qty ingredient)) " "(:ing_name ingredient))]) ]
       ])]
   [:ul
    (for [instruction (:instructions rp)]
      [:li instruction])]
   [:p (:notes rp)]])

(defn print-all []
  (for [id (map :id (sql/query db ["SELECT id FROM recipe"]))]
    (print-recipe (get-recipe id))))

(defn parse-inglist [inglist]
  (into [] (for [ing (str/split inglist #"\n")]
             {:ing_name (second (re-find #"(?:(?:.+ of )|(?:\d+ *))(\w.+)" ing))
                      :unit (inf/singular (second (re-find #"^(?:\d+ ){1}(.+)(?= of )" ing)))
                      :qty  (Integer. (first (re-find #"(^\d+)" ing)))})))

(defn parse-recipe [text]
  (let [txt (str/replace (str/replace text #" *\t+ *" " ")  #"( *\n+)" "\n")]
    {:title (second (re-find #"(?:\:title\n* *)(.+\b)" txt))
     :notes (second  (re-find #"(?:\:notes\n* *)(.+\b)" txt))
     :ingredients  (parse-inglist (second (re-find #"(?:\:ingredients.*\n)((?:.|\n)+\b)(?=\n+\:)" txt)))
     :instructions (str/split (second (re-find #"(?:\:instructions.*\n+)(\b(?:.|\n)+)" txt)) #"\n")}))

(defn import-txt-recipes [file]
  (for [rep (str/split (slurp file) #"----")]
     (save-recipe (parse-recipe rep))))

(defn super-parser [text]
  ;; (spit "/home/chris/recipe_parsed.txt" (str/replace text #"\<span class=\"T(?:1|3)\"\>" "\n:title "))
  ;; (spit "/home/chris/recipe_parsed.txt" (str/replace text #"(?m)(^.*(?:&nbsp; ){2,}\w+.*$)" ":ingredients\n$1"))
  ;; (spit "/home/chris/recipe_parsed.txt" (str/replace text #"<.?span.*?\>" ""))
  ;; (spit "/home/chris/recipe_parsed.txt" (str/replace text #" class=\".+?(?=\>)" "")))
  ;; (spit "/home/chris/recipe_parsed.txt" (str/replace text #"(&nbsp; ){2,}(?=\d)" "</p>\n<p>")))
  (reduce str/replace text ???)
