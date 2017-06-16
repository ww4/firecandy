(ns recipes.core
  (:gen-class))

(require  '[clojure.java.io :as io]

          '[clojure.string :as str]

          '[clojure.set :as set]

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

(defn get-unit [str] ;;returns a map like {:qty 2 :unit stick :ing_name butter}
  (first  (set/intersection #{"stick" "c" "can" "slice" "cup" "tsp" "lb" "tbl" "oz" "clove" "box"}
                            (set (map inf/singular (str/split str #" "))))))

(defn vulgfrac [unicode]
  (get {"\u00BA" " degrees"
        "\u00BC" (/ 1 4)
        "\u00BD" (/ 1 2)
        "\u00BE" (/ 3 4)
        "\u2150" (/ 1 7)
        "\u2151" (/ 1 9)
        "\u2152" (/ 1 10)
        "\u2153" (/ 1 3)
        "\u2154" (/ 2 3)
        "\u2155" (/ 1 5)
        "\u2156" (/ 2 5)
        "\u2157" (/ 3 5)
        "\u2158" (/ 4 5)
        "\u2159" (/ 1 6)
        "\u215A" (/ 5 6)
        "\u215B" (/ 1 8)
        "\u215C" (/ 3 8)
        "\u215D" (/ 5 8)
        "\u215E" (/ 7 8)} unicode unicode))


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
  (into [] (for [ing (str/split-lines inglist)]
             {:ing_name (if (get-unit ing)
                          (second (str/split ing (re-pattern (str (get-unit ing) " "))))
                          (inf/singular (second (re-find #"(?:^\d*(?:\/|\d)* *)(.+$)" ing))))
                      :unit (get-unit ing)
              :qty  (read-string (str (first (re-find #"(^\d+(?:\/|\d)*)" ing))))})))

(defn parse-recipe [text]
  {:title (second (re-find #"(?:\:title\n* *)(.+\b)" text))
   :notes (second  (re-find #"(?:\:notes\n* *)(.+\b)" text))
   :ingredients  (parse-inglist (second (re-find #"(?:\:title.*\n+)((?:.|\n)+\b)(?=\n+:instructions)" text)))
   :instructions (str/split (second (re-find #"(^.*\w+\. [A-Z](?:.|\n)*)" text)) #"( +)(?=[A-Z]\w+)")})

(defn super-parser [text]
  (-> text
      (str/replace #"\u00BA|\u00BC|\u00BD|\u00BE|\u2150|\u2151|\u2152|\u2153|\u2154|\u2155|\u2156|\u2157|\u2158|\u2159|\u215A|\u215B|\u215C|\u215D|\u215E" #(str (vulgfrac %1)))
      (str/replace #"\<span class=\"T(?:1|2|3|4|itle)\"\>(?![\<])" "\n\n----\n\n:title " )
      ;; (str/replace #"----(?:.|\n)*" "$1")
      (str/replace #"<.?span.*?\>" "")
      (str/replace #" class=\".+?(?=\>)" "")
      (str/replace #"(&nbsp; ){2,}(?=\d)" "</p>\n<p>")
      (str/replace #" *(&nbsp;+ *)+| {2,}" " ")
      (str/replace #" *<\/*p> *| *\<br\> *" "")
      (str/replace #"(?m)(^\d.*\n*)(^[A-Z])" "$1\n:instructions\n$2")
      (str/replace #"( *\n+)" "\n")
      (str/replace #" *\t+ *" " ")
      (str/replace #"^\d+(?=[^\d\s])" "$1 ")
      (str/replace #"&amp;" "&")
      ))

(defn import-txt-recipes [file]
  (initialize-db)
  (for [rep (rest(str/split (super-parser (slurp file)) #"----"))]
     (save-recipe (parse-recipe rep))))
