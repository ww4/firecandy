(ns recipes.core
  (:gen-class))

(require '[clojure.java.io :as io]

         '[clojure.java.jdbc :as sql]

         '[clojure-csv.core :as csv]

         '[semantic-csv.core :as sc]

         '[hiccup.core :as hic])

(def recipe-db {
         :dbtype "postgresql"
         :dbname "test"
         :user "chris"
         :password "recipe"})

(def family-recipes  ;;initial template for data structure
  {:ID123
   {:title "macaroni"
    :ingredients
    [{:ing-name "noodles" :unit "cup" :qty 2}
     {:ing-name "cheese" :unit "cup" :qty 3}]
      :instructions
       ["cook noodles"
        "add cheese"
        "enjoy"]}
   :ID101
   {:title "Skillet Chicken Macaroni"
    :ingredients
    [{:ing-name "butter" :unit "stick" :qty 1}
     {:ing-name "elbow macaroni" :unit "cup" :qty 6}
     {:ing-name "grated cheddar cheese" :unit "handful" :qty 1}
     {:ing-name "onion" :unit "" :qty 2}
     {:ing-name "diced cooked chicken" :unit "cup" :qty 2}
     {:ing-name "tomato sauce" :unit "15 oz can" :qty 3}]
    :instructions
    ["Sauté onion in butter in very large skillet." "Mix dry macaroni in butter and onion till coated and yellowed." "Add tomato sauce and water and seasonings." "Cover and simmer 15 minutes, stirring occasionally. Add water if needed." "Add chicken that has been pulled apart.  Cook 5 minutes more." "Cover with grated cheese and simmer until cheese melts."]
    :notes "Serves 14-16. This is tripled from the original recipe."}
   })

(defn save-recipe [recipe db]
  ;;takes a map structure like output of get-recipe
  ;;check for ingredient, add to ingredient table
  ;;increment last recipe id
  (let [newid (inc (:max (first (sql/query db "SELECT MAX(id) FROM recipe"))))]
    (sql/insert! db :recipe {:title (:title recipe)
                             :id newid})
    (sql/insert! db :instructions (for [instruction (:instructions recipe)]
                      {:recipe_id newid, :step_number n :instruction instruction} )) ;;get counter working
    (sql/insert! db :ing_qty (for [ingredient (:ingredients recipe)]
                      ))))

(defn get-recipe [id db]
  (merge
   (first (sql/query db ["SELECT title, notes FROM recipe WHERE id=?" id]))
         {:ingredients (into [] (sql/query db ["SELECT ing_name, unit, qty FROM ing_qty JOIN ingredients ON ing_qty.ing_id=ingredients.id WHERE recipe_id=?" id]))}
         {:instructions (into [] (for [instruction (sql/query db ["SELECT step_number, instruction FROM instructions WHERE recipe_id=?" id])] (:instruction instruction)))}
         )
  )

(defn pluralize [unit qty]
  (if (not (or (= qty 1) (nil? qty)))
    (str unit "s")
    unit))

(defn print-recipe [id db]
  (let [rp (get-recipe id db)]
    [:div
     [:h3 (:title rp)]
     [:table
      (for [ingredient (:ingredients rp)]
        [:tr
         [:td (:qty ingredient)]
         [:td (if (empty? (:unit ingredient))
                [:td (pluralize (:ing_name ingredient) (:qty ingredient))]

                [:td (str  (pluralize (:unit ingredient) (:qty ingredient)) " "(:ing_name ingredient))]) ]
         ])]
     [:ul
      (for [instruction (:instructions rp)]
        [:li instruction])]
     [:p (:notes rp)]]))

(defn print-all [db]
  (for [id (map :id (sql/query db ["SELECT id FROM recipe"]))]
    (print-recipe id db)))
