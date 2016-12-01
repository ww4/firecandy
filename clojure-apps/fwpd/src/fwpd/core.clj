(ns fwpd.core)
(def filename "suspects.csv")


(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str)
  )

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
((get conversions vamp-key) value)
)

(defn parse
"Convert a CSV string into rows of columns"
[string]
(map #(clojure.string/split % #",")
     (clojure.string/split string #"\n"))
  )

(defn mapify
"Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10"
[rows]
(map (fn [unmapped-row]
       (reduce (fn [row-map [vamp-key value]]
                 (assoc row-map vamp-key (convert vamp-key value)))
               {}
               (map vector vamp-keys unmapped-row)))
       rows)
)

(defn get-records
  []
  (mapify (parse (slurp filename)))
  )

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records)
  )

(defn name-search
  [min]
  (map :name (glitter-filter min (get-records))))

(defn append-suspect
  [suspect glitz]
  (save-csv (cons {:name suspect :glitter-index glitz} (get-records)) "" )
  )


(defn save-csv [input output]
  (if (empty? input)
    (spit filename output)
    (save-csv (rest input)
              (str  output
                    (:name (first input)) ","
                    (:glitter-index (first input)) "\n"
)
)))
