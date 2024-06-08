(ns interview.core
  (:require [clojure.string :as string]
            [clj-http.client :as client]
            [hickory.core :as h]
            [hickory.select :as s]
            [cuerdas.core :as cc]))

; TODO: Figure out why scraping is not working on this site even with working cert
(def saudi-central-bank-url "https://www.sama.gov.sa/en-US/FinExc/Pages/Currency.aspx")

(def bank-of-russia-url "https://cbr.ru/eng/currency_base/daily/")
(def bank-of-russia-table-selector
  (s/descendant (s/class "table-wrapper")
                (s/or (s/tag :th)
                      (s/tag :td))))

(defn- html->hickory
  "Parses site html into a Hickory tree."
  [site-html]
  (-> site-html h/parse h/as-hickory))

(defn- table-contents
  "Selects only contents of a table from Hickory site tree, based on the provided selector/number of columns.
  First seq is the headers, the rest is row contents."
  [selector col-no site-tree]
  (->> (s/select selector site-tree)
       (map (comp first :content))
       (partition col-no)))

(defn- update-value [map cols-to-update f]
  (reduce
    (fn [map col]
      (when (some? (map col))
        (update map col f)))
    map cols-to-update))

(defn- normalize-values
  "Converts values in map only for specific keys."
  [map]
  (let [double-columns [:rate]
        int-columns [:num-сode :unit]]
    (-> map
        (update-value double-columns #(bigdec %))
        (update-value int-columns #(Integer/parseInt %)))))

(defn- table->maps
  "Converts into maps a nested sequence of strings, assuming the first row is a header row."
  [[headers & cols]]
  (let [keys (map cc/keyword headers)]
    (->> cols
         (map #(if (string? %) (string/trim %) %))
         (map #(zipmap keys %))
         (map normalize-values))))

(defn scrap-table-as-maps
  "Parses the result of performing GET on the specified URL, extracts table
  then converts it into a vector of maps."
  [url params table-selector col-no]
  (->> (client/get url {:query-params params :debug true})
       :body
       html->hickory
       (table-contents table-selector col-no)
       table->maps))

(comment
  (def local-site-tree
    (html->hickory (slurp "resources/bank-of-russia-sample.html")))
  (def table-only
    (table-contents bank-of-russia-table-selector 5 local-site-tree))
  (table->maps table-only)

  (= (table->maps [["Num сode" "Char сode" "Unit" "Currency" "Rate"]
                   ["036" "AUD" "1" "Australian Dollar" "57.6127"]
                   ["944" "AZN" "1" "Azerbaijan Manat" "43.9132"]
                   ["051" "AMD" "100" "Armenia Dram" "14.1371"]])
     [{:num-сode 36, :char-сode "AUD", :unit 1, :currency "Australian Dollar", :rate 57.6127M}
      {:num-сode 944, :char-сode "AZN", :unit 1, :currency "Azerbaijan Manat", :rate 43.9132M}
      {:num-сode 51, :char-сode "AMD", :unit 100, :currency "Armenia Dram", :rate 14.1371M}])

  (def result (scrap-table-as-maps bank-of-russia-url
                                   {"UniDbQuery.Posted" "True"
                                    "UniDbQuery.To"     "03/07/2010"}
                                   bank-of-russia-table-selector 5))

  ;List of unit 100 char codes starting with the letter "K
  (->> result
       (filter #(= (:unit %) 100))
       (map :char-сode)
       (filter #(string/starts-with? % "K")))

  ;Show currency name and rate for all entries where exchange rate is higher than 50.
  (->> result
       (filter #(> (:rate %) 50M))
       (sort-by :rate >)
       (map (apply juxt [:currency :rate])))

  )