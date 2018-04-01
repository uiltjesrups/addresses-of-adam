(ns addresses-of-amsterdam.core
  (:require [dk.ative.docjure.spreadsheet :as xls]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

  (def url "https://www.amsterdam.nl/publish/pages/567096/sbk_openbare_ruimte1.xls")

(def number-regex #"([0-9]+)(.*)")

(defn number-splitter [input]
  (let [ match (re-matches #"([0-9]+)(.*)" input)]
    [(second match) (nth match 2)]))

(defn letter->range [b e]
  (if (not (= "" b))
      (-> (map (comp int first) [b e])
          ((fn [[b e]] (range b (inc e))))
          (#(map char %)))
      [""]))

(defn number->range [b e]
  (range (read-string b) (inc (read-string e)) 2))

(defn fill-na-postcode [c] (reverse (reduce #(cons
                                              (if (str/blank? (:postcode %2))
                                                (assoc %2 :postcode (:postcode (first %1)))
                                                %2) %1) (list) c)))

(defn fetch-workbook[address]
  (with-open [stream (.openStream (java.net.URL. address))]
    (let  [addresses (->> (xls/load-workbook-from-stream stream)
                          (xls/select-sheet "sbk_openbare_ruimte")
                          (xls/select-columns {:B :straatnaam
                                               :C :huisnr_min_oneven
                                               :D :huisnr_max_oneven
                                               :E :huisnr_min_even
                                               :F :huisnr_max_even
                                               :G :postcode
                                               }))]
      addresses)))

(defn -main
  ([] (-main url))
  ([url]
   (let [sht (rest (fetch-workbook url))
         min (map #(if (= "" (:huisnr_min_even %))
                     (:huisnr_min_oneven %)
                     (:huisnr_min_even %)) sht)
         max (map #(if (= "" (:huisnr_min_even %))
                     (:huisnr_max_oneven %)
                     (:huisnr_max_even %)) sht)
         sht              (map
                           #(assoc  %1
                                    :min-num (first (number-splitter %2))
                                    :min-let (second (number-splitter %2))
                                    :max-num (first (number-splitter %3))
                                    :max-let (second (number-splitter %3))
                                    )
                           sht min max)
         rows (filter #(not (or (nil? (:min-num %))
                                (nil? (:max-num %)))
                                        ;    (str/blank? (:postcode %))
                                        ;(str/blank? (:straatnaam %))
                            ) sht)
         rows (fill-na-postcode rows)
         nrs (map #(number->range (:min-num %) (:max-num %))  rows)
         lrs (map #(letter->range (:min-let %) (:max-let %))  rows)
         nrs (map #(for [n %1 l %2 ] (str n l)) nrs lrs)
         rows (mapcat #(for [n %1 p [%2] s [%3]] [s n p "Amsterdam"]) nrs
                      (map :postcode rows)
                      (map :straatnaam rows))
         rows (distinct (cons ["straatnaam" "huisnummer" "postcode" "plaats"] rows))]

     (with-open [writer (io/writer "alle-adressen-amsterdam.csv")]
       (csv/write-csv writer rows)))))
