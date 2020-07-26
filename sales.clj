
(def Record_customer {})
(def Record_product {})
(def Record_sale {})
(def Record_customer_id_name {})
(def Record_product_id_name {})
(def Record_product_id_cost {})
(def Record_sale_pi {})
(def Record_sale_ci {})
(def Record_sale_count {})
(def Record_sale_pi_2 {})
(def Record_product_id_name {})
(def total_product_count_Map {})
(def Record_sale_count_2 {})
(def coun 1)
(def coun1 1)
(def rec {})
(def rec_p {})
(def sale {})
(def total_Sales_cust_Map {})
(def names2 {})
(def names3 {})


(def rec_id_name)
(def rec_id_product)



(defn format_customer[x]
  (def df (clojure.string/split x #"\|"))
  ;(println df)
  (def value1 (str "[" (get df 1) "," (get df 2) "," (get df 3) "]"))
  value1
  )

(defn format_product [x]
  (def df1 (clojure.string/split x #"\|"))
  (def value2 (str  "[" (get df1 1) "," (get df1 2) "]"))
  value2
  )

(defn format_ci [x]
  (def df2 (clojure.string/split x #"\|"))
  (def value3 (str  (get df2 1)  ))
  (def names2 (assoc names2 value3 3))
  value3
  )

(defn format_pi [x]
  (def df3 (clojure.string/split x #"\|"))
  (def value4 (str  (get df3 1)  ))
  (def names3 (assoc names3 value4 3))
  value4

  )

(defn format_sale [x]
  (def df4 (clojure.string/split x #"\|"))
  (def value5 (str "[" (get rec_id_name (Integer/parseInt (get df4 1))) "," (get rec_id_product (Integer/parseInt (get df4 2))) "," (get df4 3) "]"))
  value5
  )

(defn format_pn [x]
  (def df (clojure.string/split x #"\|"))
  (def value (get df 1))
  value
  )

(defn format_pc [x]
  (def df (clojure.string/split x #"\|"))
  (def value (get df 2))
  value
  )

(defn format_scoI [x]
  (def df (clojure.string/split x #"\|"))
  (def value (get df 3))
  value
  )

(defn format_spI [x]
  (def df (clojure.string/split x #"\|"))
  (def value (get df 2))
  value
  )

(defn format_scI [x]
  (def df (clojure.string/split x #"\|"))
  (def value (get df 1))
  value
  )

(defn findcucost [p x y z a b n]

  (def ss (nth p n))
  (def sc (Integer/parseInt (get x ss)))
  (def name2 (get a sc))
  ;(println name)

  (def sp (Integer/parseInt (get y ss)))

  (def pcost (get b sp))


  (def  countp (get z ss))
  ;(println countp)
  (def purchase (* (Float/parseFloat pcost)  (Float/parseFloat countp)))
  ;(println purchase)
  (def totalPurchases (+ purchase (get total_Sales_cust_Map name2 0)))
  (def total_Sales_cust_Map (assoc total_Sales_cust_Map name2 totalPurchases))
  ;(println total_Sales_cust_Map)

  (if (< n (- (count p) 1))
    (findcucost p x y z a b (inc n))
    )

  )

(defn findcucost2 [key1 spi pi scounti nvalue]

  (def ss1 (nth key1 nvalue))
  (def sc1 (Integer/parseInt (get spi ss1)))
  ; (println sc1)
  (def name1 (get pi sc1))
  ; (println name1)
  (def  countp1 (get scounti ss1))
  ; (println countp1)
  (def total_product_count (+ (Integer/parseInt countp1) (get total_product_count_Map name1 0)))
  (def total_product_count_Map (assoc total_product_count_Map name1 total_product_count))





  (if (< nvalue (- (count key1) 1))
    (findcucost2 key1 spi pi scounti (inc nvalue))
    )

  )

(defn cim [n file mapempty format]

  (let [file1 (slurp file)]
    (let [ final_file (nth (clojure.string/split-lines file1) n)]
      (let [key1 (nth (clojure.string/split final_file #"\|" 2)0)]
        (let [val (format final_file)]
          (let [mapempty (assoc mapempty (Integer/parseInt key1) val)]
            (let [n (inc n)]
              (if (< n (count (clojure.string/split-lines file1)))
                (cim n file mapempty format) (into  mapempty)
                ))))))))

(defn format_print [na map ke]

  (print (nth ke na))
  (print ":")
  (print (get map (nth ke na)))
  (println " ")
  (if (< na (- (count ke) 1))
    (format_print (inc na) map ke)
    ))


(defn option1 []
  (def rec (cim 0 "cust.txt" Record_customer format_customer ))
  ;(def sort_Record (into (sorted-map) rec))
  ;(def k_c (keys sort_Record))
  ;(format_print 0 sort_Record k_c)

  )

(option1)

(defn calloption1 []
  (def sort_Record (into (sorted-map) rec))
  (def k_c (keys sort_Record))
  (format_print 0 sort_Record k_c)

  )
;(println "------------------------------------------------")

(defn option2 []
  (def rec_p (cim 0 "prod.txt" Record_product format_product ))
  ;(def sort_Record_p (into (sorted-map) rec_p))
  ;(def k_p (keys sort_Record_p))
  ;(format_print 0 sort_Record_p k_p)

  )
(option2)

(defn calloption2 []
  (def sort_Record_p (into (sorted-map) rec_p))
  (def k_p (keys sort_Record_p))
  (format_print 0 sort_Record_p k_p)

  )
;(println "------------------------------------------------")

(defn option3 []
  (def rec_id_name (cim 0 "cust.txt" Record_customer_id_name  format_ci))
  (def rec_id_product (cim 0 "prod.txt" Record_product_id_name format_pi))
  (def sale (cim 0 "sales.txt" Record_sale  format_sale) )
  ;(def sort_Record_s (into (sorted-map) sale))
  ;(def k_s (keys sort_Record_s))
  ;(format_print 0 sort_Record_s k_s)


  )
(option3)
(defn calloption3 []
  (def sort_Record_s (into (sorted-map) sale))
  (def k_s (keys sort_Record_s))
  (format_print 0 sort_Record_s k_s)

  )

;(println "------------------------------------------------")
;(option1)

(defn option4 []

  (def rec_saleid_ci (cim 0 "sales.txt" Record_sale_ci format_scI))
  (def rec_saleid_pi (cim 0 "sales.txt" Record_sale_pi format_spI))
  (def rec_saleid_count (cim 0 "sales.txt" Record_sale_count format_scoI))
  (def rec_id_name1 (cim 0 "cust.txt" Record_customer_id_name format_ci))
  (def rec_p_id_cost (cim 0 "prod.txt" Record_product_id_cost format_pc))
  (def k_ss (keys rec_saleid_ci))
  (def sort_c (into (sorted-map) rec_saleid_ci))
  (def hope (findcucost k_ss rec_saleid_ci rec_saleid_pi rec_saleid_count rec_id_name1 rec_p_id_cost 0))


  ;(println total_Sales_cust_Map)
  ;(def check1 (/  (get total_Sales_cust_Map inputname coun)))
  ;(inc coun)
  )
(option4)
;(println "------------------------------------------------")


(defn check2 [n1 ddd]
  (def gv (nth ddd n1))
  (def names2 (assoc names2 gv n1))
  (n1 (inc n1))
  (if (< n1 (- (count ddd) 1))
    (check2 (inc n1) ddd)
    )


  )
(defn calloption4 []

  (println "Enter a customer name :")
  (def inputname (read-line))
  (if (contains? total_Sales_cust_Map inputname)

    (do

      (println (str inputname " : $" (format "%.2f" (get total_Sales_cust_Map inputname))))
      true)
    (do
      (if (contains? names2 inputname)
        (do
          (println inputname ":" "$" "0.00" )
          true)
        (do
          (println inputname)
          (println " customer not found !!!!!")

          )


        )



      )

    )

  )

(defn option5 []

  (def rec_saleid_pi1 (cim 0 "sales.txt" Record_sale_pi_2 format_spI))
  (def rec_saleid_count1 (cim 0 "sales.txt" Record_sale_count_2 format_scoI))
  (def rec_p_id_name1 (cim 0 "prod.txt" Record_product_id_name format_pn))
  (def k_ss2 (keys rec_saleid_pi1))
  (def sort_c2 (into (sorted-map) rec_saleid_pi1))
  (def hope2 (findcucost2 k_ss2 rec_saleid_pi1 rec_p_id_name1 rec_saleid_count1 0))
  ;(println total_product_count_Map)
  )

(option5)
(defn calloption5 []
  (println "Please enter product name :")
  (def productName (read-line))
  (if (contains? total_product_count_Map productName)

    (do

      (println (str productName" : " (get total_product_count_Map productName)))
      true)
    (do
      (if (contains? names3 productName)

        (do
          (println productName ":" "0")
          true
          )
        (do
          (println "Product not found !!!!!!!"))

        )



      )

    )

  )
(defn menu []
  (println "*** Sales Menu ***")
  (println "------------------")
  (println "")
  (println "1. Display Customer Table")
  (println "2. Display Product Table")
  (println "3. Display Sales Table")
  (println "4. Total Sales for Customer")
  (println "5. Total Count for Product")
  (println "6. Exit")
  (println "")

  )
;(menu)
(defn selection []

  (println "")
  (println "")

  (menu)
  (println "Enter an option?")
  (let [selection_val (read-line) ]
    (case selection_val
      "1" (calloption1)
      "2" (calloption2)
      "3" (calloption3)
      "4" (calloption4)
      "5" (calloption5)
      "6" (    (println "Good Bye..!!")
           (System/exit 0))
      (println "Please select From option 1 to 6.")

      )
    )

  (selection)
  )
(selection)