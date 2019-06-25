(ns comforter.summary
  (:require [comforter.core :as c]
            [spork.util.table :as tbl]
            [spork.util.excel.core :as xl]))

;;Purpose of this namespace is to put the COMFORTER table created by
;;comforter.core into an excel workbook along with another sheet that
;;summarizes the sum of additional personnel and additional cost for
;;each case. Trades include negatives in the sum and without trades
;;does not include negatives.

(def schema (assoc (zipmap c/all-fields
                           (repeat (count c/all-fields) :text))
                   "In Demand?" :boolean
                   "RC Only?" :boolean))

(defn commarize
  "given the string representation of a number, insert a comma after
  every three digits."
  [num-str]
  ;;reverse the string since the first comma starts from the right
  ;;side
  (let [st (reverse num-str)]
    (->>
     ;;reduce over the range so that we can test the digit position
     (count st)
     (range)
     ;;build up a list so that we are conjing to the front and the
     ;;digits are now back in order
     (reduce (fn [acc c] (if (and (> c 0) (= (mod c 3) 0))
                           ;;insert a comma every three digits, but we
                           ;;don't need a comma after the first digit
                           ;;when c=0 and (mod c 3) = 0
                           (conj acc (str (nth st c) ","))
                           (conj acc (nth st c)))) '())
     ;;turn our list back into a string
     (reduce str))))

(defn output-records
  "Root is where output.txt lives.  returns records for the summary table."
  [root & {:keys [output-filter] :or {output-filter (fn [r] true)}}]
  (let [crecs
        (->>
         (tbl/tabdelimited->records (str root "output.txt")
                                              :schema schema :keywordize-fields? false)
                   (into [])
                   (filter output-filter)
                   (filter (fn [r] (r "In Demand?")))
                   (map (fn [r]
                          (assoc r
                                 "Additional Cost"
                                 (if (= "-" (r "Additional Cost"))
                                   0
                                   (read-string
                                    (r "Additional Cost")))
                                 "Additional Personnel"
                                 (if (= "-"
                                        (r "Additional Personnel"))
                                   0
                                   (read-string
                                    (r "Additional Personnel")))))))]
    (->> (for [[g recs] (group-by (fn [r] (r "Case")) crecs)
               :let [{_false "_false"
                      _true "_true"} (group-by (fn [r] (str "_" (neg?
                                                                 (r "Additional Cost")))) recs)
                     compute-result (fn [k recs] (reduce + (map (fn
                                                                  [r] (r k)) recs)))
                     cost-without-trades (compute-result
                                          "Additional Cost" _false)
                     personnel-without-trades (compute-result
                                               "Additional Personnel" _false)
                     comma-num (comp commarize str)]]
           [{:case g :measure "Additional Personnel" :trades? "no"
             :result (comma-num personnel-without-trades)}
            {:case g :measure "Additional Cost" :trades? "no"
             :result (str "$" (comma-num cost-without-trades))}
            {:case g  :measure "Additional Personnel" :trades? "yes"
             :result (->> (compute-result "Additional Personnel"
                                          _true)
                          (+ personnel-without-trades)
                          (comma-num))}
            {:case g :measure "Additional Cost" :trades? "yes"
             :result (->> (compute-result "Additional Cost" _true)
                          (+ cost-without-trades)
                          (comma-num)
                          (str "$"))}])
         (reduce concat))))

;;We assume that the SRC for the BCT HQ represents the entire BCT in
;;terms of cost and personnel, so all children of the BCTs should be
;;removed except for the HQ.

(def bct-elements #{"05315K700"
                    "06325K000"
                    "07195K000"
                    "17195K000"
                    "63055K000"
                    "05315K600"
                    "06235K100"
                    "07215K000"
                    "17215K000"
                    "63035K000"
                    "05315K500"
                    "06385K000"
                    "07315K000"
                    "07315K100"
                    "17315K000"
                    "63025K000"
                    ;;merged AIBCT demand with IBCT.
                    "77202K1000"})

;;all field names where data type is a number.  I believe this was
;;used as a hack since there was an error being thrown writing a
;;number to a cell if it was still a string.
(def number-headers (remove (fn [s] (= "Why no Requirements?" s))
                            (take-last 15 c/rest-of-fields)))

(defn summarize-results
  "puts results into an Excel workbook"
  [root & {:keys [output-filter] :or {output-filter (fn [r] true)}}]
  (let [stat-table (tbl/stringify-field-names
                    (tbl/order-fields-by [:case :measure :trades?
                                          :result]
                                         (tbl/records->table
                                          (output-records root
                                                          :output-filter
                                                          output-filter))))
        comforter-table (->> (tbl/tabdelimited->records
                             (str root "output.txt") :schema schema
                             :keywordize-fields? false)
                             (into [])
                             (filter output-filter)
                             (map (fn [r] (reduce (fn [acc n] (if
  (not= (acc n) "-") (assoc acc n (read-string (acc n))) acc )) r
                                                  number-headers)))
                             (tbl/records->table)
                             (tbl/order-fields-by (into [] c/all-fields)))
        ]
    (xl/tables->xlsx (str root "COMFORTER_Results.xlsx") {"Summary"
  stat-table "RawData" comforter-table })))
    
                                   
                                         
                                         
                                         
                                         
                                         
                                         
