(ns comforter.core
  (:require [spork.util 
             [table :as tbl]
             [io :as io]
             [diff :as diff]]
            [spork.cljgui.components [swing :as swing]]
            [marathon [schemas :as schemas]]))           

(def cost-schema 
  {:SRC      :text
   :Cost     :number
   :Str      :number})

(defn lookup-by [k xs]
  (into {} (map (fn [r] 
                  [(k r) r])) xs))

(defn load-costs [root]
  (->> (-> root
           (tbl/tabdelimited->records :schema cost-schema))
       (into [])
       (lookup-by :SRC)))

(def case-schema 
  {:Case             :text 
   :SupplyPath       :text 
   :RequirementsPath :text})

(defn load-cases [root]
  (->> (-> root 
           (tbl/tabdelimited->records
             :schema case-schema :relaxed? true))
       (into [])))

(defn diff [l r]
  (let [res (diff/diff-map l r)]
    (when (some seq (vals res))
      res)))

(defn diff->deltas [l r {:keys [dropped changed added]}]
  (apply concat
    (for [k dropped]
      [k (- (l k))])
    (for [[k [old new]] changed]
      [k (- new old)])
    added))

(defn load-supply [p] 
  (into [] (filter #(let [e (:Enabled %)]
                      (or (= e true) 
                          (= (clojure.string/upper-case e) "TRUE"))))
    (tbl/tabdelimited->records p 
      :schema (schemas/get-schema :SupplyRecord) 
      :relaxed? true)))

(defn compare-supply 
  "Compute a delta between two sequences of supply records.
   This will be a sequence of [[SRC Compo] +/-number], 
   indicating the distance between supplies."
  [l r]
  (let [keyf (juxt :SRC :Component)
        extract (fn [xs] 
                  (into {} (map (fn [r] [(keyf r) (:Quantity r)]) xs)))
        ls  (extract l)
        rs  (extract r)]
    (->> (diff ls rs)
         (diff->deltas ls rs))))

(defn cost-diff [src->costs xs]
  (for [[[src component] delta] xs]
    (let [ucost (-> src src->costs :Cost)
          str   (-> src src->costs :Str)]
      {:SRC src :Component component :Delta delta :Cost ucost :Str str 
       :TotalCost (* delta ucost) :TotalStr (* str delta)}))) 

(defn process-cases [cases src->costs]
  (apply concat 
    (for [{:keys [Case SupplyPath RequirementsPath] :as c} cases]
      (do (println [:running-case c])
        (try (->> (compare-supply 
                    (load-supply (io/file-path SupplyPath))
                    (load-supply (io/file-path RequirementsPath)))
                  (cost-diff src->costs)
                  (map #(merge c %)))
          (catch Exception e (throw (ex-info :error-processing-case c))))))))

(defn spit-comforter [root])
  
(comment ;;testing
  (def p 
    (io/file-path "~/Documents/TAA-2226/AUDIT_SupplyRecords.txt"))
  (def rp 
    (io/file-path "~/Documents/TAA-2226/requirements.txt"))
  (def cp 
    (io/file-path "~/Documents/TAA-2226/cost.txt"))
  (def casep 
    (io/file-path "~/Documents/TAA-2226/cases.txt")))



    
        
  
  
  
  
  
  
  
  

(defn gui [& args]
  (println "hello!"))


