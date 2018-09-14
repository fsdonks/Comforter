(ns Comforter.core
  (:gen-class)
  (:import  [javax.swing JFrame JFileChooser])
  (:require [clojure.java [io :as io]]))

;; ===== FUNCTIONS TO READ AND FORMAT INPUTS ===================================

;;Returns num from string without throwing errors
(defn read-num [string]
  (if string
    (if (= "" string)
      0
      (let [num (fn [string] (apply str (map #(re-matches #"[\d.]*" %) (map str string))))
            n (clojure.string/split (num string) #"[.]")  t (take 2 n) b (drop 2 n)
            d (read-string (str (first t) "." (second t) (apply str b)))]
        (if (zero? (- d (int d))) (int d) d)))
    0))

(defn filter-empties [lines]
  (filter #(not (every? (fn [x] (= "" x)) %)) lines))

;;Reads first line of file and maps column name to index number
(defn read-header [file]
  (let [h (with-open [r (clojure.java.io/reader file)]
            (clojure.string/split (first (line-seq r)) (re-pattern "\t")))]
    (zipmap h (range (count h)))))

;; Returns map of supply lines grouped by component
(defn read-supply [supplyfile]
  (let [header (read-header supplyfile) compos ["AC" "NG" "RC"]]
    (zipmap compos (for [c compos]
                    (filter #(= c (nth % (get header "Component")))
                     (with-open [r (clojure.java.io/reader supplyfile)]
                       (into [] (filter-empties
                                 (pmap #(clojure.string/split % (re-pattern "\t"))
                                       (line-seq r))))))))))

;; Returns map of demands lines for all demands and true demands
(defn read-demand [demandfile]
  (let [header (read-header demandfile)
        all-demands (with-open [r (clojure.java.io/reader demandfile)]
                      (into [] (filter-empties
                                (pmap #(clojure.string/split % (re-pattern "\t"))
                                      (line-seq r)))))]
    {"AllDemands"  all-demands
     "TrueDemands" (filter #(= "true" (.trim
                                       (.toLowerCase (nth % (get header "Enabled")))))
                           all-demands)}))

;; Returns a map with SRC as key and the cost of a single unit of that SRC was
;; the value.
(defn read-costs [costfile]
  (let [header (read-header costfile)]
    (with-open [r (clojure.java.io/reader costfile)]
      (let [lines (filter-empties
                   (pmap #(clojure.string/split % #"\t")
                         (drop 1 (line-seq r))))
            z (zipmap (map #(nth % (get header "src")) lines)
                      (map #(nth % (get header "total_cost")) lines))]
        (zipmap (keys z) (map read-num (vals z)))))))

;; Reads casefile and returns a map with Case and Vignette as key and list of
;; Operations as vals
(defn read-case [casefile]
  (let [header (read-header casefile)
        lines (with-open [r (clojure.java.io/reader casefile)]
                (into [] (filter-empties (map #(clojure.string/split % #"\t")
                                              (line-seq r)))))
        p (partition-by #(list (nth % (get header "Case"))
                               (nth % (get header "Vignette"))) lines)
        vs (for [val p] (map #(nth % (get header "Operation")) val))
        ks (apply concat (for [val p]
                           (set (map #(list (nth % (get header "Case"))
                                            (nth % (get header "Vignette"))) val))))]
    (zipmap ks vs)))

;; Returns map of requiremnt file where SRC is the key and string of quantity is
;; the value
(defn read-reqs [reqfile]
  (let [header (read-header reqfile)]
    (with-open [r (clojure.java.io/reader reqfile)]
      (let [lines (filter-empties
                   (filter #(= "AC" (nth % (get header "Component")))
                      (pmap #(clojure.string/split % #"\t") (line-seq r))))]
        (zipmap
         (map #(nth % (get header "SRC")) lines)
         (map #(nth % (get header "Quantity")) lines))))))

;; Returns seq of maps with keys :case, :reqfile (filepath),
;; and :vals (including case and reqfile)
(defn read-inputfile [inputfile]
  (let [header (read-header inputfile)]
    (with-open [r (clojure.java.io/reader inputfile)]
      (let [lines (filter-empties
                   (into [] (map #(clojure.string/split % #"\t")
                                 (drop 1 (line-seq r)))))]
        (map #(identity {:case (nth % (get header "Case"))
                         :reqfile (nth % (get header "Filepath"))
                         :vals %}) lines)))))

(def branches
  (zipmap ["01" "02" "03" "05" "06" "07" "08" "09" "10" "11" "12" "14"
           "16" "17" "19" "20" "27" "30" "31" "33" "34" "35" "37" "40"
           "41" "42" "43" "44" "47" "51" "52" "53" "55" "63" "77" "87" "90"]

          ["Aviation" "Band" "Chemical" "Engineer" "Fires" "Infantry" "Medical" "Ordance"
           "Quartermaster" "Signal" "Human Resources" "Finance" "Chaplaincy" "Cavalry"
           "Military Police" "Military History" "Judge Advocate General" "Military Intelligence"           "Special Forces" "MISO" "Military Intelligence" "Interpreters" "CBRNE" "Space"
           "Civil Affairs" "Quartermaster" "Maintenance" "Air Defense" "Public Affairs"
           "Stryker Brigade Combat Team" "Mission Command" "Corps Headquarters"
           "Information Operations" "Transportation" "Sustainment"
           "Infantry Brigade Combat Team" "Armor Brigade Combat Team and Divisions"
           "Contracting Branch"]))
;; =============================================================================

;;Returns set of SRCs from demand or supply file given lines and the correct header
(defn get-srcs [lines header]
  (set (map #(nth % (get header "SRC")) lines)))

;; Returns the Branch Code of the 9 digit SRC, where branch code is the first
;; two digits
(defn get-branch-code [src] (apply str (take 2 src)))
;; Returns the English Definition of the branch code from the branches map
(defn get-branch [branch-code branches]
  (get branches branch-code))
;; Returns the Title of the SRC
(defn get-title [src slines sheader]
  (nth (first (filter #(= src (nth % (get sheader "SRC"))) slines))
       (get sheader "OITitle")))
;; Returns non-nil when the Supply SRC is in the Demand file
(defn in-demand? [ssrc dsrcs] (get dsrcs ssrc))

;; Returns non-nil when the SRC is exclusivly in the union of RC and NG (ie SRC
;; is in RC or NG but not AC)
(defn rc-only? [src supply supplyheader]
  (and (not (get (get-srcs (get supply "AC") supplyheader) src))
       (get (get-srcs (concat (get supply "RC") (get supply "NG")) supplyheader)
            src)))

;; Returns cost of one unit of SRC
(defn get-src-cost [src costs] (if (get costs src) (get costs src) 0))

;; Returns the total quantity for the SRC in the supply file
(defn get-src-supply [src supply supplyheader]
  (apply + (map read-string
                (map #(nth % (get supplyheader "Quantity"))
                     (filter #(= src (nth % (get supplyheader "SRC")))
                             (set (apply concat (vals supply))))))))

(defn get-ARNG-supply [src supply supplyheader]
  (apply + (map read-num
             (map #(nth % (get supplyheader "Quantity"))
               (filter #(= src (nth % (get supplyheader "SRC")))
                 (get supply "NG"))))))
(defn get-USAR-supply [src supply supplyheader]
  (apply + (map read-num
                (map #(nth % (get supplyheader "Quantity"))
                     (filter #(= src (nth % (get supplyheader "SRC")))
                             (get supply "RC"))))))
(defn get-RC-supply   [src supply supplyheader]
  (+ (get-USAR-supply src supply supplyheader)
     (get-ARNG-supply src supply supplyheader)))

(defn get-AC-supply   [src supply supplyheader]
  (apply +
         (map read-num
              (map #(nth % (get supplyheader "Quantity"))
                   (filter #(= src (nth % (get supplyheader "SRC")))
                           (get supply "AC"))))))

;; Returns the cost of a single unit of SRC mutliplied by the quantity
(defn get-src-total-cost [src supply costs supplyheader]
  (* (get-src-supply src supply supplyheader) (get-src-cost src costs)))

;; Returns the personnel in a single unit of the SRC
(defn get-src-personnel [src supply supplyheader]
  (apply + (map #(read-num (nth % (get supplyheader "Strength")))
                (filter #(= src (nth % (get supplyheader "SRC")))
                        (apply concat (vals supply))))))

;; Returns the total personnel for the SRC by multiplying the quantity of the
;; SRC by the personnel per SRC
(defn get-src-total-personnel [src supply supplyheader]
  (apply * (map #(% src supply supplyheader) [get-src-personnel get-src-supply])))
;;Returns quantity required (0 if does not exist)
(defn get-requirement [src reqs ] (if (get reqs src) (read-num (get reqs src)) 0))
;; Returns the cost for the required number of SRCs
(defn get-requirement-cost [src req cost] (* req (get-src-cost src cost)))
;; Returns the personnel of the required quantity of SRCs
(defn get-requirement-personnel [src req supply supplyheader]
  (* req (get-src-personnel src supply supplyheader))) 
;;Formats output lines by calling the lookup functions
(defn supply->lines
  [supply supplyheader demand demandheader reqs costs]
  (for [src (get-srcs (apply concat (vals supply)) supplyheader)
        :let [req (get-requirement src reqs)]]
    [(get-branch-code src) ;;Branch Code
     (get-branch (get-branch-code src) branches) ;;Branch
     (try ;;Unit Grouping
       (nth (first (filter #(= src (nth % (get supplyheader "SRC")))
                           (apply concat (vals supply))))
            (get supplyheader "Unit Groupings"))
       (catch Exception e nil))
     src ;;SRC
     (get-title src (apply concat (vals supply)) supplyheader) ;OITitle
     (if (in-demand? src (get-srcs (get demand "TrueDemands") demandheader))
         "TRUE" "FALSE") ;;In-demand?
     (if (rc-only? src supply supplyheader) "TRUE" "FALSE") ;;RC-only?
     (get-ARNG-supply src supply supplyheader) ;;ARNG supply
     (get-USAR-supply src supply supplyheader) ;;USAR supply
     (get-RC-supply src supply supplyheader) ;;RC supply
     (get-AC-supply src supply supplyheader) ;;AC supply
     (get-src-cost src costs) ;;Cost per unit of SRC
     ;;Total cost of SRCs in supply (supply * cost)
     (* (get-src-cost src costs) (get-AC-supply src supply supplyheader))
     ;;Personnel per src
     (get-src-personnel src supply supplyheader)
     ;;Total SRC personnel for AC
     (* (get-src-personnel src supply supplyheader)
        (get-AC-supply src supply supplyheader))
     req ;;Required units of SRC
     (if (= 0 req)
       (let [d (in-demand? src (get-srcs (get demand "TrueDemands") demandheader))]
         (if d "RC met demand" "No demand"))
       "") ;;Why no requirement?
     (get-requirement-cost src req costs) ;;Requirement Cost (req * cost)
     (get-requirement-personnel src req supply supplyheader) ;; Demand Personnel
     (- (get-AC-supply src supply supplyheader) req) ;;Suppply - Demand
     (- (* (get-src-cost src costs) (get-AC-supply src supply supplyheader))
        (get-requirement-cost src req costs)) ;;Marginal Cost
     ;;Marginal Personnel
     ( - (* (get-src-personnel src supply supplyheader)
            (get-AC-supply src supply supplyheader))
      (get-requirement-personnel src req supply supplyheader))]))

;;Reads in inputfiles and formats output lines
(defn post-process->lines [inputfile supplyfile demandfile costfile]
  (let [supply (read-supply supplyfile) supplyheader (read-header supplyfile)
        demand (read-demand demandfile) demandheader (read-header demandfile)
        costs (read-costs costfile) input (read-inputfile inputfile)
        header (apply conj
                      (into [] (sort-by #(get (read-header inputfile) %)
                                        (keys (read-header inputfile))))
                      ["Branch Code" "Branch" "Unit Grouping" "Supply SRCs"
                       "Title" "In Demand?" "RC Only?" "ARNG Supply" "USAR Supply"
                       "RC Supply" "AC Supply" "AC O&S Cost/SRC" "AC Supply Cost"
                       "Personnel/SRC" "AC Supply Personnel" "Requirements"
                       "Why no Requirements?" "Requirement Cost" "Requirement Personnel"
                       "Supply - Requirement" "Additional Cost" "Additional Personnel"])]
      (conj (apply concat
              (for [i input]
                (for [line (supply->lines supply supplyheader demand demandheader
                              (read-reqs (:reqfile i)) costs)]
                  (apply conj (:vals i) line)))) header)))

;;Writes formatted lines to output file.
(defn output-post-process [outputfile lines]
  (with-open [w (clojure.java.io/writer outputfile)]
    (doseq [line lines]
      (doseq [val line]
        (.write w (if (= "0" (str val)) (str "-" "\t") (str val "\t"))))
      (.write w "\r\n"))))

;;GUI menu to select file or directory (returns string/seq of string (filenames))
(defn choose-file [title & dir-only]
  (let [f (javax.swing.JFrame.)
        c (javax.swing.JFileChooser.)]
    (.add f c)
    (.setDialogTitle c title)
    (when dir-only (.setFileSelectionMode c JFileChooser/DIRECTORIES_ONLY))
    (.setMultiSelectionEnabled c true)
    (let [x (.showOpenDialog c f)]
      (if  (zero? x)
        (map #(.getPath ^java.io.File %) (.getSelectedFiles c))
        (println "No file selected.")))))

(defn ->output [] ;;Ask user to select inputs then processes output file
  (let [supplyfile (first (choose-file "SELECT SUPPLY INPUT FILE (.TXT)"))
        demandfile (first (choose-file "SELECT DEMAND INPUT FILE (.TXT)"))
        costfile   (first (choose-file "SELECT O&S COST FILE (.TXT)"))
        inputfile  (first (choose-file
                           "SELECT FILLED OUT TEMPLATE FILE (.TXT) *MAKE SURE THE FILEPATHS TO REQUIREMENT FILES HAS BEEN FILLED OUT."))
        outputfile (str (first (choose-file "SELECT OUTPUT DIRECTORY"
        true)) "/Requirement Analysis Comforter Output.txt")]
    (println "\n\tSupply File:\t"  supplyfile
             "\n\tDemand File:\t"  demandfile
             "\n\tCost File:\t"    costfile
             "\n\tInput File:\t"   inputfile
             "\n\tOutput File:\t" outputfile)
    (time (output-post-process outputfile
              (post-process->lines inputfile supplyfile demandfile costfile)))))

(defn -main [& args]
  (try
    (->output)
    (catch Exception e (println e))
    (finally (System/exit 0))))
