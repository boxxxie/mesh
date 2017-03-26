(ns mesh.core
  (:require [clj-time.format :as f])
  (:require [spyscope.core]))


;; Boolean hint
;; Date hint

;; Number format

(defprotocol writeable
    (write-int [_ field])
    (write-byte [_ field])
    (write-long [_ field])
    (write-bool [_ field])
    (write-float [_ field])
    (write-double [_ field])
    (write-bytes [_ field]))


(defprotocol readable
    (read-int [_ field])
    (read-byte [_ field])
    (read-long [_ field])
    (read-bool [_ field])
    (read-float [_ field])
    (read-double [_ field])
    (read-bytes [_ field byte-count]))


(defprotocol Format
    (fmt-name [_])
    (coerce [_ field])
    (is-fmt [_ field])
    (in-str [_ field])

    (format-type [_ field])

    (to-bytes [_ field writable ])
    (from-bytes [_ field readable ])
    
    )

(def unknown-fmt 
 (reify Format
      (fmt-name [_] "unknown-fmt")
      (coerce [_ field] field)
      
      (is-fmt [_ field] true  )
      (in-str [_ field] "unknown -99"  )
      
      
      (format-type [_ field] -99)
      (to-bytes [_ field writable] [])
      (from-bytes [_ field readable] [])
      ) 
  
  )

(def nil-fmt
  (reify Format
      (fmt-name [_] "nil-fmt")
      (coerce [_ field] field)
      
      (is-fmt [_ field] (nil? field))
      (in-str [_ field]  (nil? field))
      
      
      (format-type [_ field] 0)
      (to-bytes [_ field writable] [])
      (from-bytes [_ field readable] [])
      ))


(defn boolean? [x]
  (instance? Boolean x))


(defn is-fmt-boolean [string]
  (or (= "true" string) (= "false" string)))


(def boolean-fmt
  (reify Format
      (fmt-name [_] "boolean-fmt")
      (coerce [_ field] (Boolean/parseBoolean field))
      
      (is-fmt [_ field]  (boolean? field))
      (in-str [_ field]  (is-fmt-boolean field))
  
    (format-type [_ field] 1)
    (to-bytes [_ field writable] (if (= true field) [(byte 1)] [(byte 0)]))
    (from-bytes [_ field readable ] (if (= field 1) true false ))))



(def string-fmt
  (reify Format
      (fmt-name [_] "string-fmt")
      (coerce [_ field] field)
      
      (is-fmt [_ field] (string? field))
      (in-str [_ field] (string? field))
   
  
    (format-type [_ field] 3)
    (to-bytes [_ field writable] (.getBytes field))
    (from-bytes [_ field readable] (String. field))))


(def integer-fmt
  (reify Format
      (fmt-name [_] "integer-fmt")
      (coerce [_ field] 
        (try
          (Long/parseLong field)
             (catch NumberFormatException e nil )))
     ;;   (if (= 0 (.length field))
      ;;         nil
       ;;  (Long/parseLong field)))

      (is-fmt [_ field] (integer? field))
      (in-str [_ field] 
        (try
          (integer?  (Long/parseLong field))
             (catch NumberFormatException e false )))
       (to-bytes [_ field writable] (.getBytes field))
       (from-bytes [_ field readable] (String. field))))


(def real-fmt
  (reify Format
      (fmt-name [_] "real-fmt")
      (coerce [_ field] (Double/parseDouble field))
      (is-fmt [_ field] (number? field))
      (in-str [_ field] 
        (try
          (number?  (Double/parseDouble field))
             (catch NumberFormatException e false )))
       (to-bytes [_ field writable] (.getBytes field))
       (from-bytes [_ field readable] (String. field))))


(defn custom-date-fmt [date-fmt]
  (let [custom-fmt (f/formatter date-fmt)]
    (reify Format
      (fmt-name [_] date-fmt)
      (coerce [_ field] (f/parse custom-fmt field ))
      (is-fmt [_ field] (= org.joda.time.DateTime (type field))) 
      (in-str [_ field] 
        (try
          (f/parse custom-fmt field )             
            (catch Exception e false)))
      
       (to-bytes [_ field writable] (.getBytes field))
       (from-bytes [_ field readable] (String. field)))))


(defn standard-date-fmt [date-fmt]
  (let [custom-fmt (f/formatters date-fmt)]
    (reify Format
      (fmt-name [_] date-fmt)
      (coerce [_ field] (f/parse custom-fmt field ))
      (is-fmt [_ field] (= org.joda.time.DateTime (type field))) 
      (in-str [_ field] 
        (try
          (f/parse custom-fmt field )             
            (catch Exception e false)))
      
       (to-bytes [_ field writable] (.getBytes field))
       (from-bytes [_ field readable] (String. field)))))



(defn file-type-detect [filepath] 
  (cond 
      (.endsWith filepath ".csv")  :csv
      (.endsWith filepath ".tsv")  :tsv
      :else :file-type-detect-failed ))



(defn split-regx [C]
   (cond   (= :csv (:file-type C))  #","
          (= :tsv (:file-type C))  #"\t" ))



(defn field-name-detect [C reader]
    (let [the-regx (split-regx C) 

          header  (clojure.string/split 
                    (first (line-seq reader)) the-regx) 

     
          fields  (map (fn [column-name]
                              (keyword (clojure.string/replace column-name #" " "-"))                 
                         ) header) ] 
          fields ))




(defn fields-as-symbols [C reader]
    (let [the-regx (split-regx C) 

          header  (clojure.string/split 
                    (first (line-seq reader)) the-regx) 

          fields  (map (fn [x]
                         (symbol (clojure.string/replace x #"\s*" "" )) 
                         ) header) 
          ] 
          fields ))




(defn detect-field-type-in-str [C string]
    (loop [formats (:formats  C)]
      (let [candidate (first formats )
            valid (in-str candidate string)]
       
         (if valid
           candidate
           (recur  (rest formats))))))



(defn detect-field-type [C field]
    (loop [formats (:formats  C)]
      (let [candidate (first formats )
            valid (is-fmt candidate field)]
       
         (if valid
           candidate
           (recur  (rest formats))))))



(defn merge-fmts [C new-fmts proposed]
   (if (empty? proposed)
      new-fmts
     
     (let [fmt-heirachy (reverse (:formats C)) 
           merged       (map (fn [a b]

             (let [winner (first (drop-while (fn [x] 
                                               (not (or (= x a ) (= x b )))) fmt-heirachy )) ]
               winner)
             ) new-fmts proposed)]

        merged )))




(defn field-types-detect-reader [C reader]
  
     (let [first100 (take 100 (rest (line-seq reader))) 
          the-regex (split-regx C)
          firstparse  (map (fn [row]
                             (let [fields        (clojure.string/split row the-regex)
                                   fields-parsed (doall  (map #(detect-field-type-in-str C %) fields))]

                                   fields-parsed )) first100)
   

        field-types   (loop [input firstparse types []]
                        (if (empty? input)
                         types 
                          (recur (rest input) 
                             (if (empty? types) 
                                (first input) 
                                (merge-fmts C (first input) types)))))]

        field-types ))



(defn field-types-detect [C rows]
  (let [firstparse (map (fn [row]
                           (doall  (map #(detect-field-type C %) (vals row)))
                          ) rows)  
        
    
    
        field-types   (loop [input firstparse types []]
                        (if (empty? input)
                         types 
                          (recur (rest input) 
                             (if (empty? types) 
                                (first input) 
                                (merge-fmts C (first input) types)))))]

        field-types ))
    

(defn hints [hints])


(defn process [C reader]
   (let [lseq (rest (line-seq reader))]
      (doall  (map (fn [line-input]
           (let [the-regex (split-regx C)
                 fields    (clojure.string/split line-input the-regex) ;;TODO handle invalid rows
                 
                 ] 
      
             
             (apply hash-map (mapcat (fn [field f-type f-name]
                     [f-name (coerce f-type field)] 
                    ) fields (:field-types C) (:field-names C)))

             )) lseq ))))


(defn format-names [formats]
   (map fmt-name formats))


(defn parse-date-fmts [date-fmts]
    (map #(custom-date-fmt %) date-fmts))


(defn parse-standard-date-fmts [date-fmts]
    (map #(standard-date-fmt %) date-fmts))


(defn loadfile [path standard-date-fmts date-fmts]
   (with-open [reader (clojure.java.io/reader (clojure.java.io/as-file path))
               reader1 (clojure.java.io/reader (clojure.java.io/as-file path))
               reader2 (clojure.java.io/reader (clojure.java.io/as-file path))
               reader3 (clojure.java.io/reader (clojure.java.io/as-file path))
               
               ]
      (let [C {}
          formats (concat (parse-standard-date-fmts standard-date-fmts) 
                          (parse-date-fmts date-fmts)
                          [integer-fmt real-fmt boolean-fmt string-fmt])

          C (assoc C :formats formats)
          C (assoc C :file-type (file-type-detect path) )

          C (assoc C :field-names (field-name-detect C  reader))
          C (assoc C :field-types (field-types-detect-reader C reader1)) 
          C (assoc C :type-description (map fmt-name (:field-types C))) 



          C (assoc C :data  (process C reader2))


          
          ]
        
       C 
    
    )))


(defn import-file [path]
   (loadfile 
     path
     [:sql :date :date-hour :date-hour-minute :date-hour-minute-second 
      :date-hour-minute-second-fraction :date-hour-minute-second-ms         
      :date-time :date-time-no-ms ]
     [] ))




