(ns mesh.core-test
  (:require [clojure.test :refer :all]
            [mesh.core :refer :all]
            [spyscope.core]))


(deftest test-file-type-detect
  (testing ".csv"
    (is (= (file-type-detect "test.csv") :csv  )))
  (testing ".tsv"
    (is (= (file-type-detect "test.tsv") :tsv  )))
  (testing ".notfound"
    (is (= (file-type-detect "test..notfound") :file-type-detect-failed  ))))


(deftest test-header-detect 
  (testing "Detect csv header"
    (let [U { :file-type :csv }

           reader (java.io.BufferedReader. (java.io.StringReader. "First Name,Last Name,Credit Rating,Active\n")) 

          detection (field-name-detect U reader)]
     
        (is (= (set  detection) #{:First-Name :Last-Name :Credit-Rating :Active } )))))


(deftest test-merge-formats
  (testing "merge-formats: same type"
     
      (let [C { :formats  [integer-fmt real-fmt string-fmt] }
            new-type [integer-fmt] 
            proposed-type [integer-fmt]] 
       (is (= [integer-fmt] (merge-fmts C new-type proposed-type)))))

  (testing "merge-formats: higher"
     
      (let [C { :formats  [integer-fmt real-fmt string-fmt] }
            new-type      [integer-fmt] 
            proposed-type [real-fmt]] 
       (is (= [real-fmt] (merge-fmts C new-type proposed-type)))))

  (testing "merge-formats: lower"
      (let [C { :formats  [integer-fmt real-fmt string-fmt] }
            new-type [real-fmt] 
            proposed-type [integer-fmt]] 
       (is (= [real-fmt] (merge-fmts C new-type proposed-type)))))



  (testing "merge-formats: many"
      (let [C { :formats  [integer-fmt real-fmt string-fmt] }

            new-type      [string-fmt  integer-fmt real-fmt    string-fmt] 
            proposed-type [integer-fmt real-fmt    integer-fmt string-fmt]] 

       (is (= [string-fmt real-fmt real-fmt string-fmt ] (merge-fmts C new-type proposed-type))))))



(deftest test-detect-field-type-in-str
  (testing "detect integer"
    (let [C {:formats  [integer-fmt real-fmt string-fmt]}]
      
     (is (= integer-fmt (detect-field-type-in-str C "734311"))) 
      ))
  (testing "detect real"
    (let [C {:formats  [integer-fmt real-fmt string-fmt]}]
      
     (is (= real-fmt (detect-field-type-in-str C "3.1415"))) 
      ))
  (testing "detect string"
    (let [C {:formats  [string-fmt real-fmt integer-fmt]}]
      
     (is (= string-fmt (detect-field-type-in-str C "734311a"))) 
      )))



(deftest test-detect-field-type
  (testing "detect integer"
    (let [C {:formats  [integer-fmt real-fmt string-fmt]}]
      
     (is (= integer-fmt (detect-field-type C 734311))) 
      ))
  (testing "detect real"
    (let [C {:formats  [integer-fmt real-fmt string-fmt]}]
      
     (is (= real-fmt (detect-field-type C 3.1415))) 
      ))
  (testing "detect string"
    (let [C {:formats  [string-fmt real-fmt integer-fmt]}]
      
     (is (= string-fmt (detect-field-type C "734311a"))) 
      )))



(deftest test-field-types-detect-reader
  (testing "field types detect"
     (let [C         {:formats  [integer-fmt real-fmt string-fmt] 
                      :file-type :csv }
           reader    (java.io.BufferedReader. (java.io.StringReader.
                         "3,Hello,2,true\n33,Hello,2,true\n3,Hello,2,true\n4443,Hello,2,true\n3243,Hello,11112.1,true\n3,Hello,2.1,true"     
                                             
                                             
                                             )) ]
       
       (is (= [integer-fmt string-fmt real-fmt string-fmt]
              (field-types-detect-reader C reader)  )) 
       )))


(deftest test-field-types-detect
  (testing "field types detect"  ;; TODO add boolean types
     (let [C         {:formats  [integer-fmt real-fmt string-fmt] 
                      :file-type :csv }
           rows       [
                       {:a 3    :b "Hello" :c 2      }
                       {:a 33   :b "Hello" :c 2      }
                       {:a 3    :b "Hello" :c 2      }
                       {:a 4433 :b "Hello" :c 2      }
                       {:a 3243 :b "Hello" :c 2.1    }
                       {:a 3    :b "Hello" :c 2.1    }
                       {:a 3    :b "Hello" :c 1111.2 }
                       
                       ]]
       
       (is (= [integer-fmt string-fmt real-fmt]
              (field-types-detect C rows)  )) 
       )))


(deftest test-format-names
  (testing "string format"
      (is (= "string-fmt" (fmt-name string-fmt))))
  (testing "real format"
      (is (= "integer-fmt" (fmt-name integer-fmt))))
  (testing "integer format"
      (is (= "real-fmt" (fmt-name real-fmt)))))


(deftest test-import-file
  (testing "load csv sample"
     (let [path  (.getPath (clojure.java.io/resource "example1.csv"))
           results (import-file path)]
        (./pp results) 
        ;; TODO Fix test
        (is (= true true)))))
