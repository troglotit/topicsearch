(ns troglotit.topicsearch.web.controllers.search
  (:require
    [ring.util.http-response :as http-response]
    [hato.client :as hc]
    [cheshire.core :as json]
    [clojure.set :refer [intersection]]))

; http://localhost:8080/search?tag=clojure&tag=scala
; https://api.stackexchange.com/2.2/search?pagesize=100&order=desc&sort=creation&tagged=clojure&site=stackoverflow

(def url "https://api.stackexchange.com/2.2/search?pagesize=100&order=desc&sort=creation&site=stackoverflow")

(defn call-stackoverflow
  [tag]
  (let [{body :body} (hc/get url {:query-params {:tagged tag}})
        body (json/parse-string body)       
        questions (get body "items")]
    questions))

(defn ->final-map
  [final-map tags question]
  (let [question-tags (get question "tags")
        modified-tags (intersection (set question-tags) tags)]
    (reduce
      (fn [final-map tag]
        (let [totals (get final-map tag)
              new-totals (update totals :total inc)
              new-totals (if (get question "is_answered")
                           (update new-totals :answered inc)
                           new-totals)]
          (assoc final-map tag new-totals)))
                                          
      final-map
      modified-tags)))
  
(defn search
  [{{tags :tag} :params}]
  (let [
        tags (if (vector? tags) tags [tags])
        ; initialize data structures
        question-ids #{}
        final-map (reduce 
                    (fn [acc curr] 
                      (conj acc [curr {:total 0 :answered 0}])) 
                    {} 
                    tags)
        tags (set tags)
        ; calls api
        questions (mapv call-stackoverflow tags)
        questions (apply concat questions)
        ;reduce questions to result
        [_ final-map] 
        (reduce
          (fn [[question-ids final-map] q]
            (let [id (get q "question_id")]
             (if (question-ids id)
               [question-ids final-map]
               [(conj question-ids id)
                (->final-map final-map tags q)])))
          [question-ids final-map]
          questions)]
    final-map
    (http-response/ok final-map)))
   
(comment
  (search {:params {:tag ["scala" "scala-native"]}})
  (search {:params {:tag ["clojure" "clojurescript" "python"]}})
  (call-stackoverflow "scala"))

