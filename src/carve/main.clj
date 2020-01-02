(ns carve.main
  (:require
   [carve.impl :as impl]
   [clojure.edn :as edn]
   [clojure.set :as set]))

(set! *warn-on-reflection* true)

(defn parse-opts [opts]
  (-> opts
      (update :api-namespaces set)
      (update :carve-ignore-file
              (fn [ci]
                (if (nil? ci) ".carve_ignore"
                    ci)))))

(defn -main [& [flag opts & _args]]
  (when-not (= "--opts" flag)
    (throw (ex-info (str "Unrecognized option: " flag) {:flag flag})))
  (let [opts (parse-opts (edn/read-string opts))]
    (when (empty? (:paths opts)) (throw (ex-info ":paths must not be empty" opts)))
    (if-not (:dry-run? opts)
      (impl/carve! #{} opts)
      (impl/report! opts))))
