(ns carve.main
  (:require
   [carve.impl :as impl]
   [clj-kondo.core :as clj-kondo]
   [clojure.edn :as edn]
   [clojure.set :as set]))

(set! *warn-on-reflection* true)

(defn ns+name [ep] [(symbol (namespace ep)) (symbol (name ep))])

(defn unused-vars-data [removed ignore-vars ignore-from-config paths analysis
                        {:keys [var-definitions var-usages] :as analysis}]
  (let [ignore-from-config (map ns+name ignore-from-config)
        ignore (map ns+name ignore-vars)
        definitions-by-ns+name (impl/index-by (juxt :ns :name) var-definitions)
        defined-vars (set (map (juxt :ns :name) var-definitions))
        used-vars (->> var-usages (remove :recursive) (map (juxt :to :name)) set)]
    (->> [ignore-from-config ignore removed]
         (reduce into used-vars)
         (set/difference defined-vars)
         (map definitions-by-ns+name)
         (group-by :filename))))

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
  (let [{:keys [:ignore-vars
                :paths
                :carve-ignore-file
                :aggressive?]} (parse-opts (edn/read-string opts))]
    (when (empty? (:paths opts)) (throw (ex-info ":paths must not be empty" opts)))
    (loop [removed #{}]
      (let [unused-vars-data (foo removed ignore-vars
                                  (impl/read-carve-ignore-file carve-ignore-file)
                                  (clj-kondo/run! {:lint paths :config {:output {:analysis true}}}))]
        (when (seq unused-vars-data)
          (doseq [[file vs] unused-vars-data]
            (impl/carve file vs opts))
          (when aggressive?
            (recur (into removed unused-vars))))))))
