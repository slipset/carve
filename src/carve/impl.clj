(ns carve.impl
  {:no-doc true}
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clj-kondo.core :as clj-kondo]
   [rewrite-cljc.node :as node]
   [rewrite-cljc.zip :as z]))

(defn index-by
  [f coll]
  (persistent! (reduce #(assoc! %1 (f %2) %2) (transient {}) coll)))

(defn read-carve-ignore-file [carve-ignore-file]
  (let [ignore-file (io/file carve-ignore-file)]
    (when (.exists ignore-file)
      (edn/read-string (format "[%s]" (slurp carve-ignore-file))))))

(defn add-to-carve-ignore-file [carve-ignore-file s]
  (let [ignore-file (io/file carve-ignore-file)]
    (when-not (.exists ignore-file) (.createNewFile ignore-file))
    (spit carve-ignore-file s :append true)))


(defn ns+name [ep] [(symbol (namespace ep)) (symbol (name ep))])

(defn unused-vars-data [removed ignore-vars ignore-from-config
                        {:keys [var-definitions var-usages] :as analysis}]
  (let [ignore-from-config (map ns+name ignore-from-config)
        ignore (map ns+name ignore-vars)
        definitions-by-ns+name (index-by (juxt :ns :name) var-definitions)
        defined-vars (set (map (juxt :ns :name) var-definitions))
        used-vars (->> var-usages (remove :recursive) (map (juxt :to :name)) set)]
    (->> [ignore-from-config ignore removed]
         (reduce into used-vars)
         (set/difference defined-vars)
         (map definitions-by-ns+name)
         (group-by :filename))))

(defn interactive [{:keys [:carve-ignore-file]} sym]
  (println (format "Type Y to remove or i to add %s to %s" sym carve-ignore-file))
  (let [input (read-line)]
    (when (= "i" input)
      (add-to-carve-ignore-file carve-ignore-file (str sym "\n")))
    input))

(defn remove-locs [zloc locs locs->syms {:keys [:interactive?
                                                :dry-run?]
                                         :or {interactive? true}
                                         :as opts}]
  (loop [zloc zloc
         locs (seq locs)
         made-changes? false]
    (if locs
      (let [[row col :as loc] (first locs)
            node (z/node zloc)
            m (meta node)]
        (if (and (= row (:row m))
                 (= col (:col m)))
          (do (println "Found unused var:")
              (println "------------------")
              (println (node/string node))
              (println "------------------")
              (let [remove? (cond dry-run? false
                                  interactive?
                                  (= "Y" (interactive opts (get locs->syms loc)))
                                  :else true)
                    zloc (if remove? (z/remove zloc) (z/next zloc))]
                (recur zloc (next locs) (or remove? made-changes?))))
          (recur (z/next zloc) locs made-changes?)))
      {:zloc zloc
       :made-changes? made-changes?})))

(defn ignore? [{:keys [:export :defined-by]}]
  (or export
      (= 'clojure.core/deftype defined-by)
      (= 'clojure.core/defrecord defined-by)
      (= 'clojure.core/defprotocol defined-by)
      (= 'clojure.core/definterface defined-by)))

(defn locs->syms [vars api-namespaces]
  (into {}
        (keep (fn [{:keys [:row :col :ns :name :private :test] :as var-info}]
                (when (and (not test)
                           (not (ignore? var-info))
                           (or (not (contains? api-namespaces ns))
                               private))
                  [[row col] (symbol (str ns) (str name))])) vars)))

(defn carve [file vs {:keys [:out-dir :api-namespaces]
                      :as opts}]
  (let [zloc (z/of-file file)
        locs->syms (locs->syms vs api-namespaces)
        locs (keys locs->syms)
        locs (sort locs)
        _ (when (seq locs)
            (println "Carving" file)
            (println))
        {:keys [:made-changes? :zloc]}
        (remove-locs zloc locs locs->syms opts)]
    (when made-changes?
      (let [out-file (io/file out-dir file)
            out-file (.getCanonicalFile out-file)]
        (io/make-parents out-file)
        (println "Writing result to" (.getPath out-file))
        (with-open [w (io/writer out-file)]
          (z/print-root zloc w))))))

(defn carve! [removed {:keys [ignore-vars paths carve-ignore-file aggressive?] :as opts}]
  (let [unused-vars-data (unused-vars-data removed ignore-vars
                                           (read-carve-ignore-file carve-ignore-file)
                                           (:analysis (clj-kondo/run! {:lint paths :config {:output {:analysis true}}})))]
    (when (seq unused-vars-data)
      (doseq [[file vars] unused-vars-data]
        (carve file vars opts))
      (when aggressive?
        (recur (into removed unused-vars-data) opts)))))

(defn make-report [[file vars] {:keys [api-namespaces] :as opts}]
  (->> api-namespaces
       (locs->syms vars)
       (map (fn [[loc symbol]]
              {:file file :row (first loc) :col (second loc) :symbol symbol}))
       (sort-by :row)))

(defn report! [{:keys [ignore-vars paths carve-ignore-file aggressive?] :as opts}]
  (let [unused-vars-data (unused-vars-data #{} ignore-vars
                                           (read-carve-ignore-file carve-ignore-file)
                                           (:analysis (clj-kondo/run! {:lint paths :config {:output {:analysis true}}})))]
    (when (seq unused-vars-data)
      (let [file-reports (mapcat #(make-report % opts) unused-vars-data)]
        (doseq [{:keys [file symbol row col]} file-reports]
          (println (str file ":" row ":" col " " symbol)))))))
