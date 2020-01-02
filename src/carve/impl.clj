(ns carve.impl
  {:no-doc true}
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
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

(defn interactive [{:keys [:carve-ignore-file]} sym]
  (println (format "Type Y to remove or i to add %s to %s" sym carve-ignore-file))
  (let [input (read-line)]
    (when (= "i" input)
      (add-to-carve-ignore-file carve-ignore-file (str sym "\n")))
    input))

(defmulti preamble :format)

(defmethod preamble :default [& _] "")

(defmethod preamble :edn [& _] "[")

(defmethod preamble :interactive [{:keys [file]}]
  (str "Carving " file "\n"))

(defmulti report :format)

(defmethod report :interactive [{:keys [node]}]
  (str "Found unused var:\n"
       "------------------\n"
       (node/string node)
       "\n"
       "------------------\n"))

(defmulti postamble :format)

(defmethod postamble :default [& _] "")

(defmethod postamble :edn [& _] "]")

(defmethod report :text [{:keys [node loc file var]}]
  (let [[row col] loc]
    (str file ":" row ":" col " " var "\n")))

(defmethod report :edn [{:keys [node loc file var]}]
  (let [[row col] loc]
    {:row row
     :col col
     :file file
     :var var}))

(defn remove-locs [file zloc locs locs->syms {:keys [:interactive?
                                                     :dry-run?
                                                     :format]
                                              :or {interactive? true
                                                   format :interactive}
                                              :as opts}]
  (when (seq locs)
    (print (preamble {:file file :format format})))
  (loop [zloc zloc
         locs (seq locs)
         made-changes? false]
    (if locs
      (let [[row col :as loc] (first locs)
            node (z/node zloc)
            m (meta node)]
        (if (and (= row (:row m))
                 (= col (:col m)))
          (do (println (str file ":" row ":" col " " (get locs->syms loc)))
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

(defn carve [file vs {:keys [:out-dir :api-namespaces]
                      :as opts}]
  (let [zloc (z/of-file file)
        locs->syms (into {}
                         (keep (fn [{:keys [:row :col :ns :name :private :test] :as var-info}]
                                 (when (and (not test)
                                            (not (ignore? var-info))
                                            (or (not (contains? api-namespaces ns))
                                                private))
                                   [[row col] (symbol (str ns) (str name))])) vs))
        locs (keys locs->syms)
        locs (sort locs)
        {:keys [:made-changes? :zloc]}
        (remove-locs file zloc locs locs->syms opts)]
    (when made-changes?
      (let [out-file (io/file out-dir file)
            out-file (.getCanonicalFile out-file)]
        (io/make-parents out-file)
        (println "Writing result to" (.getPath out-file))
        (with-open [w (io/writer out-file)]
          (z/print-root zloc w))))))
