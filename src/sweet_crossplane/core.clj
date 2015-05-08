(ns sweet-crossplane.core
  (:require (clojure      [string        :as    string])
            (clojure.java [jdbc          :as    jdbc])
            (clojure.math [combinatorics :as    combinatorics])
            (clout        [core          :as    clout])
            (compojure    [core          :as    compojure]
                          [response      :as    compojure.response])
            (dog-mission  [core          :as    dog-mission])
            (hiccup       [core          :refer :all]
                          [element       :refer :all]
                          [form          :refer :all]
                          [page          :refer :all])
            (radial-mount [core          :as    radial-mount])
            (twin-spar    [core          :refer :all])
            )
  (:import  (java.text    ParseException)
            (java.util    Locale TimeZone)))

(dog-mission/conj-resource-bundle-namespace "sweet-crossplane.message")

;; Utilities.

;; Ring Middlewares. You should add below middlewares in your application.

(defn wrap-locale
  [handler {:keys [locales]}]
  (fn [request]
    (letfn [(locale-and-quality [[locale-string quality-string]]
              [(let [[language country] (next (re-find #"([a-z]+)(?:-([A-Z]+)|)" locale-string))]
                 (Locale. language (or country "")))
               (Double/parseDouble (second (re-find #"[qQ]=([\d.]+)" (or quality-string "q=1.0"))))])]
      (let [client-locales (->> (string/split (get-in request [:headers "accept-language"]) #",")
                                (map #(locale-and-quality (string/split % #";")))
                                (sort-by second >)
                                (map first))]
        (binding [dog-mission/*locale* (or (some locales client-locales) dog-mission/*locale*)]
          (handler request))))))

(defn wrap-time-zone
  [handler]
  (fn [{:keys [uri query-string] :as request}]
    (if-let [time-zone-id (get-in request [:cookies "time-zone-id" :value])]
      (binding [dog-mission/*time-zone* (TimeZone/getTimeZone time-zone-id)]
        (handler request))
      (compojure.response/render (html5 [:body
                                         (hidden-field   :go-back-uri (str uri (and (not-empty query-string) "?") query-string))
                                         (javascript-tag "var timeZoneOffset = new Date().getTimezoneOffset();
                                                          var timeZoneId = \"GMT\" + (timeZoneOffset < 0 ? \"+\" : \"-\") + (\"0\" + (Math.floor (Math.abs (timeZoneOffset / 60)))).slice(-2) + \":\" + (\"0\" + (timeZoneOffset % 60)).slice(-2);
                                                          document.cookie = \"time-zone-id=\" + encodeURIComponent(timeZoneId);
                                                          location.href = document.getElementById(\"go-back-uri\").value;")])
                                 request))))

(defn param-key
  [entity-key property-key]
  (radial-mount/property-message-key entity-key property-key))

(declare entity-keys inputtable-property-keys parse-property-param)

(defn wrap-entity-params
  [handler]
  (letfn [(assoc-entity-param [request param-key entity-key property-key]
            (if-let [param-value (not-empty (get-in request [:params param-key]))]
              (try
                (assoc-in request [:entity-params param-key] (parse-property-param entity-key property-key param-value))
                (catch ParseException _
                  (update-in request [:entity-param-errors param-key] #(vec (conj % :has-the-wrong-format)))))))]
    (fn [request]
      (handler (reduce (fn [result entity-key]
                         (reduce (fn [result [property-key param-key]]
                                   (cond-> result
                                     (.startsWith (name param-key) (name (sweet-crossplane.core/param-key entity-key property-key))) (#(or (assoc-entity-param % param-key entity-key property-key) %))))
                                 result
                                 (combinatorics/cartesian-product (inputtable-property-keys entity-key) (keys (:params request)))))
                       request
                       (entity-keys))))))

(def ^:dynamic *request*
  nil)

(defn wrap-ring-request
  [handler]
  (fn [request]
    (binding [*request* request]
      (handler request))))

;; Models.

(def ^:private database-schema
  (atom nil))

(def ^:private database-spec
  (atom nil))

(def ^:private brand-name
  (atom nil))

(def ^:private layout
  (atom nil))

(defn initialize
  [database-schema database-spec brand-name layout]
  (reset! sweet-crossplane.core/database-schema database-schema)
  (reset! sweet-crossplane.core/database-spec   database-spec)
  (reset! sweet-crossplane.core/brand-name      brand-name)
  (reset! sweet-crossplane.core/layout          layout))

(defn- entity-keys
  []
  (keys @database-schema))

(defn- entity-schema
  [entity-key]
  (get @database-schema entity-key))

(defn- inputtable-property-keys
  [entity-key]
  (->> (entity-schema entity-key)
       ((juxt :columns :many-to-one-relationships))
       (map keys)
       (apply concat)))

(defn- property-keys
  [entity-key]
  (concat (inputtable-property-keys entity-key)
          (keys (:one-to-many-relationships (entity-schema entity-key)))))

(defn- property-type
  [entity-key property-key]
  (let [entity-schema (entity-schema entity-key)]
    (or (get-in entity-schema [:columns property-key :type])
        (and (get-in entity-schema [:many-to-one-relationship property-key]) :many-to-one)
        (and (get-in entity-schema [:one-to-many-relationship property-key]) :one-to-many))))

(defmulti ^:private parse-property-param
  (fn [entity-key property-key param-value] (property-type entity-key property-key)))

(defmethod parse-property-param :integer
  [_ _ param-value]
  (.longValue (.parse (dog-mission/number-format) param-value)))

(defmethod parse-property-param :decimal
  [_ _ param-value]
  (bigdec (.doubleValue (.parse (dog-mission/number-format) param-value))))

(defmethod parse-property-param :boolean
  [_ _ param-value]
  (= param-value (dog-mission/translate :true)))

(defmethod parse-property-param :date
  [_ _ param-value]
  (.parse (dog-mission/date-format) param-value))

(defmethod parse-property-param :timestamp
  [_ _ param-value]
  (.parse (dog-mission/date-time-format) param-value))

(defmethod parse-property-param :default
  [_ _ param-value]
  param-value)

(defmulti ^:private format-property-param
  (fn [entity-key property-key param-value] (property-type entity-key property-key)))

(defmethod format-property-param :date
  [_ _ param-value]
  (if param-value
    (.format (dog-mission/date-format) param-value)))

(defmethod format-property-param :boolean
  [_ _ param-value]
  (if param-value
    (dog-mission/translate :true)
    (dog-mission/translate :false)))

(defmethod format-property-param :default
  [_ _ param-value]
  (if param-value
    (dog-mission/l10n-format param-value)))

;; Views.

(defn default-layout
  [title contents]
  (html5
   {:lang (.getLanguage dog-mission/*locale*)}
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:http-quiv "X-UA-Compatible", :content "IE=edge"}]
    [:meta {:name "viewport", :content "width=device-width, initial-scale=1"}]
    [:title (h (str title))]
    (include-css "/lib/bootstrap/css/bootstrap.min.css")
    (include-css (format "/sweet-crossplane-%s.css" (.getLanguage dog-mission/*locale*)))]
   [:body
    [:header.navbar.navbar-default
     [:nav.container-fluid
      [:div.navbar-header
       (link-to {:class "navbar-brand"} "/" @brand-name)]
      [:div.collapse.navbar-collapse
       [:ul.nav.navbar-nav.navbar-right
        [:li.dropdown
         [:a.dropdown-toggle
          {:role "button", :data-toggle "dropdown", :aria-expanded "false"}
          (string/capitalize (dog-mission/translate :maintain-data))
          "&nbsp;"
          [:span.caret]]
         [:ul.dropdown-menu
          {:role "menu"}
          (map (fn [entity-key]
                 [:li (link-to (format "/%s" (name entity-key)) (string/capitalize (dog-mission/translate entity-key)))])
               (entity-keys))]]]]]]
    [:article
     [:div.container-fluid
      contents]]
    [:footer
     [:div.container-fluid
      [:p "Powered by Clojure, core.incubator, Logging, clojure.java.jdbc, clj-time, Compojure, Hiccup, PostgreSQL, jQuery, Bootstrap and sweet-crossplane."]]]
    (include-js "/lib/jquery/jquery.min.js")
    (include-js "/lib/bootstrap/bootstrap.min.js")]))

(defmulti condition-control
  (fn [entity-key property-key] (property-type entity-key property-key)))

(defmethod condition-control :string
  [entity-key property-key]
  (let [param-key (param-key entity-key property-key)]
    [:div.form-group
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (text-field
      {:class "form-control", :placeholder (get-in (entity-schema entity-key) [:placeholders property-key])}
      param-key
      (format-property-param entity-key property-key (get-in *request* [:entity-params param-key])))]))

(defn- number-condition-control
  [entity-key property-key]
  (let [param-key   (param-key entity-key property-key)
        param-key-1 (keyword (format "%s-1--" (name param-key)))
        param-key-2 (keyword (format "%s-2--" (name param-key)))
        errors-1    (not-empty (get-in *request* [:entity-param-errors param-key-1]))
        errors-2    (not-empty (get-in *request* [:entity-param-errors param-key-2]))]
    (letfn [(text-field [param-key errors]
              [:div
               {:class (cond-> "form-group"
                         errors (str " has-error has-feedback"))}
               (hiccup.form/text-field
                {:class "form-control"}
                param-key
                (if errors
                  (get-in *request* [:params param-key])
                  (format-property-param entity-key property-key (get-in *request* [:entity-params param-key]))))
               (if errors
                 [:span.glyphicon.glyphicon-remove.form-control-feedback])])]
      (list
       (label param-key (string/capitalize (dog-mission/translate param-key)))
       (text-field param-key-1 errors-1)
       (text-field param-key-2 errors-2)))))

(defmethod condition-control :integer
  [entity-key property-key]
  (number-condition-control entity-key property-key))

(defmethod condition-control :decimal
  [entity-key property-key]
  (number-condition-control entity-key property-key))

(defmethod condition-control :default
  [entity-key property-key]
  nil)

(defn index-view
  [entity-key entities]
  (@layout (string/capitalize (dog-mission/translate :list-view-title (dog-mission/translate entity-key)))
           [:div.row
            [:div.col-md-3
             [:div.panel.panel-default
              [:div.panel-heading
               [:div.panel-title (string/capitalize (dog-mission/translate :search-condition))]]
              [:div.panel-body
               (form-to
                [:get (:uri *request*)]
                (map (partial condition-control entity-key) (inputtable-property-keys entity-key))
                (submit-button {:class "btn btn-primary"} (string/capitalize (dog-mission/translate :search))))]]]
            [:div.col-md-9
             (map (fn [entity]
                    [:p (:name entity)])
                  entities)]]))

;; Controllers.

(defmulti condition
  (fn [entity-key property-key] (property-type entity-key property-key)))

(defmethod condition :string
  [entity-key property-key]
  (let [param-key (param-key entity-key property-key)]
    (if-let [param-value (get-in *request* [:entity-params param-key])]
      ($like property-key (format "%%%s%%" param-value)))))

(defmethod condition :decimal
  [entity-key property-key]
  (let [param-key (param-key entity-key property-key)]
    (if-let [conditions (not-empty (keep identity [(if-let [param-value (get-in *request* [:entity-params (keyword (format "%s-1--" (name param-key)))])]
                                                     ($>= property-key param-value))
                                                   (if-let [param-value (get-in *request* [:entity-params (keyword (format "%s-2--" (name param-key)))])]
                                                     ($<= property-key param-value))]))]
      (apply $and conditions))))

(defmethod condition :default
  [entity-key property-key]
  nil)

(defn index-controller
  [entity-key uri-parameters]
  (jdbc/with-db-transaction [transaction @database-spec]
    (let [condition (if-let [conditions (not-empty (keep (partial condition entity-key) (inputtable-property-keys entity-key)))]
                      (apply $and conditions))
          database  (database @database-schema (database-data @database-schema transaction entity-key condition))]
      (index-view entity-key (get-condition-matched-rows database entity-key)))))

(defn new-controller
  [entity-key uri-parameters]
  (@layout (string/capitalize (dog-mission/translate :new-view-title (dog-mission/translate entity-key)))
           [:div
            [:p "new"]
            (form-to
             [:post (format "/%s" (name entity-key))]
             (submit-button "create"))]))

(defn create-controller
  [entity-key uri-parameters]
  (@layout (string/capitalize (dog-mission/translate :create-view-title (dog-mission/translate entity-key)))
           [:p "create"]))

(defn edit-controller
  [entity-key uri-parameters]
  (@layout (string/capitalize (dog-mission/translate :edit-view-title (dog-mission/translate entity-key)))
           [:div
            [:p "edit"]
            (form-to
             [:patch  (format "/%s/%s" (name entity-key) (:key uri-parameters))]
             (submit-button "update"))
            (form-to
             [:delete (format "/%s/%s" (name entity-key) (:key uri-parameters))]
             (submit-button "destroy"))]))

(defn update-controller
  [entity-key uri-parameters]
  (@layout (string/capitalize (dog-mission/translate :update-view-title (dog-mission/translate entity-key)))
           [:p "update"]))

(defn destroy-controller
  [entity-key uri-parameters]
  (@layout (string/capitalize (dog-mission/translate :destroy-view-title (dog-mission/translate entity-key)))
           [:p "destroy"]))

(defn process-request
  []
  (fn [request]
    (letfn [(route-matches [method route]
              (let [request-method (:request-method request)]
                (if (or (= method request-method)
                        (and (= request-method :post)
                             (.equalsIgnoreCase (name method)
                                                (or (get-in request [:form-params      "_method"])
                                                    (get-in request [:multipart-params "_method"])))))
                  (clout/route-matches route request))))]
      (some #(some (fn [[method path-format controller]]
                     (if-let [uri-parameters (route-matches method (format path-format (name %)))]
                       (compojure.response/render (controller % uri-parameters) request)))
                   [[:get    "/%s"           index-controller]
                    [:get    "/%s/new"       new-controller]
                    [:post   "/%s"           create-controller]
                    [:get    "/%s/:key/edit" edit-controller]
                    [:patch  "/%s/:key"      update-controller]
                    [:delete "/%s/:key"      destroy-controller]])
            (entity-keys)))))
