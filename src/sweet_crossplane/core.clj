(ns sweet-crossplane.core
  (:require (clojure            [string        :as    string])
            (clojure.core       [strint        :refer :all])
            (clojure.data       [json          :as    json])
            (clojure.data.codec [base64        :as    base64])
            (clojure.java       [jdbc          :as    jdbc])
            (clojure.math       [combinatorics :as    combinatorics])
            (clj-time           [coerce        :as    time.coerce])
            (clout              [core          :as    clout])
            (compojure          [core          :as    compojure]
                                [response      :as    compojure.response])
            (dog-mission        [core          :as    dog-mission])
            (hiccup             [core          :refer :all]
                                [def           :refer :all]
                                [element       :refer :all]
                                [form          :refer :all]
                                [page          :refer :all]
                                [util          :refer :all])
            (radial-mount       [core          :as    radial-mount])
            (twin-spar          [core          :refer :all])
            )
  (:import  (java.net           URLEncoder)
            (java.text          ParseException)
            (java.util          Locale TimeZone)))

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
      (handler (reduce (fn [request entity-key]
                         (reduce (fn [request [property-key param-key]]
                                   (cond-> request
                                     (.startsWith (name param-key) (name (sweet-crossplane.core/param-key entity-key property-key))) (#(or (assoc-entity-param % param-key entity-key property-key) %))))
                                 request
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

(defn base64-encode
  [plain]
  (String. (base64/encode (.getBytes (pr-str plain) "UTF-16")) "US-ASCII"))

(defn base64-decode
  [encoded]
  (read-string (String. (base64/decode (.getBytes encoded "US-ASCII")) "UTF-16")))

;; Initializing.

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

;; Models.

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

(defn- list-property-keys
  [entity-key]
  (or (get-in (entity-schema entity-key) [:list :properties])
      (property-keys entity-key)))

(defn- sort-keyfn-and-comp
  [entity-key]
  (let [[keyfn comp] (get-in (entity-schema entity-key) [:list :sort-by])]
    [(or keyfn (first (list-property-keys entity-key)))
     (or comp  compare)]))

(defn- representative-property-key
  [entity-key]
  (or (:representative (entity-schema entity-key))
      :name))

(defn- property-type
  [entity-key property-key]
  (let [entity-schema (entity-schema entity-key)]
    (or (get-in entity-schema [:columns property-key :type])
        (and (get-in entity-schema [:many-to-one-relationships property-key]) :many-to-one)
        (and (get-in entity-schema [:one-to-many-relationships property-key]) :one-to-many))))

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
  (time.coerce/from-date (.parse (dog-mission/date-format) param-value)))

(defmethod parse-property-param :timestamp
  [_ _ param-value]
  (time.coerce/from-date (.parse (dog-mission/date-time-format) param-value)))

(defmethod parse-property-param :default
  [_ _ param-value]
  param-value)

(defmulti ^:private format-property-param
  (fn [entity-key property-key param-value] (property-type entity-key property-key)))

(defmethod format-property-param :date
  [_ _ param-value]
  (if param-value
    (.format (dog-mission/date-format) (time.coerce/to-date param-value))))

(defmethod format-property-param :boolean
  [_ _ param-value]
  (if param-value
    (dog-mission/translate :true)
    (dog-mission/translate :false)))

(defmethod format-property-param :many-to-one
  [_ _ param-value]
  (:name param-value))

(defmethod format-property-param :one-to-many
  [_ _ param-value]
  (string/join (dog-mission/translate :comma) (map :name param-value)))

(defmethod format-property-param :default
  [_ _ param-value]
  (if param-value
    (dog-mission/l10n-format param-value)))

;; Views.

(defn- datetimepicker-date-format
  []
  (-> (.toLocalizedPattern (dog-mission/date-format))
      (string/replace #"y" "Y")
      (string/replace #"d" "D")))

(defn- datetimepicker-timestamp-format
  []
  (str (datetimepicker-date-format) " HH:mm:ss"))  ; momentの書式指定文字は特殊っぽいので、時刻に関してはロケール無視の決め打ちで行きます。

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
    (include-css "/lib/eonasdan-bootstrap-datetimepicker/css/bootstrap-datetimepicker.min.css")
    (include-css "/sweet-crossplane.css")
    (include-css (format "/sweet-crossplane-%s.css" (.getLanguage dog-mission/*locale*)))
    (include-js "/lib/jquery/jquery.min.js")
    (include-js "/lib/bootstrap/bootstrap.min.js")
    (include-js "/lib/moment/moment-with-locales.min.js")
    (include-js "/lib/eonasdan-bootstrap-datetimepicker/bootstrap-datetimepicker.min.js")
    (include-js "/sweet-crossplane.js")
    (javascript-tag (<< "$.crossplane.setDefaults(~(json/write-str {:locale          (.getLanguage dog-mission/*locale*)
                                                                    :dateFormat      (datetimepicker-date-format)
                                                                    :timestampFormat (datetimepicker-timestamp-format)
                                                                    :labels          {:confirm (string/capitalize (dog-mission/translate :confirm))
                                                                                      :yes     (string/capitalize (dog-mission/translate :yes))
                                                                                      :no      (string/capitalize (dog-mission/translate :no))}}));"))]
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
      [:p "Powered by Clojure, core.incubator, data.codec, data.json, math.combinatorics, Logging, clojure.java.jdbc, clj-time, Compojure, Hiccup, PostgreSQL, jQuery, Bootstrap, Bootstrap 3 Datepicker and sweet-crossplane."]]]]))

(defelem link-to-with-method-and-params
  [url method params & content]
  [:a {:href (<< "javascript: $.crossplane.goToWithMethodAndParams(\"~(to-uri url)\", \"~(name method)\", ~(json/write-str params))")}
   content])

(defelem link-to-with-params
  [url params & content]
  (apply link-to-with-method-and-params url :get params content))

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
                  (format-property-param entity-key property-key (get-in *request* [:entity-params param-key]))))])]
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

(defn- date-time-condition-control
  [entity-key property-key type]
  (let [param-key   (param-key entity-key property-key)
        param-key-1 (keyword (format "%s-1--" (name param-key)))
        param-key-2 (keyword (format "%s-2--" (name param-key)))
        errors-1    (not-empty (get-in *request* [:entity-param-errors param-key-1]))
        errors-2    (not-empty (get-in *request* [:entity-param-errors param-key-2]))]
    (letfn [(text-field [param-key errors]
              (list [:div {:class (cond-> "form-group"
                                    errors (str " has-error has-feedback"))}
                     [:div.input-group.date {:data-type type}
                      (hiccup.form/text-field
                       {:class "form-control"}
                       param-key
                       (if errors
                         (get-in *request* [:params param-key])
                         (format-property-param entity-key property-key (get-in *request* [:entity-params param-key]))))
                      [:span.input-group-addon
                       [:span.glyphicon.glyphicon-calendar]]]]))]
      (list
       (label param-key (string/capitalize (dog-mission/translate param-key)))
       (text-field param-key-1 errors-1)
       (text-field param-key-2 errors-2)))))

(defmethod condition-control :date
  [entity-key property-key]
  (date-time-condition-control entity-key property-key :date))

(defmethod condition-control :timestamp
  [entity-key property-key]
  (date-time-condition-control entity-key property-key :timestamp))

(defmethod condition-control :default
  [entity-key property-key]
  nil)

(defn- pager-control
  [page-count page]
  (letfn [(pager-item-control [class caption enable page]
            [:li {:class (cond-> class
                           (not enable) (str " disabled"))}
             (link-to-with-params (:uri *request*) (assoc (:query-params *request*) "page" page) caption)])]
    [:ul.pager
     (pager-item-control "previous" (string/capitalize (dog-mission/translate :previous)) (> page 1)          (dec page))
     (pager-item-control "next"     (string/capitalize (dog-mission/translate :next))     (< page page-count) (inc page))]))

(defn index-view
  [entity-key & [entities page-count page]]
  (@layout (string/capitalize (dog-mission/translate :list-view-title (dog-mission/translate entity-key)))
           [:div.row
            [:div.col-md-3
             [:div.panel.panel-default
              [:div.panel-heading
               [:div.panel-title (string/capitalize (dog-mission/translate :search-condition))]]
              [:div.panel-body
               (form-to
                [:get (:uri *request*)]
                (hidden-field :page "1")
                (map (partial condition-control entity-key) (inputtable-property-keys entity-key))
                [:button.btn.btn-primary {:type "submit", :name :command, :value "search"}
                 (string/capitalize (dog-mission/translate :search))])]]]
            [:div.col-md-9
             (if entities
               (list [:table.table.table-condensed.table-hover
                      [:thead
                       [:tr
                        (map (fn [property-key]
                               [:th (string/capitalize (dog-mission/translate (param-key entity-key property-key)))])
                             (list-property-keys entity-key))
                        [:th]]]
                      [:tbody
                       (map (fn [entity]
                              [:tr
                               (map (fn [property-key]
                                      [:td
                                       [:div (format-property-param entity-key property-key (get entity property-key))]])
                                    (list-property-keys entity-key))
                               [:td.command-cell
                                (link-to-with-method-and-params {:class "btn btn-primary btn-xs"} (<< "/~(name entity-key)/~(:key entity)/edit") :get {:index-params (base64-encode (:query-params *request*))} [:span.glyphicon.glyphicon-pencil])
                                "&nbsp;"
                                (link-to-with-method-and-params {:class "btn btn-danger btn-xs", :data-confirm (string/capitalize (dog-mission/translate :are-you-sure?))} (<< "/~(name entity-key)/~(:key entity)") :delete {:index-params (base64-encode (:query-params *request*))} [:span.glyphicon.glyphicon-trash])]])
                            entities)]]
                     (pager-control page-count page)
                     [:p
                      (link-to-with-method-and-params {:class "btn btn-primary"} (<< "/~(name entity-key)/new") :get {:index-params (base64-encode (:query-params *request*))} (string/capitalize (dog-mission/translate :new)))]))]]))

;; Controllers.

(defmulti condition
  (fn [entity-key property-key] (property-type entity-key property-key)))

(defmethod condition :string
  [entity-key property-key]
  (let [param-key (param-key entity-key property-key)]
    (if-let [param-value (get-in *request* [:entity-params param-key])]
      ($like property-key (format "%%%s%%" param-value)))))

(defn- range-condition
  [entity-key property-key]
  (let [param-key (param-key entity-key property-key)]
    (if-let [conditions (not-empty (keep identity [(if-let [param-value (get-in *request* [:entity-params (keyword (format "%s-1--" (name param-key)))])]
                                                     ($>= property-key param-value))
                                                   (if-let [param-value (get-in *request* [:entity-params (keyword (format "%s-2--" (name param-key)))])]
                                                     ($<= property-key param-value))]))]
      (apply $and conditions))))

(defmethod condition :integer
  [entity-key property-key]
  (range-condition entity-key property-key))

(defmethod condition :decimal
  [entity-key property-key]
  (range-condition entity-key property-key))

(defmethod condition :date
  [entity-key property-key]
  (range-condition entity-key property-key))

(defmethod condition :timestamp
  [entity-key property-key]
  (range-condition entity-key property-key))

(defmethod condition :default
  [entity-key property-key]
  nil)

(defn index-controller
  [entity-key uri-parameters]
  (jdbc/with-db-transaction [transaction @database-spec]
    (apply index-view
      entity-key
      (if (and (= (get-in *request* [:params :command]) "search") (empty? (:entity-param-errors *request*)))
        (let [pages      (->> (-> (->> (if-let [conditions (not-empty (keep (partial condition entity-key) (inputtable-property-keys entity-key)))]
                                         (apply $and conditions))
                                       (database-data @database-schema transaction entity-key)
                                       (database      @database-schema))
                                  (get-condition-matched-rows entity-key))
                              ((apply partial sort-by (sort-keyfn-and-comp entity-key)))
                              (partition-all 20)
                              (not-empty))
              page-count (count pages)
              page       (let [request-page (Integer/parseInt (or (get-in *request* [:params :page]) "1"))]
                           (cond->> request-page
                             (> request-page page-count) ((constantly page-count))
                             (< request-page 1)          ((constantly 1))))]
          [(if (empty? pages)
             []
             (nth pages (dec page)))
           page-count
           page])))))

(defn new-controller
  [entity-key uri-parameters]
  (@layout (string/capitalize (dog-mission/translate :new-view-title (dog-mission/translate entity-key)))
           (list [:p "new"]
                 (form-to
                  [:post (format "/%s" (name entity-key))]
                  (hidden-field :index-params (get-in *request* [:params :index-params]))
                  (submit-button "create"))
                 (link-to-with-method-and-params {:class "btn btn-default"} (<< "/~(name entity-key)") :get (base64-decode (get-in *request* [:params :index-params])) (string/capitalize (dog-mission/translate :back))))))

(defn create-controller
  [entity-key uri-parameters]
  (@layout (string/capitalize (dog-mission/translate :create-view-title (dog-mission/translate entity-key)))
           (list [:p "create"]
                 (link-to-with-method-and-params {:class "btn btn-default"} (<< "/~(name entity-key)") :get (base64-decode (get-in *request* [:params :index-params])) (string/capitalize (dog-mission/translate :back))))))

(defn edit-controller
  [entity-key uri-parameters]
  (@layout (string/capitalize (dog-mission/translate :edit-view-title (dog-mission/translate entity-key)))
           (list [:p "edit"]
                 (form-to
                  [:patch  (format "/%s/%s" (name entity-key) (:key uri-parameters))]
                  (hidden-field :index-params (get-in *request* [:params :index-params]))
                  (submit-button "update"))
                 (link-to-with-method-and-params {:class "btn btn-default"} (<< "/~(name entity-key)") :get (base64-decode (get-in *request* [:params :index-params])) (string/capitalize (dog-mission/translate :back))))))

(defn update-controller
  [entity-key uri-parameters]
  (@layout (string/capitalize (dog-mission/translate :update-view-title (dog-mission/translate entity-key)))
           (list [:p "update"]
                 (link-to-with-method-and-params {:class "btn btn-default"} (<< "/~(name entity-key)") :get (base64-decode (get-in *request* [:params :index-params])) (string/capitalize (dog-mission/translate :back))))))

(defn destroy-controller
  [entity-key uri-parameters]
  (@layout (string/capitalize (dog-mission/translate :destroy-view-title (dog-mission/translate entity-key)))
           (list [:p "destroy"]
                 (link-to-with-method-and-params {:class "btn btn-default"} (<< "/~(name entity-key)") :get (base64-decode (get-in *request* [:params :index-params])) (string/capitalize (dog-mission/translate :back))))))

;; Rouring.

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
                   [[:get    "/%s"           index-controller]  ; TODO: 画面生成を抑制するオプションを追加する。
                    [:get    "/%s/new"       new-controller]
                    [:post   "/%s"           create-controller]
                    [:get    "/%s/:key/edit" edit-controller]
                    [:patch  "/%s/:key"      update-controller]
                    [:delete "/%s/:key"      destroy-controller]])
            (entity-keys)))))
