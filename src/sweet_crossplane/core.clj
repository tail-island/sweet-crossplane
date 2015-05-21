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
            (twin-spar          [core          :refer :all]))
  (:import  (java.net           URLEncoder)
            (java.text          ParseException)
            (java.util          Locale TimeZone UUID)))

(dog-mission/conj-resource-bundle-namespace "sweet-crossplane.message")

;; Utilities.

;; ring middlewares. You should add below middlewares in your application.

(defn wrap-http-header-cache-control
  [handler]
  (fn [request]
    (if-let [response (handler request)]
      (assoc-in response [:headers "Cache-Control"] "max-age=0, private, must-revalidate"))))

(defn wrap-time-zone
  [handler]
  (fn [{:keys [uri query-string] :as request}]
    (if-let [time-zone-id (get-in request [:cookies "time-zone-id" :value])]
      (binding [dog-mission/*time-zone* (TimeZone/getTimeZone time-zone-id)]
        (handler request))
      (compojure.response/render (html5 [:body
                                         (hidden-field   :go-back-uri (str uri (and (not-empty query-string) "?") query-string))
                                         (javascript-tag "var timeZoneOffset = new Date().getTimezoneOffset();
                                                          var timeZoneId = 'GMT' + (timeZoneOffset < 0 ? '+' : '-') + ('0' + (Math.floor (Math.abs (timeZoneOffset / 60)))).slice(-2) + ':' + ('0' + (timeZoneOffset % 60)).slice(-2);
                                                          document.cookie = 'time-zone-id=' + encodeURIComponent(timeZoneId);
                                                          location.href = document.getElementById('go-back-uri').value;")])
                                 request))))

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

;; utility functions and macros.

(defn base64-encode
  [plain]
  (String. (base64/encode (.getBytes (pr-str plain) "UTF-16")) "US-ASCII"))

(defn base64-decode
  [encoded]
  (read-string (String. (base64/decode (.getBytes encoded "US-ASCII")) "UTF-16")))

(def ^:dynamic *transaction*
  nil)

(defmacro with-db-transaction'
  [& body]
  `(jdbc/with-db-transaction [transaction# @database-spec]
     (binding [*transaction* transaction#]
       ~@body)))

;; Initializing.

(def ^:private database-schema
  (atom nil))

(def ^:private database-spec
  (atom nil))

(def ^:private brand-name
  (atom nil))

(def ^:private layout-fn
  (atom nil))

(defn initialize
  [database-schema database-spec brand-name layout-fn]
  (reset! sweet-crossplane.core/database-schema database-schema)
  (reset! sweet-crossplane.core/database-spec   database-spec)
  (reset! sweet-crossplane.core/brand-name      brand-name)
  (reset! sweet-crossplane.core/layout-fn       layout-fn))

;; Models.

(defn entity-keys
  []
  (keys @database-schema))

(defn entity-schema
  [entity-key]
  (get @database-schema entity-key))

(defn property-keys
  ([entity-key & property-types]
   (->> (entity-schema entity-key)
        ((apply juxt property-types))
        (map keys)
        (apply concat)))
  ([entity-key]
   (property-keys entity-key :columns :many-to-one-relationships :one-to-many-relationships)))

(defn inputtable-property-keys
  [entity-key]
  (property-keys entity-key :columns :many-to-one-relationships))

(defn search-condition-property-keys
  [entity-key]
  (or (get-in (entity-schema entity-key) [:search-condition :properties])
      (inputtable-property-keys entity-key)))

(defn list-property-keys
  [entity-key]
  (or (get-in (entity-schema entity-key) [:list :properties])
      (property-keys entity-key)))

(defn sort-keyfn-and-comp
  [entity-key]
  (let [[keyfn comp] (get-in (entity-schema entity-key) [:list :sort-by])]
    [(or keyfn (first (list-property-keys entity-key)))
     (or comp  compare)]))

(defn representative-property-key
  [entity-key]
  (or (:representative (entity-schema entity-key))
      :name))

(defn property-schema
  [entity-key property-key]
  (let [entity-schema (entity-schema entity-key)]
    (or (get-in entity-schema [:columns                   property-key])
        (get-in entity-schema [:many-to-one-relationships property-key])
        (get-in entity-schema [:one-to-many-relationships property-key]))))

(defn placeholder
  [entity-key property-key]
  (get-in (entity-schema entity-key) [:placeholders property-key]))

(defn property-type
  [entity-key property-key]
  (let [entity-schema (entity-schema entity-key)]
    (or (get-in entity-schema [:columns property-key :type])
        (and (get-in entity-schema [:many-to-one-relationships property-key]) :many-to-one)
        (and (get-in entity-schema [:one-to-many-relationships property-key]) :one-to-many))))

(defmulti parse-property-param
  (fn [entity-key property-key param-value] (property-type entity-key property-key)))

(defmethod parse-property-param :integer
  [_ _ param-value]
  (.longValue (.parse (dog-mission/number-format) param-value)))

(defmethod parse-property-param :decimal
  [_ _ param-value]
  (bigdec (.doubleValue (.parse (dog-mission/number-format) param-value))))

(defmethod parse-property-param :boolean
  [_ _ param-value]
  (= param-value (string/capitalize (dog-mission/translate :true))))

(defmethod parse-property-param :date
  [_ _ param-value]
  (time.coerce/from-date (.parse (dog-mission/date-format) param-value)))

(defmethod parse-property-param :timestamp
  [_ _ param-value]
  (time.coerce/from-date (.parse (dog-mission/date-time-format) param-value)))

(defmethod parse-property-param :many-to-one
  [_ _ param-value]
  (try
    (UUID/fromString param-value)
    (catch IllegalArgumentException _
      param-value)))

(defmethod parse-property-param :default
  [_ _ param-value]
  param-value)

(defmulti format-property-param
  (fn [entity-key property-key param-value] (property-type entity-key property-key)))

(defmethod format-property-param :date
  [_ _ param-value]
  (if param-value
    (.format (dog-mission/date-format) (time.coerce/to-date param-value))))

(defmethod format-property-param :boolean
  [_ _ param-value]
  (case param-value
    true  (string/capitalize (dog-mission/translate :true))
    false (string/capitalize (dog-mission/translate :false))
    ""))

(defn- format-relationship-property-param
  [entity-key property-key param-value]
  (if param-value
    (if (map? param-value)
      (let [entity-key   (:table-key (property-schema entity-key property-key))
            property-key (representative-property-key entity-key)]
        (format-property-param entity-key property-key (get param-value property-key)))
      (dog-mission/l10n-format param-value))))

(defmethod format-property-param :many-to-one
  [entity-key property-key param-value]
  (format-relationship-property-param entity-key property-key param-value))

(defmethod format-property-param :one-to-many
  [entity-key property-key param-value]
  (string/join (dog-mission/translate :comma) (map (partial format-relationship-property-param entity-key property-key) param-value)))

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

(defn default-layout-fn
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
    (include-css (<< "/sweet-crossplane-~(.getLanguage dog-mission/*locale*).css"))
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
                 [:li (link-to (<< "/~(name entity-key)") (string/capitalize (dog-mission/translate entity-key)))])
               (entity-keys))]]]]]]
    [:article
     [:div.container-fluid
      contents]]
    [:footer
     [:div.container-fluid
      [:p "Powered by Clojure, core.incubator, data.codec, data.json, math.combinatorics, Logging, clojure.java.jdbc, clj-time, Compojure, Hiccup, PostgreSQL, jQuery, Bootstrap, Bootstrap 3 Datepicker and sweet-crossplane."]]]]))

(defn layout
  [title contents]
  (@layout-fn title contents))

(defelem link-to-javascript
  [javascript & content]
  [:a {:href (<< "javascript: ~{javascript}")}
   content])

(defelem link-to-with-method-and-params
  [uri method params & content]
  (apply link-to-javascript (<< "$.crossplane.goToWithMethodAndParams('~(to-uri uri)', '~(name method)', ~(json/write-str params))") content))

(defelem link-to-with-params
  [uri params & content]
  (apply link-to-with-method-and-params uri :get params content))
  
;; controls for input condition.

(defmulti condition-control
  (fn [entity-key property-key] (property-type entity-key property-key)))

(defn- string-condition-control
  [entity-key property-key]
  (let [param-key (param-key entity-key property-key)]
    [:div.form-group
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (text-field {:class "form-control", :placeholder (placeholder entity-key property-key)} param-key (format-property-param entity-key property-key (get-in *request* [:entity-params param-key])))]))

(defmethod condition-control :string
  [entity-key property-key]
  (string-condition-control entity-key property-key))

(defmethod condition-control :text
  [entity-key property-key]
  (string-condition-control entity-key property-key))

(defn- number-condition-control
  [entity-key property-key]
  (let [param-key   (param-key entity-key property-key)
        param-key-1 (keyword (format "%s-1--" (name param-key)))
        param-key-2 (keyword (format "%s-2--" (name param-key)))
        errors-1    (not-empty (get-in *request* [:entity-param-errors param-key-1]))
        errors-2    (not-empty (get-in *request* [:entity-param-errors param-key-2]))]
    (letfn [(text-field [param-key errors]
              [:div.form-group
               (if errors
                 {:class "has-error has-feedback"})
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

(defmethod condition-control :boolean
  [entity-key property-key]
  (let [param-key (param-key entity-key property-key)]
    [:div.form-group
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (drop-down {:class "form-control"} param-key (map (partial format-property-param entity-key property-key) [nil true false]) (format-property-param entity-key property-key (get-in *request* [:entity-params param-key])))]))

(defn- date-timestamp-condition-control
  [entity-key property-key]
  (let [param-key   (param-key entity-key property-key)
        param-key-1 (keyword (format "%s-1--" (name param-key)))
        param-key-2 (keyword (format "%s-2--" (name param-key)))
        errors-1    (not-empty (get-in *request* [:entity-param-errors param-key-1]))
        errors-2    (not-empty (get-in *request* [:entity-param-errors param-key-2]))]
    (letfn [(text-field [param-key errors]
              [:div.form-group
               (if errors
                 {:class "has-error has-feedback"})
               [:div.input-group.date {:data-type (:type (property-schema entity-key property-key))}
                (hiccup.form/text-field
                 {:class "form-control"}
                 param-key
                 (if errors
                   (get-in *request* [:params param-key])
                   (format-property-param entity-key property-key (get-in *request* [:entity-params param-key]))))
                [:span.input-group-addon
                 [:span.glyphicon.glyphicon-calendar]]]])]
      (list
       (label param-key (string/capitalize (dog-mission/translate param-key)))
       (text-field param-key-1 errors-1)
       (text-field param-key-2 errors-2)))))

(defmethod condition-control :date
  [entity-key property-key]
  (date-timestamp-condition-control entity-key property-key))

(defmethod condition-control :timestamp
  [entity-key property-key]
  (date-timestamp-condition-control entity-key property-key))

(defmethod condition-control :many-to-one
  [entity-key property-key]
  (let [param-key          (param-key entity-key property-key)
        param-key-key      (keyword (format "%s-key"    (name param-key)))
        param-key-name-key (keyword (format "%s-name--" (name param-key-key)))]
    [:div.form-group
     (label param-key-key (string/capitalize (dog-mission/translate param-key)))
     (hidden-field param-key-key (get-in *request* [:entity-params param-key-key]))
     [:div.input-group
      (text-field {:class "form-control", :readonly "readonly"} param-key-name-key (format-property-param entity-key property-key (get-in *request* [:entity-params param-key-name-key])))
      [:span.input-group-addon {:data-open-window-uri (to-uri (<< "/~(name (:table-key (property-schema entity-key property-key)))/select?target-element-id=~(URLEncoder/encode (name param-key-key))"))}
       [:span.glyphicon.glyphicon-search]]]]))

(defmethod condition-control :default
  [entity-key property-key]
  nil)

(defn pagination-control
  [page-count page]
  (letfn [(pagination-item-start-and-count []
            (let [item-count (min page-count 5)]
              [item-count (min (max (- page (quot item-count 2)) 1) (- page-count (dec item-count)))]))
          (pagination-item-control [caption enable active link-to-page]
            [:li {:class (cond-> ""
                           (not enable) (str " disabled")
                           active       (str " active"))}
             (link-to-with-params (:uri *request*) (assoc (:query-params *request*) "page" link-to-page) caption)])]
    [:ul.pagination
     (pagination-item-control "&laquo;" (> page 1) false 1)
     (let [[pagination-item-count pagination-item-start] (pagination-item-start-and-count)]
       (map #(pagination-item-control (str %) true (= % page) %) (take pagination-item-count (iterate inc pagination-item-start))))
     (pagination-item-control "&raquo;" (< page page-count) false page-count)]))

(defn search-condition-panel
  [entity-key & [params]]
  [:div.panel.panel-default
   [:div.panel-heading
    [:div.panel-title (string/capitalize (dog-mission/translate :search-condition))]]
   [:div.panel-body
    (form-to
     [:get (:uri *request*)]
     (hidden-field :page "1")
     (map (fn [[param-name param-value]]
            (hidden-field param-name param-value))
          params)
     (map (partial condition-control entity-key) (search-condition-property-keys entity-key))
     [:button.btn.btn-primary {:type "submit", :name :command, :value "search"}
      (string/capitalize (dog-mission/translate :search))])]])

;; controls for input value.

(defmulti input-control
  (fn [entity-key entity property-key] (property-type entity-key property-key)))

(defn- string-input-control
  [entity-key entity property-key]
  (let [param-key (param-key entity-key property-key)
        errors    (not-empty (get-in *request* [:entity-param-errors param-key]))]
    [:div.form-group
     (if errors
       {:class "has-error has-feedback"})
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (text-field {:class "form-control", :placeholder (placeholder entity-key property-key)} param-key (format-property-param entity-key property-key (get entity property-key)))]))

(defmethod input-control :string
  [entity-key entity property-key]
  (string-input-control entity-key entity property-key))

(defmethod input-control :text
  [entity-key entity property-key]
  (string-input-control entity-key entity property-key))

(defn- number-input-control
  [entity-key entity property-key]
  (let [param-key (param-key entity-key property-key)
        errors    (not-empty (get-in *request* [:entity-param-errors param-key]))]
    [:div.form-group
     (if errors
       {:class "has-error has-feedback"})
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (text-field
      {:class "form-control"}
      param-key
      (if errors
        (get-in *request* [:params param-key])
        (format-property-param entity-key property-key (get entity property-key))))]))

(defmethod input-control :integer
  [entity-key entity property-key]
  (number-input-control entity-key entity property-key))

(defmethod input-control :decimal
  [entity-key entity property-key]
  (number-input-control entity-key entity property-key))

(defmethod input-control :boolean
  [entity-key entity property-key]
  (let [param-key (param-key entity-key property-key)
        errors    (not-empty (get-in *request* [:entity-param-errors param-key]))]
    [:div.form-group
     (if errors
       {:class "has-error has-feedback"})
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (drop-down {:class "form-control"} param-key (map (partial format-property-param entity-key property-key) [nil true false]) (format-property-param entity-key property-key (get entity property-key)))]))

(defn- date-timestamp-input-control
  [entity-key entity property-key]
  (let [param-key (param-key entity-key property-key)
        errors    (not-empty (get-in *request* [:entity-param-errors param-key]))]
    [:div.form-group
     (if errors
       {:class "has-error has-feedback"})
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     [:div.input-group.date {:data-type (:type (property-schema entity-key property-key))}
      (hiccup.form/text-field
       {:class "form-control"}
       param-key
       (if errors
         (get-in *request* [:params param-key])
         (format-property-param entity-key property-key (get entity property-key))))
      [:span.input-group-addon
       [:span.glyphicon.glyphicon-calendar]]]]))

(defmethod input-control :date
  [entity-key entity property-key]
  (date-timestamp-input-control entity-key entity property-key))

(defmethod input-control :timestamp
  [entity-key entity property-key]
  (date-timestamp-input-control entity-key entity property-key))

(defmethod input-control :many-to-one
  [entity-key entity property-key]
  (let [param-key          (param-key entity-key property-key)
        param-key-key      (keyword (format "%s-key"    (name param-key)))
        param-key-name-key (keyword (format "%s-name--" (name param-key-key)))
        errors             (get-in *request* [:entity-pararam-errors param-key])]
    [:div.form-group
     (if errors
       {:class "has-error has-feedback"})
     (label param-key-key (string/capitalize (dog-mission/translate param-key)))
     (hidden-field param-key-key (get entity (many-to-one-relationship-key-to-physical-column-key property-key)))
     [:div.input-group
      (text-field {:class "form-control", :readonly "readonly"} param-key-name-key (format-property-param entity-key property-key (get entity property-key)))
      [:span.input-group-addon {:data-open-window-uri (to-uri (<< "/~(name (:table-key (property-schema entity-key property-key)))/select?target-element-id=~(URLEncoder/encode (name param-key-key))"))}
       [:span.glyphicon.glyphicon-search]]]]))

(defmethod input-control :one-to-many
  [entity-key entity property-key]
  (let [param-key (param-key entity-key property-key)]
    [:div.form-group
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (text-field {:class "form-control", :disabled "disabled"} param-key (format-property-param entity-key property-key (get entity property-key)))]))

(defmethod input-control :default
  [entity-key entity property-key]
  nil)

(defn input-form-panel
  [entity-key entity method uri submit-button-caption]
  (form-to
   [method uri]
   (hidden-field :index-params (get-in *request* [:params :index-params]))
   (map (partial input-control entity-key entity) (property-keys entity-key))
   [:p
    (submit-button {:class "btn btn-primary"} (string/capitalize (dog-mission/translate submit-button-caption)))
    "&nbsp;"
    (link-to-with-method-and-params {:class "btn btn-default"} (<< "/~(name entity-key)") :get (base64-decode (get-in *request* [:params :index-params])) (string/capitalize (dog-mission/translate :back)))]))

;; views.

(defn list-entities-panel
  [entity-key entities page-count page entity-command-fn command-fn]
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
                     (entity-command-fn entity)]])
                 entities)]]
          [:div.text-center (pagination-control page-count page)]
          [:p (command-fn)])))

(defn index-view
  [entity-key & [entities page-count page]]
  (layout (string/capitalize (dog-mission/translate :list-view-title (dog-mission/translate entity-key)))
          [:div.row
           [:div.col-md-3 (search-condition-panel entity-key)]
           [:div.col-md-9 (list-entities-panel    entity-key entities page-count page
                                                  (fn [entity]
                                                    (list (link-to-with-method-and-params
                                                           {:class "btn btn-primary btn-xs"} (<< "/~(name entity-key)/~(:key entity)/edit") :get {:index-params (base64-encode (:query-params *request*))}
                                                           [:span.glyphicon.glyphicon-pencil])
                                                          "&nbsp;"
                                                          (link-to-with-method-and-params
                                                           {:class "btn btn-danger btn-xs", :data-confirm (string/capitalize (dog-mission/translate :are-you-sure?))} (<< "/~(name entity-key)/~(:key entity)") :delete {:index-params (base64-encode (:query-params *request*))}
                                                           [:span.glyphicon.glyphicon-trash])))
                                                  (fn []
                                                    (link-to-with-method-and-params {:class "btn btn-primary"} (<< "/~(name entity-key)/new") :get {:index-params (base64-encode (:query-params *request*))} (string/capitalize (dog-mission/translate :new)))))]]))

(defn select-view
  [entity-key & [entities page-count page]]
  (let [target-element-id           (get-in *request* [:params :target-element-id])
        representative-property-key (representative-property-key entity-key)]
    (layout (string/capitalize (dog-mission/translate :select-view-title (dog-mission/translate entity-key)))
            [:div.row
             [:div.col-md-3 (search-condition-panel entity-key {:target-element-id (get-in *request* [:params :target-element-id])})]
             [:div.col-md-9 (list-entities-panel    entity-key entities page-count page
                                                    (fn [entity]
                                                      (link-to-javascript
                                                       {:class "btn btn-primary btn-xs"} (<< "$.crossplane.setOpenerSelectProperty('~{target-element-id}', '~(:key entity)', '~(format-property-param entity-key representative-property-key (get entity representative-property-key))'); window.close()")
                                                       [:span.glyphicon.glyphicon-link]))
                                                    (fn []
                                                      (let [target-element-id (get-in *request* [:params :target-element-id])]
                                                        (list (link-to-javascript {:class "btn btn-default"} (<< "$.crossplane.setOpenerSelectProperty('~{target-element-id}', '', ''); window.close()") (string/capitalize (dog-mission/translate :select-nothing)))
                                                              "&nbsp;"
                                                              (link-to-javascript {:class "btn btn-default"} "window.close()" (string/capitalize (dog-mission/translate :cancel)))))))]])))

(defn new-view
  [entity-key entity]
  (layout (string/capitalize (dog-mission/translate :new-view-title (dog-mission/translate entity-key)))
          (input-form-panel entity-key entity :post (<< "/~(name entity-key)") :create)))

(defn edit-view
  [entity-key entity]
  (layout (string/capitalize (dog-mission/translate :edit-view-title (dog-mission/translate entity-key)))
          (input-form-panel entity-key entity :patch (<< "/~(name entity-key)/~(:key entity)") :update)))

;; Controllers.

(defmulti condition
  (fn [entity-key property-key] (property-type entity-key property-key)))

(defn- string-condition
  [entity-key property-key]
  (if-let [param-value (get-in *request* [:entity-params (param-key entity-key property-key)])]
    ($like property-key (format "%%%s%%" param-value))))

(defmethod condition :string
  [entity-key property-key]
  (string-condition entity-key property-key))

(defmethod condition :text
  [entity-key property-key]
  (string-condition entity-key property-key))

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

(defmethod condition :boolean
  [entity-key property-key]
  (let [param-value (get-in *request* [:entity-params (param-key entity-key property-key)])]
    (if-not (nil? param-value)
      (if param-value
        property-key
        ($not property-key)))))

(defmethod condition :date
  [entity-key property-key]
  (range-condition entity-key property-key))

(defmethod condition :timestamp
  [entity-key property-key]
  (range-condition entity-key property-key))

(defmethod condition :many-to-one
  [entity-key property-key]
  (if-let [param-value (get-in *request* [:entity-params (keyword (format "%s-key" (name (param-key entity-key property-key))))])]
    ($= (many-to-one-relationship-key-to-physical-column-key property-key) param-value)))

(defmethod condition :default
  [entity-key property-key]
  nil)

(defn condition-matched-entities-and-page
  [entity-key]
  (if (and (= (get-in *request* [:params :command]) "search") (empty? (:entity-param-errors *request*)))
    (let [pages      (->> (-> (->> (if-let [conditions (not-empty (keep (partial condition entity-key) (search-condition-property-keys entity-key)))]
                                     (apply $and conditions))
                                   (database-data @database-schema *transaction* entity-key)
                                   (database      @database-schema))
                              (get-condition-matched-rows entity-key))
                          ((apply partial sort-by (sort-keyfn-and-comp entity-key)))
                          (partition-all 20)
                          (not-empty))
          page-count (count pages)
          page       (let [request-page (Integer/parseInt (get-in *request* [:params :page]))]
                       (max (min request-page page-count) 1))]
      [(if (empty? pages)
         []
         (nth pages (dec page)))
       page-count
       page])))

(defn target-entity-database
  [entity-key uri-parameters]
  (->> (database-data @database-schema *transaction* entity-key ($= :key (UUID/fromString (:key uri-parameters))))
       (database      @database-schema)))

(defn index-controller
  [entity-key uri-parameters]
  (with-db-transaction'
    (apply index-view entity-key (condition-matched-entities-and-page entity-key))))

(defn select-controller
  [entity-key uri-parameters]
  (with-db-transaction'
    (apply select-view entity-key (condition-matched-entities-and-page entity-key))))

(defn new-controller
  [entity-key uri-parameters]
  (new-view entity-key {}))

(defn create-controller
  [entity-key uri-parameters]
  (with-db-transaction'
    (let [entity-schema (entity-schema entity-key)
          entity        (->> (reduce (fn [entity property-key]
                                       (let [many-to-one? (= (property-type entity-key property-key) :many-to-one)
                                             column-key   (cond-> property-key
                                                            many-to-one? (many-to-one-relationship-key-to-physical-column-key))
                                             param-key    (cond-> (param-key entity-key property-key)
                                                            many-to-one? (#(keyword (format "%s-key" (name %)))))
                                             errors       (not-empty (get-in *request* [:entity-param-errors param-key]))]
                                         (cond-> entity
                                           (not errors) (assoc column-key (get-in *request* [:entity-params param-key])))))
                                     {}
                                     (inputtable-property-keys entity-key)))]
      (if (not-empty (get-in *request* [:entity-param-errors]))
        (new-view entity-key entity)
        (let [database (-> (database @database-schema)
                           (assoc-in [entity-key (new-key)] entity))]
          (save! @database-schema database *transaction*)
          (layout (string/capitalize (dog-mission/translate :update-view-title (dog-mission/translate entity-key)))
                  (javascript-tag (<< "$.crossplane.goToWithMethodAndParams('~(to-uri (<< \"/~(name entity-key)\"))', 'get', ~(json/write-str (base64-decode (get-in *request* [:params :index-params]))));"))))))))
      
(defn edit-controller
  [entity-key uri-parameters]
  (with-db-transaction'
    (edit-view entity-key (get-in (target-entity-database entity-key uri-parameters) [entity-key (UUID/fromString (:key uri-parameters))]))))

(defn update-controller
  [entity-key uri-parameters]
  (with-db-transaction'
    (let [entity-schema (entity-schema entity-key)
          database      (->> (reduce (fn [database property-key]
                                       (let [many-to-one? (= (property-type entity-key property-key) :many-to-one)
                                             column-key   (cond-> property-key
                                                            many-to-one? (many-to-one-relationship-key-to-physical-column-key))
                                             param-key    (cond->> (param-key entity-key property-key)
                                                            many-to-one? (#(keyword (format "%s-key" (name %)))))
                                             errors       (not-empty (get-in *request* [:entity-param-errors param-key]))]
                                         (cond-> database
                                           (not errors) (assoc-in [entity-key (UUID/fromString (:key uri-parameters)) column-key] (get-in *request* [:entity-params param-key])))))
                                     (target-entity-database entity-key uri-parameters)
                                     (inputtable-property-keys entity-key)))]
      (if (not-empty (get-in *request* [:entity-param-errors]))
        (edit-view entity-key (get-in database [entity-key (UUID/fromString (:key uri-parameters))]))
        (do (save! @database-schema database *transaction*)
            (layout (string/capitalize (dog-mission/translate :update-view-title (dog-mission/translate entity-key)))
                    (javascript-tag (<< "$.crossplane.goToWithMethodAndParams('~(to-uri (<< \"/~(name entity-key)\"))', 'get', ~(json/write-str (base64-decode (get-in *request* [:params :index-params]))));"))))))))

(defn destroy-controller
  [entity-key uri-parameters]
  (layout (string/capitalize (dog-mission/translate :destroy-view-title (dog-mission/translate entity-key)))
          (list [:p "destroy"]
                (link-to-with-method-and-params {:class "btn btn-default"} (to-uri (<< "/~(name entity-key)")) :get (base64-decode (get-in *request* [:params :index-params])) (string/capitalize (dog-mission/translate :back))))))

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
                   [[:get    "/%s"           index-controller]  ; TODO: 画面生成を抑制するオプションを追加する。毎回うまいこと上書きできるとは限らないため。
                    [:get    "/%s/select"    select-controller]
                    [:get    "/%s/new"       new-controller]
                    [:post   "/%s"           create-controller]
                    [:get    "/%s/:key/edit" edit-controller]
                    [:patch  "/%s/:key"      update-controller]
                    [:delete "/%s/:key"      destroy-controller]])
            (entity-keys)))))
