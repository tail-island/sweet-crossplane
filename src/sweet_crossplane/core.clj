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
            (java.sql           SQLException)
            (java.text          ParseException)
            (java.util          Locale TimeZone UUID)
            (org.joda.time      DateTimeZone)))

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
      (binding [dog-mission/*time-zone*      (TimeZone/getTimeZone time-zone-id)
                dog-mission/*joda-time-zone* (DateTimeZone/forTimeZone dog-mission/*time-zone*)]
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
  [& args]
  (apply radial-mount/property-message-key args))

(declare entity-class-keys inputtable-property-keys parse-property-param)

(defn wrap-entity-params
  [handler]
  (letfn [(assoc-entity-param [request param-key entity-class-key property-key]
            (if-let [param-value (not-empty (get-in request [:params param-key]))]
              (try
                (assoc-in request [:entity-params param-key] (parse-property-param entity-class-key property-key param-value))
                (catch ParseException _
                  (update-in request [:entity-param-errors param-key] #(vec (conj % (string/capitalize (dog-mission/translate :has-the-wrong-format (dog-mission/translate param-key))))))))))]
    (fn [request]
      (handler (reduce (fn [request entity-class-key]
                         (reduce (fn [request [property-key param-key]]
                                   (cond-> request
                                     (.startsWith (name param-key) (name (sweet-crossplane.core/param-key entity-class-key property-key))) (#(or (assoc-entity-param % param-key entity-class-key property-key) %))))
                                 request
                                 (combinatorics/cartesian-product (inputtable-property-keys entity-class-key) (keys (:params request)))))
                       request
                       (entity-class-keys))))))

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

;; TODO: STIに対応する。。。面倒くさい。。。

(defn entity-class-keys
  []
  (keys @database-schema))

(defn entity-class-schema
  [entity-class-key]
  (get @database-schema entity-class-key))

(defn property-keys
  ([entity-class-key & property-types]
   (->> (entity-class-schema entity-class-key)
        ((apply juxt property-types))
        (map keys)
        (apply concat)))
  ([entity-class-key]
   (property-keys entity-class-key :columns :many-to-one-relationships :one-to-many-relationships)))

(defn inputtable-property-keys
  [entity-class-key]
  (property-keys entity-class-key :columns :many-to-one-relationships))

(defn representative-property-key
  [entity-class-key]
  (or (:representative (entity-class-schema entity-class-key))
      (some #{:name} (property-keys entity-class-key))
      (first (property-keys entity-class-key))))

(defn placeholder
  [entity-class-key property-key]
  (get-in (entity-class-schema entity-class-key) [:placeholders property-key]))

(defn search-condition-property-keys
  [entity-class-key]
  (or (get-in (entity-class-schema entity-class-key) [:search-condition :properties])
      (inputtable-property-keys entity-class-key)))

(defn list-property-keys
  [entity-class-key]
  (or (get-in (entity-class-schema entity-class-key) [:list :properties])
      (property-keys entity-class-key)))

(defn input-property-keys
  [entity-class-key]
  (or (get-in (entity-class-schema entity-class-key) [:input :properties])
      (property-keys entity-class-key)))

;; TODO: STIに合わせて仕様変更する。

(defn sort-keyfn-and-comp
  [entity-class-key]
  (let [{:keys [key comp]} (get-in (entity-class-schema entity-class-key) [:list :sort-by])]
    [(or key  (first (list-property-keys entity-class-key)))
     (or comp compare)]))

(defn property-schema
  [entity-class-key property-key]
  (let [entity-class-schema (entity-class-schema entity-class-key)]
    (or (get-in entity-class-schema [:columns                   property-key])
        (get-in entity-class-schema [:many-to-one-relationships property-key])
        (get-in entity-class-schema [:one-to-many-relationships property-key]))))

(defn property-type
  [entity-class-key property-key]
  (let [entity-class-schema (entity-class-schema entity-class-key)]
    (or (get-in entity-class-schema [:columns property-key :type])
        (and (get-in entity-class-schema [:many-to-one-relationships property-key]) :many-to-one)
        (and (get-in entity-class-schema [:one-to-many-relationships property-key]) :one-to-many))))

(defmulti parse-property-param
  (fn [entity-class-key property-key param-value] (property-type entity-class-key property-key)))

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
  (fn [entity-class-key property-key param-value] (property-type entity-class-key property-key)))

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
  [entity-class-key property-key param-value]
  (if param-value
    (if (map? param-value)
      (let [entity-class-key (:table-key (property-schema entity-class-key property-key))
            property-key     (representative-property-key entity-class-key)]
        (format-property-param entity-class-key property-key (get param-value property-key)))
      (dog-mission/l10n-format param-value))))

(defmethod format-property-param :many-to-one
  [entity-class-key property-key param-value]
  (format-relationship-property-param entity-class-key property-key param-value))

(defmethod format-property-param :one-to-many
  [entity-class-key property-key param-value]
  (string/join (dog-mission/translate :comma) (map (partial format-relationship-property-param entity-class-key property-key) param-value)))

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
          (map (fn [entity-class-key]
                 [:li (link-to (<< "/~(name entity-class-key)") (string/capitalize (dog-mission/translate entity-class-key)))])
               (entity-class-keys))]]]]]]
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

;; controls for search.

(defmulti condition-control
  (fn [entity-class-key property-key] (property-type entity-class-key property-key)))

(defn- string-condition-control
  [entity-class-key property-key]
  (let [param-key    (param-key entity-class-key property-key)
        parse-errors (not-empty (get-in *request* [:entity-param-errors param-key]))]
    [:div.form-group
     (if parse-errors
       {:class "has-error has-feedback"})
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (text-field
      {:class "form-control", :placeholder (placeholder entity-class-key property-key)}
      param-key
      (if parse-errors
        (get-in *request* [:params param-key])
        (format-property-param entity-class-key property-key (get-in *request* [:entity-params param-key]))))]))

(defmethod condition-control :string
  [& args]
  (apply string-condition-control args))

(defmethod condition-control :text
  [& args]
  (apply string-condition-control args))

(defn- number-condition-control
  [entity-class-key property-key]
  (let [param-key      (param-key entity-class-key property-key)
        param-key-1    (keyword (format "%s-1--" (name param-key)))
        param-key-2    (keyword (format "%s-2--" (name param-key)))
        parse-errors-1 (not-empty (get-in *request* [:entity-param-errors param-key-1]))
        parse-errors-2 (not-empty (get-in *request* [:entity-param-errors param-key-2]))]
    (letfn [(text-field [param-key parse-errors]
              [:div.form-group
               (if parse-errors
                 {:class "has-error has-feedback"})
               (hiccup.form/text-field
                {:class "form-control"}
                param-key
                (if parse-errors
                  (get-in *request* [:params param-key])
                  (format-property-param entity-class-key property-key (get-in *request* [:entity-params param-key]))))])]
      (list
       (label param-key (string/capitalize (dog-mission/translate param-key)))
       (text-field param-key-1 parse-errors-1)
       (text-field param-key-2 parse-errors-2)))))

(defmethod condition-control :integer
  [& args]
  (apply number-condition-control args))

(defmethod condition-control :decimal
  [& args]
  (apply number-condition-control args))

(defmethod condition-control :boolean
  [entity-class-key property-key]
  (let [param-key (param-key entity-class-key property-key)]
    [:div.form-group
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (drop-down {:class "form-control"} param-key (map (partial format-property-param entity-class-key property-key) [nil true false]) (format-property-param entity-class-key property-key (get-in *request* [:entity-params param-key])))]))

(defn- date-timestamp-condition-control
  [entity-class-key property-key]
  (let [param-key      (param-key entity-class-key property-key)
        param-key-1    (keyword (format "%s-1--" (name param-key)))
        param-key-2    (keyword (format "%s-2--" (name param-key)))
        parse-errors-1 (not-empty (get-in *request* [:entity-param-errors param-key-1]))
        parse-errors-2 (not-empty (get-in *request* [:entity-param-errors param-key-2]))]
    (letfn [(text-field [param-key parse-errors]
              [:div.form-group
               (if parse-errors
                 {:class "has-error has-feedback"})
               [:div.input-group.date {:data-type (:type (property-schema entity-class-key property-key))}
                (hiccup.form/text-field
                 {:class "form-control"}
                 param-key
                 (if parse-errors
                   (get-in *request* [:params param-key])
                   (format-property-param entity-class-key property-key (get-in *request* [:entity-params param-key]))))
                [:span.input-group-addon
                 [:span.glyphicon.glyphicon-calendar]]]])]
      (list
       (label param-key (string/capitalize (dog-mission/translate param-key)))
       (text-field param-key-1 parse-errors-1)
       (text-field param-key-2 parse-errors-2)))))

(defmethod condition-control :date
  [& args]
  (apply date-timestamp-condition-control args))

(defmethod condition-control :timestamp
  [& args]
  (apply date-timestamp-condition-control args))

(defmethod condition-control :many-to-one
  [entity-class-key property-key]
  (let [param-key          (param-key entity-class-key property-key)
        param-key-key      (keyword (format "%s-key"    (name param-key)))
        param-key-name-key (keyword (format "%s-name--" (name param-key-key)))
        parse-errors       (get-in *request* [:entity-pararam-errors param-key])]
    [:div.form-group
     (if parse-errors
       {:class "has-error has-feedback"})
     (label param-key-key (string/capitalize (dog-mission/translate param-key)))
     (hidden-field param-key-key (get-in *request* [:entity-params param-key-key]))
     [:div.input-group.many-to-one
      (text-field {:class "form-control", :readonly "readonly"} param-key-name-key (format-property-param entity-class-key property-key (get-in *request* [:entity-params param-key-name-key])))
      [:span.input-group-addon {:data-open-window-uri (to-uri (<< "/~(name (:table-key (property-schema entity-class-key property-key)))/select?target-element-id=~(URLEncoder/encode (name param-key-key))"))}
       [:span.glyphicon.glyphicon-search]]]]))

;; TODO: ドロップダウンで選択できるようにする。

(defmethod condition-control :default
  [entity-class-key property-key]
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
  [entity-class-key & [params]]
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
     (map (partial condition-control entity-class-key) (search-condition-property-keys entity-class-key))
     [:button.btn.btn-primary {:type "submit", :name :command, :value "search"}
      (string/capitalize (dog-mission/translate :search))])]])

(defn list-entities-panel
  [entity-class-key entities page-count page entity-command-fn command-fn]
  (if entities
    (list [:table.table.table-condensed.table-hover
           [:thead
            [:tr
             (map (fn [property-key]
                    [:th (string/capitalize (dog-mission/translate (param-key entity-class-key property-key)))])
                  (list-property-keys entity-class-key))
             [:th]]]
           [:tbody
            (map (fn [entity]
                   [:tr
                    (map (fn [property-key]
                           [:td
                            [:div (format-property-param entity-class-key property-key (get entity property-key))]])
                         (list-property-keys entity-class-key))
                    [:td.command-cell
                     (entity-command-fn entity)]])
                 entities)]]
          [:div.text-center (pagination-control page-count page)]
          [:p (command-fn)])))

;; controls for input value.

(defmulti input-control
  (fn [entity-class-key entity property-key errors] (property-type entity-class-key property-key)))

(defn- string-input-control
  [entity-class-key entity property-key errors]
  (let [param-key    (param-key entity-class-key property-key)
        parse-errors (not-empty (get-in *request* [:entity-param-errors param-key]))]
    [:div.form-group
     (if (or parse-errors errors)
       {:class "has-error has-feedback"})
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     ((cond-> text-field
        (= (:type (property-schema entity-class-key property-key)) :text) ((constantly text-area)))
      {:class "form-control", :placeholder (placeholder entity-class-key property-key)}
      param-key
      (if parse-errors
        (get-in *request* [:params param-key])
        (format-property-param entity-class-key property-key (get entity property-key))))]))

(defmethod input-control :string
  [& args]
  (apply string-input-control args))

(defmethod input-control :text
  [& args]
  (apply string-input-control args))

(defn- number-input-control
  [entity-class-key entity property-key errors]
  (let [param-key    (param-key entity-class-key property-key)
        parse-errors (not-empty (get-in *request* [:entity-param-errors param-key]))]
    [:div.form-group
     (if (or parse-errors errors)
       {:class "has-error has-feedback"})
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (text-field
      {:class "form-control"}
      param-key
      (if parse-errors
        (get-in *request* [:params param-key])
        (format-property-param entity-class-key property-key (get entity property-key))))]))

(defmethod input-control :integer
  [& args]
  (apply number-input-control args))

(defmethod input-control :decimal
  [& args]
  (apply number-input-control args))

(defmethod input-control :boolean
  [entity-class-key entity property-key errors]
  (let [param-key    (param-key entity-class-key property-key)
        parse-errors (not-empty (get-in *request* [:entity-param-errors param-key]))]
    [:div.form-group
     (if (or parse-errors errors)
       {:class "has-error has-feedback"})
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (drop-down {:class "form-control"} param-key (map (partial format-property-param entity-class-key property-key) [nil true false]) (format-property-param entity-class-key property-key (get entity property-key)))]))

(defn- date-timestamp-input-control
  [entity-class-key entity property-key errors]
  (let [param-key    (param-key entity-class-key property-key)
        parse-errors (not-empty (get-in *request* [:entity-param-errors param-key]))]
    [:div.form-group
     (if (or parse-errors errors)
       {:class "has-error has-feedback"})
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     [:div.input-group.date {:data-type (:type (property-schema entity-class-key property-key))}
      (hiccup.form/text-field
       {:class "form-control"}
       param-key
       (if parse-errors
         (get-in *request* [:params param-key])
         (format-property-param entity-class-key property-key (get entity property-key))))
      [:span.input-group-addon
       [:span.glyphicon.glyphicon-calendar]]]]))

(defmethod input-control :date
  [& args]
  (apply date-timestamp-input-control args))

(defmethod input-control :timestamp
  [& args]
  (apply date-timestamp-input-control args))

(defmethod input-control :many-to-one
  [entity-class-key entity property-key errors]
  (let [param-key          (param-key entity-class-key property-key)
        param-key-key      (keyword (format "%s-key"    (name param-key)))
        param-key-name-key (keyword (format "%s-name--" (name param-key-key)))
        parse-errors       (get-in *request* [:entity-pararam-errors param-key])]
    [:div.form-group
     (if (or parse-errors errors)
       {:class "has-error has-feedback"})
     (label param-key-key (string/capitalize (dog-mission/translate param-key)))
     (hidden-field param-key-key (get entity (many-to-one-relationship-key-to-physical-column-key property-key)))
     [:div.input-group.many-to-one
      (text-field {:class "form-control", :readonly "readonly"} param-key-name-key (format-property-param entity-class-key property-key (get entity property-key)))
      [:span.input-group-addon {:data-open-window-uri (to-uri (<< "/~(name (:table-key (property-schema entity-class-key property-key)))/select?target-element-id=~(URLEncoder/encode (name param-key-key))"))}
       [:span.glyphicon.glyphicon-search]]]]))

;; TODO: ドロップダウンで選択できるようにする。

(defmethod input-control :one-to-many
  [entity-class-key entity property-key errors]
  (let [param-key (param-key entity-class-key property-key)]
    [:div.form-group
     (label param-key (string/capitalize (dog-mission/translate param-key)))
     (text-area {:class "form-control", :disabled "disabled"} param-key (format-property-param entity-class-key property-key (get entity property-key)))]))

(defmethod input-control :default
  [entity-class-key entity property-key errors]
  nil)

(defn error-message-control
  [entity-class-key parse-errors entity-errors]
  [:ul.bg-danger
   (map (fn [property-key]
          (let [param-key (param-key entity-class-key property-key)]
            (if-let [errors (not-empty (concat (get parse-errors param-key)
                                               (get entity-errors property-key)))]
              (map (fn [error]
                     [:li error])
                   errors))))
        (property-keys entity-class-key))])

(defn input-form-panel
  [entity-class-key entity entity-errors method uri submit-button-caption]
  (form-to
   [method uri]
   (hidden-field :index-params (get-in *request* [:params :index-params]))
   (error-message-control entity-class-key (get-in *request* [:entity-param-errors]) entity-errors)
   (map #(input-control entity-class-key entity % (get entity-errors %)) (input-property-keys entity-class-key))
   [:p
    (submit-button {:class "btn btn-primary"} (string/capitalize (dog-mission/translate submit-button-caption)))
    "&nbsp;"
    (link-to-with-method-and-params {:class "btn btn-default"} (<< "/~(name entity-class-key)") :get (base64-decode (get-in *request* [:params :index-params])) (string/capitalize (dog-mission/translate :back)))]))

;; views.

(defn index-view
  [entity-class-key & [entities page-count page]]
  (layout (string/capitalize (dog-mission/translate :list-view-title (dog-mission/translate entity-class-key)))
          [:div.row
           [:div.col-md-3 (search-condition-panel entity-class-key)]
           [:div.col-md-9 (list-entities-panel    entity-class-key entities page-count page
                                                  (fn [entity]
                                                    (list (link-to-with-method-and-params
                                                           {:class "btn btn-primary btn-xs"} (<< "/~(name entity-class-key)/~(:key entity)/edit") :get {:index-params (base64-encode (:query-params *request*))}
                                                           [:span.glyphicon.glyphicon-pencil])
                                                          "&nbsp;"
                                                          (link-to-with-method-and-params
                                                           {:class "btn btn-danger btn-xs", :data-confirm (string/capitalize (dog-mission/translate :are-you-sure?))} (<< "/~(name entity-class-key)/~(:key entity)") :delete {:index-params (base64-encode (:query-params *request*))}
                                                           [:span.glyphicon.glyphicon-trash])))
                                                  (fn []
                                                    (link-to-with-method-and-params {:class "btn btn-primary"} (<< "/~(name entity-class-key)/new") :get {:index-params (base64-encode (:query-params *request*))} (string/capitalize (dog-mission/translate :new)))))]]))

(defn select-view
  [entity-class-key & [entities page-count page]]
  (let [target-element-id           (get-in *request* [:params :target-element-id])
        representative-property-key (representative-property-key entity-class-key)]
    (layout (string/capitalize (dog-mission/translate :select-view-title (dog-mission/translate entity-class-key)))
            [:div.row
             [:div.col-md-3 (search-condition-panel entity-class-key {:target-element-id (get-in *request* [:params :target-element-id])})]
             [:div.col-md-9 (list-entities-panel    entity-class-key entities page-count page
                                                    (fn [entity]
                                                      (link-to-javascript {:class "btn btn-primary btn-xs"} (<< "$.crossplane.setOpenerSelectProperty('~{target-element-id}', '~(:key entity)', '~(format-property-param entity-class-key representative-property-key (get entity representative-property-key))'); window.close()")
                                                       [:span.glyphicon.glyphicon-link]))
                                                    (fn []
                                                      (let [target-element-id (get-in *request* [:params :target-element-id])]
                                                        (list (link-to-javascript {:class "btn btn-default"} (<< "$.crossplane.setOpenerSelectProperty('~{target-element-id}', '', ''); window.close()") (string/capitalize (dog-mission/translate :select-nothing)))
                                                              "&nbsp;"
                                                              (link-to-javascript {:class "btn btn-default"} "window.close()" (string/capitalize (dog-mission/translate :cancel)))))))]])))

(defn new-view
  [entity-class-key entity & [entity-errors]]
  (layout (string/capitalize (dog-mission/translate :new-view-title (dog-mission/translate entity-class-key)))
          (input-form-panel entity-class-key entity entity-errors :post (<< "/~(name entity-class-key)") :create)))

(defn edit-view
  [entity-class-key entity & [entity-errors]]
  (layout (string/capitalize (dog-mission/translate :edit-view-title (dog-mission/translate entity-class-key)))
          (input-form-panel entity-class-key entity entity-errors :patch (<< "/~(name entity-class-key)/~(:key entity)") :update)))

(defn back-to-index-view
  [entity-class-key]
  (layout ""
          (javascript-tag (<< "$.crossplane.goToWithMethodAndParams('~(to-uri (<< \"/~(name entity-class-key)\"))', 'get', ~(json/write-str (base64-decode (get-in *request* [:params :index-params]))));"))))

;; Controllers.

(defmulti condition
  (fn [entity-class-key property-key] (property-type entity-class-key property-key)))

(defn- string-condition
  [entity-class-key property-key]
  (if-let [param-value (get-in *request* [:entity-params (param-key entity-class-key property-key)])]
    ($like property-key (format "%%%s%%" param-value))))

(defmethod condition :string
  [& args]
  (apply string-condition args))

(defmethod condition :text
  [& args]
  (apply string-condition args))

(defn- range-condition
  [entity-class-key property-key]
  (let [param-key (param-key entity-class-key property-key)]
    (if-let [conditions (not-empty (keep identity [(if-let [param-value (get-in *request* [:entity-params (keyword (format "%s-1--" (name param-key)))])]
                                                     ($>= property-key param-value))
                                                   (if-let [param-value (get-in *request* [:entity-params (keyword (format "%s-2--" (name param-key)))])]
                                                     ($<= property-key param-value))]))]
      (apply $and conditions))))

(defmethod condition :integer
  [& args]
  (apply range-condition args))

(defmethod condition :decimal
  [& args]
  (apply range-condition args))

(defmethod condition :boolean
  [entity-class-key property-key]
  (let [param-value (get-in *request* [:entity-params (param-key entity-class-key property-key)])]
    (if-not (nil? param-value)
      (if param-value
        property-key
        ($not property-key)))))

(defmethod condition :date
  [& args]
  (apply range-condition args))

(defmethod condition :timestamp
  [& args]
  (apply range-condition args))

(defmethod condition :many-to-one
  [entity-class-key property-key]
  (if-let [param-value (get-in *request* [:entity-params (keyword (format "%s-key" (name (param-key entity-class-key property-key))))])]
    ($= (many-to-one-relationship-key-to-physical-column-key property-key) param-value)))

(defmethod condition :default
  [entity-class-key property-key]
  nil)

(defn condition-matched-entities-and-page
  [entity-class-key]
  (if (and (= (get-in *request* [:params :command]) "search") (empty? (:entity-param-errors *request*)))
    (let [entity-pages (->> (-> (->> (if-let [conditions (not-empty (keep (partial condition entity-class-key) (search-condition-property-keys entity-class-key)))]
                                       (apply $and conditions))
                                     (database-data @database-schema *transaction* entity-class-key)
                                     (database      @database-schema))
                                (get-condition-matched-rows entity-class-key))
                            ((apply partial sort-by (sort-keyfn-and-comp entity-class-key)))
                            (partition-all 20)
                            (not-empty))
          page-count   (count entity-pages)
          page         (let [request-page (Integer/parseInt (get-in *request* [:params :page]))]
                         (max (min request-page page-count) 1))]
      [(if (empty? entity-pages)
         []
         (nth entity-pages (dec page)))
       page-count
       page])))

(defn entity-contained-database
  [entity-class-key entity-key]
  (->> (database-data @database-schema *transaction* entity-class-key ($= :key entity-key))
       (database      @database-schema)))

(defn create-or-update-entity!
  [entity-class-key database entity-key new-or-edit-view-fn]
  (let [entity-class-schema (entity-class-schema entity-class-key)
        database            (->> (reduce (fn [database property-key]
                                           (if (some #{property-key} (inputtable-property-keys entity-class-key))
                                             (let [many-to-one? (= (property-type entity-class-key property-key) :many-to-one)
                                                   column-key   (cond-> property-key
                                                                  many-to-one? (many-to-one-relationship-key-to-physical-column-key))
                                                   param-key    (cond-> (param-key entity-class-key property-key)
                                                                  many-to-one? (#(keyword (format "%s-key" (name %)))))
                                                   param-value  (get-in *request* [:entity-params param-key])
                                                   errors       (not-empty (get-in *request* [:entity-param-errors param-key]))]
                                               (cond-> database
                                                 (not errors) (-> (cond-> many-to-one? (#(->> (database-data           @database-schema *transaction* (:table-key (property-schema entity-class-key property-key)) ($= :key param-value) (get-data %))
                                                                                              (twin-spar.core/database @database-schema))))
                                                                  (assoc-in [entity-class-key entity-key column-key] param-value))))
                                             database))
                                         database
                                         (input-property-keys entity-class-key)))
        entity-errors       (or (and (not-empty (get-in *request* [:entity-param-errors])) {})
                                (let [database (if-let [before-validate (:before-validate entity-class-schema)]
                                                 (before-validate database entity-class-key entity-key)
                                                 database)]
                                  (or (not-empty (get-in (radial-mount/validate @database-schema database) [entity-class-key entity-key]))
                                      (try
                                        (save! @database-schema database *transaction*)
                                        nil
                                        (catch SQLException ex
                                          (or (if-let [sql-exception-catch (:sql-exception-catch entity-class-schema)]
                                                (sql-exception-catch ex))
                                              (throw ex)))))))]
    (if entity-errors
      (new-or-edit-view-fn entity-class-key (get-in database [entity-class-key entity-key]) entity-errors)
      (back-to-index-view entity-class-key))))

(defn delete-entity!
  [entity-class-key database entity-key]
  (save! @database-schema (dissoc-in database [entity-class-key entity-key]) *transaction*)
  (back-to-index-view entity-class-key))

(defn index-controller
  [entity-class-key uri-parameters]
  (with-db-transaction'
    (apply index-view entity-class-key (condition-matched-entities-and-page entity-class-key))))

(defn select-controller
  [entity-class-key uri-parameters]
  (with-db-transaction'
    (apply select-view entity-class-key (condition-matched-entities-and-page entity-class-key))))

(defn new-controller
  [entity-class-key uri-parameters]
  (with-db-transaction'
    (new-view entity-class-key {})))

(defn create-controller
  [entity-class-key uri-parameters]
  (with-db-transaction'
    (create-or-update-entity! entity-class-key (database @database-schema) (new-key) new-view)))

(defn edit-controller
  [entity-class-key uri-parameters]
  (with-db-transaction'
    (let [entity-key (UUID/fromString (:key uri-parameters))]
      (edit-view entity-class-key (get-in (entity-contained-database entity-class-key entity-key) [entity-class-key entity-key])))))

(defn update-controller
  [entity-class-key uri-parameters]
  (with-db-transaction'
    (let [entity-key (UUID/fromString (:key uri-parameters))]
      (create-or-update-entity! entity-class-key (entity-contained-database entity-class-key entity-key) entity-key edit-view))))

(defn destroy-controller
  [entity-class-key uri-parameters]
  (with-db-transaction'
    (let [entity-key (UUID/fromString (:key uri-parameters))]
      (delete-entity! entity-class-key (entity-contained-database entity-class-key entity-key) entity-key))))

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
                   [[:get    "/%s"           index-controller]  ; TODO: 画面生成を抑制するオプションを追加する。毎回うまいこと上書きできるとは限らないもんね。
                    [:get    "/%s/select"    select-controller]
                    [:get    "/%s/new"       new-controller]
                    [:post   "/%s"           create-controller]
                    [:get    "/%s/:key/edit" edit-controller]
                    [:patch  "/%s/:key"      update-controller]
                    [:delete "/%s/:key"      destroy-controller]])
            (entity-class-keys)))))

;; TODO: condition-controlとinput-controlの共通部分を抽出してリファクタリングする。できるかなぁ……。
