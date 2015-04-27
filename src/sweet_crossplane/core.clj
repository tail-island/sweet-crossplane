(ns sweet-crossplane.core
  (:require (clojure      [string   :as    string])
            (clojure.java [jdbc     :as    jdbc])
            (clout        [core     :as    clout])
            (compojure    [core     :as    compojure]
                          [response :as    compojure.response])
            (dog-mission  [core     :as    dog-mission])
            (hiccup       [core     :refer :all]
                          [element  :refer :all]
                          [form     :refer :all]
                          [page     :refer :all])
            (radial-mount [core     :as    radial-mount])
            (twin-spar    [core     :as    twin-spar])
            )
  (:import  (java.util   Locale TimeZone)))

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
               (keys @database-schema))]]]]]]
    [:article
     [:div.container-fluid
      contents]]
    [:footer
     [:div.container-fluid
      [:p "Powered by Clojure, core.incubator, Logging, clojure.java.jdbc, clj-time, Compojure, Hiccup, PostgreSQL, jQuery, Bootstrap and sweet-crossplane."]]]
    (include-js "/lib/jquery/jquery.min.js")
    (include-js "/lib/bootstrap/bootstrap.min.js")]))

(defn property-type
  [entity-schema property-key]
  (or (get-in entity-schema [:columns property-key :type])
      (and (get-in entity-schema [:many-to-one-relationship property-key]) :many-to-one)
      (and (get-in entity-schema [:one-to-many-relationship property-key]) :one-to-many)))

(defmulti search-condition-control
  (fn [entity-key entity-schema property-key] (property-type entity-schema property-key)))

(defmethod search-condition-control :string
  [entity-key entity-schema property-key]
  [:div.form-group
   (label property-key (string/capitalize (dog-mission/translate (radial-mount/property-message-key entity-key property-key))))
   (text-field {:class "form-control", :placeholder (get-in entity-schema [:placeholders property-key])}
               property-key
               (get-in *request* [:params property-key]))])

(defmethod search-condition-control :decimal
  [entity-key entity-schema property-key]
  (let [[property-key-1 property-key-2] (map #(keyword (format "%s-%d--" (name %1) %2)) (repeat property-key) (iterate inc 1))]
    [:div.form-group
     (label property-key (string/capitalize (dog-mission/translate (radial-mount/property-message-key entity-key property-key))))
     (text-field {:class "form-control"}
                 property-key-1
                 (get-in *request* [:params property-key-1]))
     (string/capitalize (dog-mission/translate :-))
     (text-field {:class "form-control"}
                 property-key-2
                 (get-in *request* [:params property-key-2]))]))

(defmethod search-condition-control :default
  [entity-key entity-schema property-key]
  nil)

(defn index-view
  [entity-key {:keys [columns many-to-one-relationships] :as entity-schema} entities]
  (@layout (dog-mission/translate :list-view-title (dog-mission/translate entity-key))
           [:div.row
            [:div.col-md-3
             [:div.panel.panel-default
              [:div.panel-heading
               [:div.panel-title (string/capitalize (dog-mission/translate :search-condition))]]
              [:div.panel-body
               (form-to
                [:get (:uri *request*)]
                (map (partial search-condition-control entity-key entity-schema) (concat (keys columns) (keys many-to-one-relationships)))
                (submit-button {:class "btn btn-primary"} (string/capitalize (dog-mission/translate :search))))]]]
            [:div.col-md-9
             [:p "List"]]]))

;; Controllers.

(defn index-controller
  [entity-key entity-schema uri-parameters]
  (jdbc/with-db-transaction [transaction @database-spec]
    (index-view entity-key entity-schema nil)))

(defn new-controller
  [entity-key entity-schema uri-parameters]
  (@layout (dog-mission/translate :new-view-title (dog-mission/translate entity-key))
           [:div
            [:p "new"]
            (form-to
             [:post (format "/%s" (name entity-key))]
             (submit-button "create"))]))

(defn create-controller
  [entity-key entity-schema uri-parameters]
  (@layout (dog-mission/translate :create-view-title (dog-mission/translate entity-key))
           [:p "create"]))

(defn edit-controller
  [entity-key entity-schema uri-parameters]
  (@layout (dog-mission/translate :edit-view-title (dog-mission/translate entity-key))
           [:div
            [:p "edit"]
            (form-to
             [:patch  (format "/%s/%s" (name entity-key) (:key uri-parameters))]
             (submit-button "update"))
            (form-to
             [:delete (format "/%s/%s" (name entity-key) (:key uri-parameters))]
             (submit-button "destroy"))]))

(defn update-controller
  [entity-key entity-schema uri-parameters]
  (@layout (dog-mission/translate :update-view-title (dog-mission/translate entity-key))
           [:p "update"]))

(defn destroy-controller
  [entity-key entity-schema uri-parameters]
  (@layout (dog-mission/translate :destroy-view-title (dog-mission/translate entity-key))
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
      (some (fn [[entity-key entity-schema]]
              (some (fn [[method path-format controller]]
                      (if-let [uri-parameters (route-matches method (format path-format (name entity-key)))]
                        (compojure.response/render (controller entity-key entity-schema uri-parameters) request)))
                    [[:get    "/%s"           index-controller]
                     [:get    "/%s/new"       new-controller]
                     [:post   "/%s"           create-controller]
                     [:get    "/%s/:key/edit" edit-controller]
                     [:patch  "/%s/:key"      update-controller]
                     [:delete "/%s/:key"      destroy-controller]]))
            @database-schema))))
