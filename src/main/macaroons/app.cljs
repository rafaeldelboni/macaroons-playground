(ns macaroons.app
  (:require ["macaroons.js" :as mac]
            ["react-dom/client" :as rdom]
            ["uuid" :as uuid]
            [helix.core :refer [$ defnc]]
            [helix.dom :as d]
            [helix.hooks :as hooks]))

;; macaroons fns
(defn add-caveats [macaroon caveats]
  (if (seq caveats)
    (let [modifier (.modify mac/MacaroonsBuilder macaroon)]
      (doseq [caveat caveats
              :when (seq caveat)]
        (.add_first_party_caveat modifier caveat))
      (.getMacaroon modifier))
    macaroon))

(defn verify [macaroon secret caveats]
  (let [verifier (new mac/MacaroonsVerifier macaroon)
        time-verifier (:TimestampCaveatVerifier (js->clj mac/verifier :keywordize-keys true))]
    (doseq [caveat caveats]
      (.satisfyExact verifier caveat))
    (.satisfyGeneral verifier time-verifier)
    (.isValid verifier secret)))

;; state managing fns
(defn add-caveat-input [set-state caveat-key]
  #(let [id (uuid/v4)]
     (set-state assoc-in [caveat-key id] {:id id :value ""})))

(defn remove-caveat-input [set-state caveat-key id]
  #(set-state update-in [caveat-key] dissoc id))

(defn edit-caveat-input [set-state caveat-key id]
  #(set-state update-in [caveat-key id] assoc :value (.. % -target -value)))

;; clipboard function
(defn copy-to-clip [value]
  (-> (.writeText js/navigator.clipboard value)
      (.then #(js/console.log "result:" %))
      (.catch #(js/console.log "error:" %))
      (.finally #(js/console.log "cleanup"))))

;; components
(defnc serialized-macaroons
  "A component which shows a serial macaroon"
  [{:keys [serialized]}]
  (let [[state set-state] (hooks/use-state {:tooltip-display nil})]
    (d/div
     (d/div
      (d/h3 "Serialized macaroon: ")
      (d/cite (str serialized)))
     (d/button {:data-tooltip (:tooltip-display state)
                :style {:margin-top "20px"}
                :on-mouse-out #(set-state assoc :tooltip-display nil)
                :on-click #(do (copy-to-clip serialized)
                               (set-state assoc :tooltip-display "Copied."))} "Copy"))))

(defnc inspect-macaroons
  "A component which shows macaroons inpect"
  [{:keys [macaroon]}]
  (d/div (d/pre (.inspect macaroon))))

(defnc verify-macaroons
  "A component which shows macaroons validity"
  [{:keys [secret caveats macaroon]}]
  (let [caveats-values (->> caveats vals (map :value))
        verifier (verify macaroon secret caveats-values)]
    (d/div "Is Valid?: " (d/strong (if verifier "yes" "no")))))

(defnc deserialize-verify-macaroons
  "A component which shows macaroons inpect"
  [{:keys [secret caveats serialized]}]
  (let [macaroon (try (.deserialize mac/MacaroonsBuilder serialized)
                      (catch js/Error _ nil))]
    (d/div
     (d/h3 "Inspect:")
     (if macaroon
       (d/div
        ($ inspect-macaroons {:macaroon macaroon})
        ($ verify-macaroons {:macaroon macaroon
                             :caveats caveats
                             :secret secret}))
       (d/div "No data to inspect.")))))

(defnc caveats-input
  "A component which creates caveats inputs"
  [{:keys [set-state caveats caveat-key]}]
  (for [caveat (vals caveats)
        :let [id (:id caveat)]]
    (d/div {:class "grid"
            :key id}
           (d/input {:value (:value caveat)
                     :placeholder "caveat=value"
                     :on-change (edit-caveat-input set-state caveat-key id)})
           (d/button {:class "secondary"
                      :on-click (remove-caveat-input set-state caveat-key id)} "Delete"))))

;; app
(defnc app []
  (let [[state set-state] (hooks/use-state {:new/location ""
                                            :new/identifier ""
                                            :new/secret ""
                                            :new/caveats {}
                                            :verify/serialized ""
                                            :verify/secret ""
                                            :verify/caveats {}})
        {:new/keys [location secret identifier caveats]} state
        caveats-values (->> caveats vals (map :value))
        macaroon (.create mac/MacaroonsBuilder location secret identifier)
        macaroon_caveats (add-caveats macaroon caveats-values)
        serialized (.serialize macaroon_caveats)]
    (d/div
     (d/h1 "Macaroons Playground!")

     (d/div {:class "grid"}
            (d/article

             (d/header (d/h2 "New"))

             (d/div
              (d/label {:for "location"} "Location: ")
              (d/input {:value (:location state)
                        :id "location"
                        :placeholder "https://your.domain"
                        :on-change #(set-state assoc :new/location (.. % -target -value))}))

             (d/div
              (d/label {:for "identifier"} "Identifier: ")
              (d/input {:value (:identifier state)
                        :id "identifier"
                        :placeholder "user 123"
                        :on-change #(set-state assoc :new/identifier (.. % -target -value))}))

             (d/div
              (d/label {:for "secret"} "Secret: ")
              (d/input {:value (:secret state)
                        :id "secret"
                        :placeholder "huge-hash-of-your-secret"
                        :on-change #(set-state assoc :new/secret (.. % -target -value))}))

             (d/button {:on-click (add-caveat-input set-state :new/caveats)} "+ Caveat")

             ($ caveats-input {:set-state set-state
                               :caveats (:new/caveats state)
                               :caveat-key :new/caveats})

             ($ deserialize-verify-macaroons {:secret secret
                                              :caveats caveats
                                              :serialized serialized})

             ($ serialized-macaroons {:serialized serialized}))

            (d/article

             (d/header (d/h2 "Verify"))

             (d/div
              (d/label {:for "serialized"} "Serialized Macaroon: ")
              (d/input {:value (:serialized state)
                        :id "serialized"
                        :placeholder "MDAwZmxvY2F0aW9..."
                        :on-change #(set-state assoc :verify/serialized (.. % -target -value))}))

             (d/div
              (d/label {:for "secret"} "Secret: ")
              (d/input {:value (:secret state)
                        :id "secret"
                        :placeholder "huge-hash-of-your-secret"
                        :on-change #(set-state assoc :verify/secret (.. % -target -value))}))

             (d/button {:on-click (add-caveat-input set-state :verify/caveats)} "+ Caveat")

             ($ caveats-input {:set-state set-state
                               :caveats (:verify/caveats state)
                               :caveat-key :verify/caveats})

             ($ deserialize-verify-macaroons {:secret (:verify/secret state)
                                              :caveats (:verify/caveats state)
                                              :serialized (:verify/serialized state)}))))))

;; start your app with your React renderer
(defn ^:export init []
  (doto (rdom/createRoot (js/document.getElementById "app"))
    (.render ($ app))))
