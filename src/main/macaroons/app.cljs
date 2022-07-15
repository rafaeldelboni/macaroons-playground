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
(defn add-caveat-input [set-state]
  #(let [id (uuid/v4)]
     (set-state assoc-in [:caveats id] {:id id :value ""})))

(defn remove-caveat-input [set-state id]
  #(set-state update-in [:caveats] dissoc id))

(defn edit-caveat-input [set-state id]
  #(set-state update-in [:caveats id] assoc :value (.. % -target -value)))

;; components
(defnc serialized-macaroons
  "A component which shows a serial macaroon"
  [{:keys [serialized]}]
  (d/div "Serialized macaroon: " (d/strong (str serialized))))

(defnc deserialize-verify-macaroons
  "A component which greets a user."
  [{:keys [secret caveats serialized]}]
  (let [caveats-values (->> caveats vals (map :value))
        macaroon (.deserialize mac/MacaroonsBuilder serialized)
        verifier (verify macaroon secret caveats-values)]
    (d/div
     (d/h2 "Inspect:")
     (if serialized
       (d/div
        (d/pre (.inspect macaroon))
        (d/div "Is Valid?: " (d/strong (if verifier "yes" "no"))))
       (d/div "No data to inspect.")))))

(defnc caveats-input
  "A component which creates caveats inputs"
  [{:keys [set-state caveats]}]
  (for [caveat (vals caveats)
        :let [id (:id caveat)]]
    (d/div {:key id}
           (d/input {:value (:value caveat)
                     :on-change (edit-caveat-input set-state id)})
           (d/button {:on-click (remove-caveat-input set-state id)} "-"))))

;; app
(defnc app []
  (let [[state set-state] (hooks/use-state {:name "Helix User"
                                            :location ""
                                            :identifier ""
                                            :secret ""
                                            :caveats {}})
        {:keys [location secret identifier caveats]} state
        caveats-values (->> caveats vals (map :value))
        macaroon (.create mac/MacaroonsBuilder location secret identifier)
        macaroon_caveats (add-caveats macaroon caveats-values)
        serialized (.serialize macaroon_caveats)]
    (d/div
     (d/h1 "Macaroons Playground!")

     (d/div

      (d/div
       (d/label {:for "location"} "Location: ")
       (d/input {:value (:location state)
                 :id "location"
                 :placeholder "https://your.domain"
                 :on-change #(set-state assoc :location (.. % -target -value))}))

      (d/div
       (d/label {:for "identifier"} "Identifier: ")
       (d/input {:value (:identifier state)
                 :id "identifier"
                 :placeholder "user 123"
                 :on-change #(set-state assoc :identifier (.. % -target -value))}))

      (d/div
       (d/label {:for "secret"} "Secret: ")
       (d/input {:value (:secret state)
                 :id "secret"
                 :placeholder "huge-hash-of-your-secret"
                 :on-change #(set-state assoc :secret (.. % -target -value))}))

      (d/button {:on-click (add-caveat-input set-state)} "+ Caveat"))

     ($ caveats-input {:set-state set-state
                       :caveats (:caveats state)})

     ;; TODO: should be an input text
     ($ serialized-macaroons {:serialized serialized})

     ;; TODO: caveats and secret shold be in separated verify state
     ($ deserialize-verify-macaroons {:secret secret
                                      :caveats caveats
                                      :serialized serialized}))))

;; start your app with your React renderer
(defn ^:export init []
  (doto (rdom/createRoot (js/document.getElementById "app"))
    (.render ($ app))))

