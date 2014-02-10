(ns p-p-p-pokerface)

(defn face->value [face]
  "Convert card face to value"
  (get (zipmap [\T \J \Q \K \A] (range 10 15)) face))

(defn rank [[r]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (face->value r)))

(defn suit [[_ s]]
  (str s))

(defn card-n? [hand n]
  "Verify matching cards at least n"
  (boolean (some #(<= n %) (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (card-n? hand 2))

(defn three-of-a-kind? [hand]
  (card-n? hand 3))

(defn four-of-a-kind? [hand]
  (card-n? hand 4))

(defn flush? [hand]
   (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (= #{2 3}
     (set (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (->> ranks frequencies vals)
        pairs (filter #(= 2 %) freqs)]
    (= 2 (count pairs))))

(defn straight? [hand]
  (letfn [(straight-helper? [ranks]
            (let [n (count ranks)
                  m (apply min ranks)
                  real-straight-ranks (range m (+ n m))]
              (= (sort ranks) real-straight-ranks)))]
    (let [ranks (map rank hand)]
      (or
        (straight-helper? ranks)
        (straight-helper? (replace {14 1} ranks))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers (list [straight-flush? 8]
                       [four-of-a-kind? 7]
                       [full-house? 6]
                       [flush? 5]
                       [straight? 4]
                       [three-of-a-kind? 3]
                       [two-pairs? 2]
                       [pair? 1]
                       [high-card? 0])
        matched-checker (first (filter #((first %) hand) checkers))]
    (second matched-checker)))


