(ns interest-is-interesting)

(defn interest-rate
  "Returns the interest rate based on the specified balance."
  [balance]
  (cond
    (< balance 0) -3.213
    (and (>= balance 0) (< balance 1000)) 0.5
    (and (>= balance 1000) (< balance 5000)) 1.621
    (>= balance 5000) 2.475
    )
  )


(defn annual-balance-update
  "Returns the annual balance update, taking into account the interest rate."
  [balance]
  (let [bd-balance (bigdec balance)
        rate-percentage (/ (bigdec (interest-rate balance))  (bigdec 100))]
    (cond
      (< balance 0)
      (bigdec (+ bd-balance (* bd-balance rate-percentage -1)))
      :else
      (bigdec (+ bd-balance (* bd-balance rate-percentage))))))


(defn amount-to-donate
  "Returns how much money to donate based on the balance and the tax-free percentage."
  [balance tax-free-percentage]

  (cond
    (<= balance 0)
    0
    :else
    (int (* 2 (* balance (bigdec (/ (bigdec tax-free-percentage)  100))))))
  )
