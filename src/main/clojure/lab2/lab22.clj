(ns lab2.lab22)

; 2.2. Оптимизируйте функцию с помощью бесконечной последовательности частичных решений

; Вычисление площади трапеции
(defn trapezoidArea [f a b]
  (* (/ (+ (f a) (f b)) 2.0) (- b a))
  )

; Получение ленивой последовательности сумм интегрированных промежутков
(defn lazySequence [f step]
      (let [area #(trapezoidArea f % (+ % step))
            steps (iterate #(+ % step) 0)
            ]
           (reductions + 0 (map area steps))
           )
  )

; Вычисление интеграла функции f от 0 до x методом трапеции с постоянным шагом step.
; Берем из lazySequence элемент с номером (x div step)
(defn integrate [f step]
      (let [nthSeq (lazySequence f step)]
           (fn [x] (nth nthSeq (quot x step)))
           )
  )

; Тестовые функции
(def line #(+ (* 2 %) 1))

(let [integrateLine (integrate line 1)]
  (prn (time (integrateLine 10000)))
  (prn (time (integrateLine 9999)))
  (prn (time (integrateLine 10001)))
  (prn (time (integrateLine 10002))))

