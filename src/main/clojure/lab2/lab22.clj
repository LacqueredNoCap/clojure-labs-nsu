(ns lab2.lab22)

; 2.2. Оптимизируйте функцию с помощью бесконечной последовательности частичных решений

; Длина шага
(def step 0.5)

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
(defn integrate [f]
      (let [nthSeq (lazySequence f step)]
           (fn [x] (nth nthSeq (quot x step)))
           )
  )

; Тестовые функции
(def line #(+ (* 2 %) 1))
(def cube #(* % % %))
(def shiftedHyperbola #(/ 1 (+ % 1)))

(prn (time ((integrate line) 10))) ; = 110
(prn (time ((integrate line) 12))) ; = 156

(prn (time ((integrate cube) 4))) ; ~ 64
(prn (time ((integrate cube) 6))) ; ~ 324

(prn (time ((integrate shiftedHyperbola) (Math/pow Math/E 5)))) ; ~ 5
(prn (time ((integrate shiftedHyperbola) (Math/pow Math/E 6)))) ; ~ 6
