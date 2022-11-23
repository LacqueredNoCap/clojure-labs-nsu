(ns lab2.lab21)

; Lab 2. Численное интегрирование
; Общее условие:
; Реализовать функцию (оператор), принимающую аргументом функцию f(x) от одной переменной и
; возвращающую функцию F(x) одной переменной, вычисляющую (численно) интеграл функции f(t) от 0 до x.
; Можно использовать метод трапеций с постоянным шагом.
; При оптимизации исходить из того, что полученная первообразная будет использоваться для
; построения графика (т.е. вызываться многократно в разных точках)

; Lab 2.1. Оптимизируйте функцию с помощью мемоизации

; Вычисление площади трапеции
(defn trapezoidArea [f a b]
  (-> (f a)
      (+ (f b))
      (* 0.5)
      (* (- b a))
      )
  )

; Вычисление интеграла функции f от 0 до x методом трапеции с постоянным шагом step
(defn integrate [f step]
  (let [recurIntegrate (fn [recurIntegrate f x]
                 (if (> x 0)
                   (+ (trapezoidArea f (- x step) x) (recurIntegrate recurIntegrate f (- x step)))
                   0)
                 )]
    (partial recurIntegrate recurIntegrate f)
    ))

; Мемоизованная версия функции integrate
(defn memoizeIntegrate [f step]
  (let [recurIntegrate (fn [recurIntegrate x]
                 (if (> x 0)
                   (+ (recurIntegrate recurIntegrate (- x step)) (trapezoidArea f (- x step) x))
                   0)
                 )
        ]
    (partial recurIntegrate (memoize recurIntegrate))
    )
  )


; Проверка

; Тестовые функции
(def line #(+ (* 2 %) 1))

(let [nonMemInt (integrate line 1)
      memInt (memoizeIntegrate line 1)]
  (prn (time (nonMemInt 100)))
  (prn (time (memInt 100)))
  (prn (time (memInt 100)))
  (prn (time (memInt 101)))
  (prn (time (memInt 102)))
  (prn (time (memInt 99))))
