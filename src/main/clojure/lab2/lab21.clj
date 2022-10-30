(ns lab2.lab21)

; Lab 2. Численное интегрирование
; Общее условие:
; Реализовать функцию (оператор), принимающую аргументом функцию f(x) от одной переменной и
; возвращающую функцию F(x) одной переменной, вычисляющую (численно) интеграл функции f(t) от 0 до x.
; Можно использовать метод трапеций с постоянным шагом.
; При оптимизации исходить из того, что полученная первообразная будет использоваться для
; построения графика (т.е. вызываться многократно в разных точках)

; Lab 2.1. Оптимизируйте функцию с помощью мемоизации

; Формулу численного интегрирования от a до b функции f(x) методом трапеций при постоянном шаге dx
; можно записать как ~ dx/2 * ( f(x0) + 2f(x1) + ... + 2f(xn-1) + f(xn) ), при x0 = a, xn = b

; Длина шага
(def step 1)

; Вычисление члена суммы
(defn fxn [f x n]
  (if (or (= n 0) (= (* step n) x))
    (f (* n step))
    (* 2 (f (* n step)))
    )
  )

; Мемоизованная версия функции fxn
(def memoizeFxn (memoize fxn))

; Вычисление интеграла функции f от 0 до x методом трапеции с постоянным шагом step
(defn integrate [f]
  (fn [x]
    (let [sum (->> (iterate #(+ % step) 0.0)
                 (take-while #(<= % x))
                 (map #(fxn f x %))
                 (reduce +)
                   )
        ]
      (#(* % (/ step 2.0)) sum)
      )
    )
  )

; Мемоизованная версия функции integrate
(def memoizeIntegrate (memoize integrate))

; Проверка

; Тестовые функции
(def line #(+ (* 2 %) 1))
(def cube #(* % % %))
(def shiftedHyperbola #(/ 1 (+ % 1)))

(prn (time ((integrate line) 10))) ; = 110
(prn (time ((memoizeIntegrate line) 10))) ; = 110
(prn (time ((memoizeIntegrate line) 10))) ; = 110

(prn (time ((integrate cube) 6))) ; ~ 324
(prn (time ((memoizeIntegrate cube) 6))) ; ~ 324
(prn (time ((memoizeIntegrate cube) 6))) ; ~ 324

(prn (time ((integrate shiftedHyperbola) (Math/pow Math/E 5)))) ; ~ 5
(prn (time ((memoizeIntegrate shiftedHyperbola) (Math/pow Math/E 5)))) ; ~ 5
(prn (time ((memoizeIntegrate shiftedHyperbola) (Math/pow Math/E 5)))) ; ~ 5