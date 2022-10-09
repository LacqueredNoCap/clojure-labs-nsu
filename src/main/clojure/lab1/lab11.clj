(ns lab1.lab11)

; Lab 1. Базовые операции над структурами данных.
; Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,
; состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд.
; Например, для символов 'а', 'b', 'c' и n=2 результат должен быть
; ("ab" "ac" "ba" "bc" "ca" "cb") с точностью до перестановки

; Lab 1.1. Решите задачу с помощью элементарных операций над последовательностями и рекурсии.
; Будем составлять комбинации так:
; 1) сначала у нас нет ничего
; 2) составляем комбинацию из одного элемента
; 3) приписываем по одному элементу за шаг и проверяем, чтобы элемент не повторялся
; 4) запоминаем новые комбинации и проделываем шаг (3), пока длина комбинаций не будет равна n

; Функция добавляет элемент в начало комбинации и возвращает список.
(defn addElement [combination elements]
  (if (empty? elements)
    ()
    (if (not= (first combination) (first elements))
      (cons (cons (first elements) combination) (addElement combination (rest elements)))
      (addElement combination (rest elements))
      )
    )
  )

; Функция возвращает список новых комбинаций,
; используя функцию addElement для всех элементов, а не для одного
(defn nextCombination [combinations elements]
  (if (empty? combinations)
    ()
    (concat (addElement (first combinations) elements) (nextCombination (rest combinations) elements))
    )
  )

; Функция нужна вместо цикла, чтобы проделать шаг n раз и довести комбинацию до длины n
(defn looping [combinations elements n]
  (if (= n 0)
    combinations
    (looping (nextCombination combinations elements) elements (dec n))
    )
  )

; Требуемая к реализации функция
(defn permutation [elements n]
  (if (not= n 0)
    (looping '(()) elements n)
    ()
    )
  )

; Проверка permutation
(prn (permutation '() 2))
(prn (permutation '("a") 1))
(prn (permutation '("a" "b" "c") 3))
(prn (permutation '("clj" [1 "d"] :lab1 1/5) 2))
