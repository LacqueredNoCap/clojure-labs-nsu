(ns lab1.lab14)

; Lab 1.4. Решите задачу с помощью элементарных операций над последовательностями и функционалов
; map/reduce/filter

; Функция добавляет элемент в начало комбинации и возвращает список
(defn addElement [combination elements]
  (letfn [(fltr [elem] (not= (first combination) elem))]
    (letfn [(addMap [filteredElements] (cons filteredElements combination))]
      (map addMap (filter fltr elements)))
    )
  )

; Функция возвращает список новых комбинаций,
; используя функцию addElement для всех элементов, а не для одного
(defn nextCombination [combinations elements]
  (letfn [(addElementMap [combinations] (addElement combinations elements))]
    (mapcat addElementMap combinations)
    )
  )

; Требуемая к реализации функция
(defn permutation [elements n]
  (if (not= n 0)
    (nth (iterate (fn [combinations] (nextCombination combinations elements)) '(())) n)
    ()
    )
  )


; Проверка permutation
(prn (permutation '() 2))
(prn (permutation '("a") 1))
(prn (permutation '("a" "b" "c") 3))
(prn (permutation '("clj" [1 "d"] :lab1 1/5) 2))

