(ns lab1.lab12)

; Lab 1.2. Перепишите программу Lab 1.1 так, чтобы все рекурсивные вызовы были хвостовыми

; Функция добавляет элемент в начало комбинации и возвращает список.
(defn addElement [combination elements]
  (loop [combinationLoop combination elementsLoop elements combinationsLoop '()]
    (if (empty? elementsLoop)
      combinationsLoop
      (if (not= (first combination) (first elementsLoop))
        (recur combinationLoop (rest elementsLoop) (cons (cons (first elementsLoop) combination) combinationsLoop))
        (recur combinationLoop (rest elementsLoop) combinationsLoop)
        )
      )
    )
  )

; Функция возвращает список новых комбинаций,
; используя функцию addElement для всех элементов, а не для одного
(defn nextCombination [combinations elements]
  (loop [combinationsLoop combinations newCombinationsLoop '()]
    (if (empty? combinationsLoop)
      newCombinationsLoop
      (recur (rest combinationsLoop) (concat newCombinationsLoop (addElement (first combinationsLoop) elements)))
      )
    )
  )

; Функция нужна вместо цикла, чтобы проделать шаг n раз и довести комбинацию до длины n
(defn looping [combinations elements n]
  (if (= n 1)
    (nextCombination combinations elements)
    (recur (nextCombination combinations elements) elements (dec n))
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
