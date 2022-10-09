(ns lab1.lab13)

; Lab 1.3. Определить функции my-map и my-filter, аналогичные map (для одного списка) и filter,
; выразив их через reduce и базовые операции над списками (cons, first, concat и т.п.)

; Реализация my-map:
(defn my-map [f coll]
  (letfn [(func [acc x] (conj acc (f x)))]
    (reverse (reduce func nil coll))
    )
  )

; Проверка my-map
(prn (map inc '(1 2 3 4 5)))
(prn (my-map inc '(1 2 3 4 5))) ;=> (2 3 4 5 6)

(prn (map #(* 2 %) '(1 2 3 4 5)))
(prn (my-map #(* 2 %) '(1 2 3 4 5))) ;=> (2 4 6 8 10)

(prn (map #(str % "!") '("a" "b" "c")))
(prn (my-map #(str % "!") '("a" "b" "c"))) ;=> (a! b! c!)



; Реализация my-filter
(defn my-filter [f coll]
  (letfn [(func [acc x] (if (f x) (conj acc x) acc))]
    (reverse (reduce func nil coll))
    )
  )

; Проверка my-filter
(prn (filter even? '(1 2 3 4 5 6 7 8)))
(prn (my-filter even? '(1 2 3 4 5 6 7 8))) ;=> (2 4 6 8)

(prn (filter int? '(1 1/4 6.25 "abc" 5 :lab2)))
(prn (my-filter int? '(1 1/4 6.25 "abc" 5 :lab2))) ;=> (1 5)

(prn (filter #(= (mod % 7) 0) '(-14 8 0 28 100 -98)))
(prn (my-filter #(= (mod % 7) 0) '(-14 8 0 28 100 -98))) ;=> (-14 0 28 -98)