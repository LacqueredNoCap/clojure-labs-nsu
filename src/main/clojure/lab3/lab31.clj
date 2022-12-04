(ns lab3.lab31)

; Lab3. Параллельная обработка последовательностей

; 3.1. Реализуйте параллельный вариант filter (необязательно ленивый) с помощью future.
; Параллельная обработка должна производиться блоками по заданному числу элементов.
; Размер блоков следует вычислять вручную, без использования готовых функций,
; таких как partition (для разделения последовательности следует использовать take и drop).
; Продемонстрируйте прирост производительности в сравнении с обычным фильтром.

; Разделение коллекции на части длины n
(defn splitCollection [coll n]
  (if (empty? coll)
    ()
    (concat (list (take n coll)) (splitCollection (drop n coll) n))
    )
  )

; Фильтрация элементов коллекции по частям определенного размера, каждая в отдельном потоке
(defn parallelFilterBatch [pred coll batchSize]
  (->> (splitCollection coll batchSize)
       (map #(future (doall (filter pred %))))
       (doall)
       (map deref)
       (mapcat #(concat %))
       )
  )

; Фильтрация элементов коллекции, разделенной на части по количеству потоков
(defn parallelFilterThreads [pred coll threads]
  (let [size (count coll)]
    (if (== (mod size threads) 0)
      (parallelFilterBatch pred coll (quot size threads))
      (parallelFilterBatch pred coll (+ 1 (quot size threads)))
      )
    )
  )

; Параллельный фильтр
(defn parallelFilter
  ([pred coll] (parallelFilterThreads pred coll (.availableProcessors (Runtime/getRuntime))))
  ([pred coll threads] (parallelFilterThreads pred coll threads))
  )

; Проверка на последовательности элементов (чисел)
(def testSequence (range 500))

(defn heavyPred [x]
  (Thread/sleep 10)
  (= 0 (mod x 17))
  )

(time (prn (filter heavyPred testSequence)))
(time (prn (parallelFilter heavyPred testSequence 2)))
(time (prn (parallelFilter heavyPred testSequence)))
(time (prn (parallelFilter heavyPred testSequence 100)))


; Проверка на коллекции различных элементов
(def collection
  '((1 :A 1 "$3@") (2 "ABC" 5 :A 4) (1) (4 2) (3 ("4" 4) 5 6) () ((3 "4")) (4 5 ()) (3 ()) ((1 "qwe" (:A "55")) 2 1) () ((:A ("11" "22")) 3) (() ()))
  )

(defn heavyPredColl [x]
  (Thread/sleep 100)
  (even? (count x))
  )

(time (prn (filter heavyPredColl collection)))
(time (prn (parallelFilter heavyPredColl collection 2)))
(time (prn (parallelFilter heavyPredColl collection)))
(time (prn (parallelFilter heavyPredColl collection 100)))
