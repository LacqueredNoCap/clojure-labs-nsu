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
       (flatten)
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

; Проверка
(defn heavyPred [x]
  (Thread/sleep 10)
  (= 0 (mod x 17))
  )

(time (prn (filter heavyPred (range 500))))
(time (prn (parallelFilter heavyPred (range 500) 2)))
(time (prn (parallelFilter heavyPred (range 500))))
(time (prn (parallelFilter heavyPred (range 500) 100)))
