(ns lab3.lab32)

; Lab 3.2. Реализуйте ленивый параллельный filter, который должен работать, в том числе
; с бесконечными потоками.
; Продемонстрируйте прирост производительности в сравнении с обычным фильтром.

; Разделение коллекции на части длины n
(defn splitCollection [n coll]
  (map (partial take n) (iterate (partial drop n) coll))
  )

; Фильтрация элементов коллекции по частям определенного размера, каждая в отдельном потоке
(defn parallelFilterBatch [pred coll batchSize threads]
  (->> (splitCollection batchSize coll)
       (splitCollection threads)
       (map (fn [batch]
              (->> (map #(future (doall (filter pred %))) batch)
                   (doall threads)
                   (map deref)
                   )
              )
            )
       (flatten)
      )
  )

; Ленивый параллельный фильтр
(defn lazyParallelFilter
  ([pred coll batchSize] (parallelFilterBatch pred coll batchSize (.availableProcessors (Runtime/getRuntime))))
  ([pred coll batchSize threads] (parallelFilterBatch pred coll batchSize threads))
  )


; Проверка
(defn heavyPred [x]
  (Thread/sleep 10)
  (= 0 (mod x 17))
  )

(time (prn (take 20 (filter heavyPred (iterate inc 0)))))
(time (prn (take 20 (lazyParallelFilter heavyPred (iterate inc 0) 10 2))))
(time (prn (take 20 (lazyParallelFilter heavyPred (iterate inc 0) 10 100))))
(time (prn (take 20 (lazyParallelFilter heavyPred (iterate inc 0) 10))))
