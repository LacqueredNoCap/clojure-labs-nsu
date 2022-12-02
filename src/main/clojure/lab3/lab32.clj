(ns lab3.lab32)

; Lab 3.2. Реализуйте ленивый параллельный filter, который должен работать, в том числе
; с бесконечными потоками.
; Продемонстрируйте прирост производительности в сравнении с обычным фильтром.

; Разделение коллекции на части длины n
(defn splitCollection [n coll]
  (if (empty? coll)
    ()
    (lazy-seq (cons (take n coll) (splitCollection n (drop n coll))))
    )
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
       (apply concat)
       (apply concat)
      )
  )

; Ленивый параллельный фильтр
(defn lazyParallelFilter
  ([pred coll batchSize] (parallelFilterBatch pred coll batchSize (.availableProcessors (Runtime/getRuntime))))
  ([pred coll batchSize threads] (parallelFilterBatch pred coll batchSize threads))
  )


; Проверка на последовательности элементов (чисел)
(def testSequence (iterate inc 0))

(defn heavyPred [x]
  (Thread/sleep 10)
  (= 0 (mod x 17))
  )

(time (prn (take 20 (filter heavyPred testSequence))))
(time (prn (take 20 (lazyParallelFilter heavyPred testSequence 10 2))))
(time (prn (take 20 (lazyParallelFilter heavyPred testSequence 10))))
(time (prn (take 20 (lazyParallelFilter heavyPred testSequence 10 100))))


; Проверка на коллекции различных элементов
(def collection
  '((1 :A 1 "$3@") (2 "ABC" 5 :A 4) (1) (4 2) (3 ("4" 4) 5 6) () ((3 "4")) (4 5 ()) (3 ()) ((1 "qwe" (:A "55")) 2 1) () ((:A ("11" "22")) 3) (() ()))
  )

(defn heavyPredColl [x]
  (Thread/sleep 100)
  (even? (count x))
  )

(time (prn (filter heavyPredColl collection)))
(time (prn (lazyParallelFilter heavyPredColl collection 2 2)))
(time (prn (lazyParallelFilter heavyPredColl collection 2)))
(time (prn (lazyParallelFilter heavyPredColl collection 2 100)))
