(ns lab4.lab4)

; Lab 4. ДНФ.
; По аналогии с задачей дифференцирования реализовать представление символьных булевых
; выражений с операциями конъюнкции, дизъюнкции, отрицания, импликации. Выражения могут
; включать как булевы константы, так и переменные.
; Реализовать подстановку значения переменной в выражение с его приведением к ДНФ.

; Делаем как на лекции (для дифференцирования) с константой, переменной и суммой.
; Только добавляем аналогично проверку на true, false и логические операции.

; API для переменных
(defn variable [name]
  {:pre [(keyword? name)]}
  (list :var name))

(defn variable? [expr]
  (= (first expr) :var))

(defn variable-name [v]
  (second v))

(defn same-variables? [v1 v2]
  (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1)
       (variable-name v2))))


; API для констант
(def const-true (list :true))

(def const-false (list :false))

(defn const-true? [expr]
  (= (first expr) :true))

(defn const-false? [expr]
  (= (first expr) :false))

(defn const? [expr]
  (or (const-false? expr)
      (const-true? expr)))


; API для отрицания
(defn negation [expr & rest]
  (cons :negation (cons expr rest)))

(defn negation? [expr]
  (= :negation (first expr)))


; API для конъюнкции
(defn conjunction [expr & rest]
  (cons :conjunction (cons expr rest)))

(defn conjunction? [expr]
  (= :conjunction (first expr)))


; API для дизъюнкции
(defn disjunction [expr & rest]
  (cons :disjunction (cons expr rest)))

(defn disjunction? [expr]
  (= :disjunction (first expr)))


; API для импликации
(defn implication [expr & rest]
  (cons :implication (cons expr rest)))

(defn implication? [expr]
  (= :implication (first expr)))


;API для алгебраических операций
(defn args [expr]
  (rest expr))

(defn invert [expr]
  (list :inv expr))


; Реализация функции приведения к ДНФ
(defn to-dnf [expr dnf-rules]
  ((some #(if ((first %) expr) (second %) false) dnf-rules)
   expr))


;--------------------------------------------------------------------------------------------------
; Нулевой шаг алгоритма: обойти выражение и заменить переменную на ее значение.
;--------------------------------------------------------------------------------------------------
(declare define-expr)

(defn define-rules [var val]
  (list
    [(fn [expr] (and (variable? expr) (same-variables? var expr)))
     (fn [expr] val)]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (define-expr var val (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(define-expr var val %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(define-expr var val %) (args expr))))]
    ))

(defn define-expr [var val expr]
  (to-dnf expr (define-rules var val)))


;--------------------------------------------------------------------------------------------------
; Первый шаг алгоритма: заменить все логические операции на конъюнкцию, дизъюнкцию и отрицание.
;--------------------------------------------------------------------------------------------------
(declare step1-expr)

(def step1-rules
  (list
    ; A -> B = !A \/ B
    [(fn [expr] (implication? expr))
     (fn [expr] (step1-expr (disjunction
                              (negation (step1-expr (first (args expr))))
                              (step1-expr (second (args expr))))))]

    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step1-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step1-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step1-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))

(defn step1-expr [expr]
  (to-dnf expr step1-rules))


;--------------------------------------------------------------------------------------------------
; Второй шаг алгоритма: применить все знаки отрицания, чтобы они относились не к выражениям, а к переменным.
;--------------------------------------------------------------------------------------------------
(declare step2-expr)

(def step2-rules
  (list
    ; !(A /\ B) = !A \/ !B
    [(fn [expr] (and (negation? expr) (conjunction? (second expr))))
     (fn [expr] (step2-expr (apply disjunction (map #(negation %) (args (second expr))))))]
    ; !(A \/ B) = !A /\ !B
    [(fn [expr] (and (negation? expr) (disjunction? (second expr))))
     (fn [expr] (step2-expr (apply conjunction (map #(negation %) (args (second expr))))))]

    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step2-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step2-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step2-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))

(defn step2-expr [expr]
  (to-dnf expr step2-rules))


;--------------------------------------------------------------------------------------------------
; Третий шаг алгоритма: убрать двойные отрицания и заменить !true и !false.
;--------------------------------------------------------------------------------------------------
(declare step3-expr)

(def step3-rules
  (list
    ; !!A = A
    [(fn [expr] (and (negation? expr) (negation? (second expr))))
     (fn [expr] (step3-expr (first (args (second expr)))))]
    ; !true = false
    [(fn [expr] (and (negation? expr) (const-true? (first (args expr)))))
     (fn [expr] const-false)]
    ; !false = true
    [(fn [expr] (and (negation? expr) (const-false? (first (args expr)))))
     (fn [expr] const-true)]

    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step3-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step3-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step3-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))

(defn step3-expr [expr]
  (to-dnf expr step3-rules))


;--------------------------------------------------------------------------------------------------
; Четвертый шаг алгоритма: избавиться от внешней конъюнкции, применив свойство дистрибутивности.
;--------------------------------------------------------------------------------------------------
(declare step4-expr)

(def step4-rules
  (list
    ; A /\ (B \/ C) = (A /\ B) \/ (A /\ C)
    [(fn [expr] (and (conjunction? expr) (disjunction? (second (args expr)))))
     (fn [expr] (step4-expr (disjunction
                              (conjunction
                                (first (args expr))
                                (first (args (second (args expr)))))
                              (conjunction
                                (first (args expr))
                                (second (args (second (args expr)))))))
       )]
    ; (A \/ B) /\ C = (A /\ C) \/ (B /\ C)
    [(fn [expr] (and (conjunction? expr) (disjunction? (second expr))))
     (fn [expr] (step4-expr (disjunction
                              (conjunction
                                (first (args (first (args expr))))
                                (second (args expr)))
                              (conjunction
                                (second (args (first (args expr))))
                                (second (args expr))))))]

    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step4-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step4-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step4-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))

(defn step4-expr [expr]
  (to-dnf expr step4-rules))


;--------------------------------------------------------------------------------------------------
; Пятый шаг алгоритма: избавиться от констант или от выражений с константами.
; Например: 1) (:disjunction (:true) (:var :A)) => (:true)
;           2) (:conjunction (:true) (:var :A)) => (:var :A)
;--------------------------------------------------------------------------------------------------
(declare step5-expr)

(def step5-rules
  (list
    [(fn [expr] (and (conjunction? expr) (some const? (args expr))))
     (fn [expr] (if (some const-false? (args expr))
                  const-false
                  (apply conjunction (map #(step5-expr %) (filter #(not (= const-true %)) (args expr))))))]
    [(fn [expr] (and (disjunction? expr) (some const? (args expr))))
     (fn [expr] (if (some const-true? (args expr))
                  const-true
                  (apply disjunction (map #(step5-expr %) (filter #(not (= const-false %)) (args expr))))))]

    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step5-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step5-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step5-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))

(defn step5-expr [expr]
  (to-dnf expr step5-rules))


;--------------------------------------------------------------------------------------------------
; Шестой шаг алгоритма: избавиться от одиночных конъюнкций и дизъюнкций.
;--------------------------------------------------------------------------------------------------
(declare step6-expr)

(def step6-rules
  (list
    ; /\ A = A
    [(fn [expr] (and (conjunction? expr) (= (count (args expr)) 1)))
     (fn [expr] (second expr))]
    ; \/ A = A
    [(fn [expr] (and (disjunction? expr) (= (count (args expr)) 1)))
     (fn [expr] (second expr))]

    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step6-expr (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step6-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step6-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))

(defn step6-expr [expr]
  (to-dnf expr step6-rules))


;--------------------------------------------------------------------------------------------------
(defn execute [var val expr]
  (->> expr
       step1-expr
       step2-expr
       step3-expr
       step4-expr
       (define-expr var val)
       step3-expr
       step5-expr
       step6-expr))

(defn -main []
  (prn (execute (variable :A) const-true (variable :A)))
  (prn (execute (variable :A) const-true (negation (negation (variable :A)))))
  (prn (execute nil nil (negation (negation (variable :A)))))
  (prn (execute (variable :A) const-false (disjunction (variable :A) (variable :B))))
  (prn (execute (variable :A) const-true (conjunction (negation (variable :A)) (variable :B))))
  (prn (execute (variable :A) const-false (disjunction (negation (variable :A)) (variable :B))))
  (prn (execute (variable :A) const-true (negation (disjunction (negation (variable :A)) (variable :B)))))
  (prn (execute (variable :A) const-true (negation (conjunction (negation (variable :A)) (variable :B)))))
  (prn (execute (variable :A) const-false (implication (variable :A) (variable :B))))
  )