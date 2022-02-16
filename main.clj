(def A [[2 0 1 2 ][3 2 0 3 ][ 0 0 1 2][ 0 0 1 2]] )
(def B [[1 1 0 1 ][0 1 1 1 ][ 1 0 1 0 ][ 1 1 1 0 ]] )
(def C [[2 0 1 1 ][ 3 2 0 0 ][0 0 1 1]] )
(def D [[2 0 ][3 0 ][0 2][1 2]] )
(def I2 [[1 0][0 1]])



(defn nth-row [X i]  (nth X i))
(defn nth-col [X i]  (map #(nth % i) X))
(defn entry [X i j] (nth  (nth-row X i) j) )



;;determinants

(defn det2 [X] (-
	(* (entry X  0 0 ) (entry X  1 1) )
	(* (entry X  0 1 ) (entry X  1 0) )
))



(defn det3 [X] (-(+
	(* (entry X  0 0 ) (entry X  1 1) (entry X  2 2))
 	(* (entry X  0 1 ) (entry X  1 2) (entry X  2 0))
	(* (entry X  0 2 ) (entry X  1 0) (entry X  2 1)))
	
	(+
	(* (entry X  0 2 ) (entry X  1 1) (entry X  2 0))
 	(* (entry X  0 0 ) (entry X  1 2) (entry X  2 1))
	(* (entry X  0 1 ) (entry X  1 0) (entry X  2 2)))
)
)






(defn transpose-aux [X] (for [i (range (count (first X) ))] (nth-col X i))) 
(defn dimension [X]  (list (count X) (count (first X) ) ) )
(defn escalarm-aux [a] #(* a % ))
(defn escalarm [a X ]  ( for [ i (range (count X)) ]  (map  (escalarm-aux a) (nth-row X i) )      ))
(defn prod-ij  [X Y] (reduce + (map * X Y)))
(defn prodm [X  Y] (for [ i (range (count X)) ] (for  [  j  (range (count (first Y) )) ]  (prod-ij   (nth-row X i ) (nth-col Y j ) )  )))
(defn summ [X Y] (map + X Y))
(defn difm [X Y] (map - X Y))

(def AT (transpose-aux A))
(def BT (transpose-aux B))
(def CT (transpose-aux C))
(def DT (transpose-aux D))


(defn unimod  [a b] (if  (> a b)  [[1 0][ (- 0 (quot a b) )  1]]   [[1 (- 0 (quot b a)) ][ 0   1]] ))
(defn euclid [a b] (if  (> a b)  [(mod a b) b]   [a (mod b a)] ))

(unimod  8 3)
(euclid 8 3)

(defn  stepdiof  [[a b]  C  ]   [ (euclid a b )  (prodm C (unimod a b))  ]  )

(stepdiof  [8 3] '((1 0)(0 1)) )
(println (stepdiof  [8 3] I2 ))


(defn diofcalc [[a b]  C]
  (if (= (* a b) 0) [[a b]  C] (recur (euclid a b )  (prodm C (unimod a b)))))


(defn diofsol [a b] (diofcalc [a b] I2 ) )

(println (diofsol 32 37))

(defn mdcsol [a b]
{:x a :y b :mdc (first (filter #(not= % 0) (first (diofcalc [a b] I2 ))))   })


(println "mdc(2134, 3124) ="(:mdc (mdcsol 32 37)))

(println (mod 2022 8))

;;(defn solutionk  [k]   (prodm (second (diofsol 32 37)) [[1][k]] ))

;;(println  (map  (comp first solutionk)  '(1 2 3 4 5)))
;;(println  (map   solutionk  '(1 2 3 4 5)))
;;(defn CLI [v]  (+(* 32 (ffirst v)) (* 37 (first (second v)) ) ) )

;;(println "<debug:>" (first (second '((1) (2 3)) )) )

;;(println (map CLI (map   solutionk  '(1 2 3 4 5))) )