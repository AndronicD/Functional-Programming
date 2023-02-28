#lang racket
;(require (lib "trace.ss"))

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (if (null? X)
      0
      (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))))

; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply M V)
  (multiply-tail M V '()))

(define (multiply-tail M V ACC)
  (if (null? M)
      (reverse ACC)
      (multiply-tail (cdr M) V (cons (dot-product (car M) V) ACC))))

; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)

(define (power base exp)
  (power-tail base exp 1))

(define (power-tail base exp acc)
  (if (= exp 0)
      acc
      (power-tail base (- exp 1) (* acc base))))

(define (sum-of-3-power limit)
  (sum-of-3-power-tail limit 0))

(define (sum-of-3-power-tail limit acc)
  (if (= limit 0)
      (+ acc 1)
      (sum-of-3-power-tail (- limit 1) (+ acc (power 3 limit)))))

(define (get-level n)
  (if (= n 1)
      0
      (get-level-tail n 0)))

(define (get-level-tail n acc)
  (if (and (> n (sum-of-3-power acc)) (<= n (sum-of-3-power (+ acc 1))))
      (+ acc 1)
      (get-level-tail n (+ acc 1))))

(define (get-transformations n)
  (if (= n 1)
      '()
  (get-transformations-tail n (get-level n) (sum-of-3-power (- (get-level n) 1)) (sum-of-3-power (get-level n)) '())))

(define (get-transformations-tail n level inf sup acc)
  (if (= level 0)
   (reverse acc)
  (cond
    ((and (> n inf) (<= n (+ (power 3 (- level 1)) inf))) (get-transformations-tail n (- level 1) inf (+ (power 3 (- level 1)) inf) (cons 1 acc)))
    ((and (> n (+ (power 3 (- level 1)) inf)) (<= n (+ (* (power 3 (- level 1)) 2) inf))) (get-transformations-tail n (- level 1) (+ (power 3 (- level 1)) inf) (+ (* (power 3 (- level 1)) 2) inf) (cons 2 acc)))
    ((and (> n (+ (* (power 3 (- level 1)) 2) inf)) (<= n sup)) (get-transformations-tail n (- level 1) (+ (* (power 3 (- level 1)) 2) inf) sup (cons 3 acc))))))

; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations Ts ppt)
  (if (null? Ts)
   ppt    
  (cond
    ((= (car Ts) 1) (apply-matrix-transformations (cdr Ts) (multiply T1 ppt)))
    ((= (car Ts) 2) (apply-matrix-transformations (cdr Ts) (multiply T2 ppt)))
    ((= (car Ts) 3) (apply-matrix-transformations (cdr Ts) (multiply T3 ppt))))))

; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))
