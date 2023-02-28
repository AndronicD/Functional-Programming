#lang racket

(provide (all-defined-out))
(require (lib "trace.ss"))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  (apply + (map * X Y)))


; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
(define (multiply M V)
  (reverse (foldl (lambda (x L) (cons (dot-product x V) L)) '() M)))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.
;(define (get-transformations n)
;  'your-code-here)

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
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).
(define (apply-functional-transformations Fs tuple)
  (foldl (lambda (x L) (x L)) tuple Fs))

;(trace apply-functional-transformations)
;(apply-functional-transformations (list reverse ((curry map) add1) cdr) '(3 4 5))


; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.
(define (FQ1 V) (apply Q1 V))
(define (FQ2 V) (apply Q2 V))
(define (FQ3 V) (apply Q3 V))

(define (FT1 V) (multiply T1 V))
(define (FT2 V) (multiply T2 V))
(define (FT3 V) (multiply T3 V))

(define get-nth-tuple
   (lambda (x)
     (lambda (y)
       (lambda (z)
         (lambda (w)
          (apply-functional-transformations (x (y w) '()) z))))))

(define (from-list-to-Tlist L final)
  (if (null? L)
  (reverse final)    
  (cond
    ((= (car L) 1) (from-list-to-Tlist (cdr L) (cons FT1 final)))
    ((= (car L) 2) (from-list-to-Tlist (cdr L) (cons FT2 final)))
    ((= (car L) 3) (from-list-to-Tlist (cdr L) (cons FT3 final))))))

(define (from-list-to-Qlist L final)
  (if (null? L)
  (reverse final)    
  (cond
    ((= (car L) 1) (from-list-to-Qlist (cdr L) (cons FQ1 final)))
    ((= (car L) 2) (from-list-to-Qlist (cdr L) (cons FQ2 final)))
    ((= (car L) 3) (from-list-to-Qlist (cdr L) (cons FQ3 final))))))

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.
(define get-nth-ppt-from-matrix-transformations
   (curry (((get-nth-tuple from-list-to-Tlist) get-transformations) '(3 4 5))))

;(trace get-nth-ppt-from-matrix-transformations)
;(get-nth-ppt-from-matrix-transformations 64)
;(trace get-transformations)
;(get-transformations 64)

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define get-nth-quadruple
  (curry (((get-nth-tuple from-list-to-Qlist)get-transformations) '(1 1 2 3))))
;(trace get-nth-quadruple)
;(get-nth-quadruple 64)


; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
(define (from-Q-to-T L)
  (list (* (car L) (cadddr L)) (* 2 (caddr L) (cadr L)) (+ (power (caddr L) 2) (power (cadr L) 2))))

(define get-nth-ppt-from-GH-quadruples
  (lambda (x) (from-Q-to-T (get-nth-quadruple x))))
;(trace get-nth-ppt-from-GH-quadruples)
;(get-nth-ppt-from-GH-quadruples 64)