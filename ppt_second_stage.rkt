#lang racket

(provide (all-defined-out))

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
  (map ((curry dot-product) V) M))

; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.

; functie care calculeaza nivelul pe care se afla numarul
(define (get-level n i)
  (if (>= (/ (- (expt 3 (add1 i)) 1) 2) n) ;daca n <= (3^(i+1)-1)/(3-1)=suma
      (add1 i)
      (get-level n (add1 i))))

; functie care determina indexul minim de pe nivelul
; in care se afla numarul
(define (get-min-index n)
  (add1 (/ (sub1 (expt 3 (sub1 (get-level n 0)))) 2)))

; functie care determina indexul maxim de pe nivelul
; in care se afla numarul
(define (get-max-index n)
  (/ (sub1 (expt 3(get-level n 0))) 2))

; functie care determina pozitia numarului
; pe nivelul in care se afla
(define (get-position n)
  (add1 (- n (get-min-index n)))) ; scad din n indexul minim si adaug 1

; am folosit recursivitatea pe coada si am inversat la sfarsit rezultatul
(define (get-transformations-helper n elements pos result)
  (cond
    [(= n 1) null]
    ; daca numarul de elemente este egal cu 1 functia va returna rezultatul inversat
    [(= elements 1) (reverse result)]
    ; daca pozitia numarului este mai mica sau egala cu prima treime a numarului total de
    ; elemente de pe nivel atunci impart numarul de elemente la 3, pozitia ramane aceeasi, si adaug 1 in result
    [(<= pos (/ elements 3)) (get-transformations-helper n (/ elements 3) pos (cons 1 result))]
    ; daca pozitia numarului este mai mica sau egala cu a doua treime a numarului total de elemente
    ; de pe nivel atunci impart numarul de elemente la 3, pozitia acum devine pozitia initiala minus
    ; (numarul de elemente / 3) si adaug 2 in result
    [(<= pos (* (/ elements 3) 2)) (get-transformations-helper n (/ elements 3) (- pos (/ elements 3)) (cons 2 result))]
    ; altfel impart numarul de elemente la 3, pozitia acum devine pozitia initiala minus de doua ori
    ; (numarul de elemente / 3) si adaug 3 in result
    [else (get-transformations-helper n (/ elements 3) (- pos (* 2 (/ elements 3))) (cons 3 result))]))

; elements = indexul maxim - indexul minim de pe nivel + 1 (numarul de elemente)
; pos = get-position
(define (get-transformations n)
  (get-transformations-helper n (add1 (- (get-max-index n) (get-min-index n))) (get-position n) null))

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
  (foldr (λ (f x) (f x)) tuple (reverse Fs)))


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

; functie care formeaza lista de functii
(define (get-nth-tuple-helper n f1 f2 f3)
  (map (λ (x) (cond
           [(= x 1) f1]
           [(= x 2) f2]
           [else f3])) (get-transformations n)))

(define (get-nth-tuple n Fs tuple)
  (apply-functional-transformations (get-nth-tuple-helper n (car Fs) (cadr Fs) (caddr Fs)) tuple))


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.
(define (get-nth-ppt-from-matrix-transformations n)
  (get-nth-tuple n (list ((curry multiply) T1)
                         ((curry multiply) T2)
                         ((curry multiply) T3)) '(3 4 5)))


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define (get-nth-quadruple n)
  (get-nth-tuple n (list ((curry apply) Q1)
                         ((curry apply) Q2)
                         ((curry apply) Q3)) '(1 1 2 3)))

; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
(define (get-nth-ppt-from-GH-quadruples n)
  (list (* (car (get-nth-quadruple n)) (cadddr (get-nth-quadruple n)))
        (* 2 (cadr (get-nth-quadruple n)) (caddr (get-nth-quadruple n)))
        (+ (* (cadr (get-nth-quadruple n)) (cadr (get-nth-quadruple n))) (* (caddr (get-nth-quadruple n)) (caddr (get-nth-quadruple n))))))
