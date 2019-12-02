;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab1_B_josue) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Questão 1:

(+ 5 (/ 4 2)) ;; tinha divisão por zero

(sqr 4) ;; um argumento a mais que foi retirado para chegar a 4 ao quadrado = 16

(define (f y) ;; variavel x não definida, foi trocada por y
  (+ y 10))

(define (g x) ;; faltou um ) no final, variavel 'a com erro, trocada por um numero
  (- x 22))

(define (h x) ;; h(x) está errado, o correto é (h x)
  (+ x 10))

(define (j k) ;; "v" está errado, o correto é 'v
  (symbol=? k 'v))

;; Questão 2:
;; a)

;;medias: numero numero numero string -> numero
;;obj;; calcular a média aritmetrica, gemoetrica ou harmonica de 3 numeros
;;exemplo (media 2 4 6 "aritmetica") -> 4
;;        (media 2 4 8 "geometrica") -> 4
;;        (media 2 4 8 "harmonica")  -> 3.43

(define (media one two three string)
  (cond
    [(string=? string "aritmetica") (/ (+ one two three) 3)] ;; (check-expect (media 2 4 6 "aritmetica") 4) -> The test passed!
    [(string=? string "geometrica") (expt (* one two three) (/ 1 3))] ;; (check-expect (media 2 4 8 "geometrica") 4) -> first argument of equality cannot be an inexact number, given #i3.9999999999999996
    [(string=? string "harmonica") (/ 3 (+ (/ 1 one) (/ 1 two) (/ 1 three)))])) ;; (check-expect (media 2 4 8 "geometrica") 4) -> erro por ser numero com virgula




