;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listas) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;um lista-simbolos é:
;;1 - ou empty
;;2 - ou (cons simbolo lista-simbolos)
;;onde:
;;simbolo : simbolo
;;lista-simbolos : lista-simbolos

;;contem-simbolo : lista-simbolos simbolo -> boleano
;;obj:
;;exemplos:
;;(contem-simbolo empty 'a) -> 'false
;;(contem-simbolo (cons 'b empty) 'a) -> 'false
;;(contem-simbolo (cons 'b(cons 'c(cons 'a empty))) 'a) -> 'true

(define(contem-simbolo lista-simbolos simbolo)
  (cond
    [(empty? lista-simbolos) 'false]
    [(symbol=? (first lista-simbolos) simbolo) 'true]
    [else (contem-simbolo(rest lista-simbolos) simbolo)]))

;;quantos : lista-simbolos -> numero
;;obj:
;;exemplos:
;;(quantos empty) -> 0
;;(quantos (cons 'b empty)) -> 1

(define(quantos lista-simbolos)
  (cond
    [(empty? lista-simbolos) 0]
    [else (+ 1(quantos(rest lista-simbolos)))]))

;;1

;;total : lista-valores -> numero
;;obj: determina o valor total dos objetos em estoque em uma loja
;;exemplos:
;;(total empty) -> 0
;;(total (cons 65 empty)) -> 65

(define (total lista-valores)
  (cond
    [(empty? lista-valores) 0]
    [else(+ (first lista-valores)(total(rest lista-valores)))]))

;;2

;;um-preco : lista-valores numero -> booleano
;;obj: checa se todos os precos estao abaixo de um valor indicado
;;exemplos:
;;(um-preco empty 1) -> 'true
;;(um-preco (cons 65 empty) 1) -> 'false
;;(um-preco (cons 0.5 empty) 1) -> 'true

(define(um-preco lista-valores numero)
  (cond
    [(empty? lista-valores) 'true]
    [(>= (first lista-valores) numero) 'false]
    [else (um-preco (rest lista-valores) numero)]))

;;3

;;converte : lista-valores numero -> numero
;;obj: converte uma lista de digitos em um numero decimal
;;exemplos:
;;(converte empty 0) -> 0
;;(converte (cons 7(cons 5 empty))0) -> 57

(define (converte-real lista-valores)
  (cond
    [(empty? lista-valores) "lista vazia"]
    [else (converte lista-valores 0)]))
    
(define (converte lista-valores numero)
  (cond
    [(empty? lista-valores) 0]
    [else (+ (* (first lista-valores)(expt 10 numero))(converte (rest lista-valores)(+ numero 1)))]))

