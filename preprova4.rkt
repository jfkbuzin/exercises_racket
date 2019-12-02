;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname preprova4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Uma página web é:
;; (a)empty, ou
;; (b)(cons s wp), onde
;; s é um símbolo e
;; wp é uma página web, ou
;; (c)(cons ewp wp), onde ;arvore?
;; ewp e wp são páginas web
(define p1(list 'e 'b 'c 'e 'd))
(define p2(list 'e (list 'e 'b 'c 'e 'd) 'd))
(define p3(list (list 'e 'b (list 'd) 'd) 'd))

;testar trocar um simbolo pelo outro

;;quantos-simbolos : pw simbolo -> numero
;;obj.: recebe uma página web e um símbolo e contabiliza quantas ocorrências deste símbolo existem a partir da página web fornecida
;;exemplo:
;;(quantos-simbolos p1 'e) -> 2

(define (quantos-simbolos pw s)
  (cond
    [(empty? pw) 0] ;pag vzia
    [(symbol? (first pw)) (cond
                             [(symbol=? (first pw) s) (+ 1 (quantos-simbolos (rest pw) s))]
                             [else (quantos-simbolos (rest pw) s)])] ; primeiro e simbolo
    [else (+ (quantos-simbolos (first pw) s)
             (quantos-simbolos (rest pw) s))])) ; primeiro é pagina


;custo: O(n x n)

;;trocar-simbolos : pw simbolo simbolo -> pw
;;obj.: recebe uma página web e dois símbolos e troca quantas ocorrências do primeiro símbolo existem pelo segundo simbolo, a partir da página web fornecida
;;exemplo:
;;(trocar-simbolos p1 'e 'g) -> (list 'g 'b 'c 'g 'd)

(define (trocar-simbolos pw s1 s2)
  (cond
    [(empty? pw) empty] ;pag vzia
    [(symbol? (first pw)) (cond
                             [(symbol=? (first pw) s1) (cons s2 (trocar-simbolos (rest pw) s1 s2))]
                             [else (cons (first pw) (trocar-simbolos (rest pw) s1 s2))])] ; primeiro e simbolo
    [else (cons (trocar-simbolos (first pw) s1 s2)
                  (trocar-simbolos (rest pw) s1 s2))])) ; primeiro é pagina