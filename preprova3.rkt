;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname preprova3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;(ordena (list 2 5 3 6 1 4))

;(define (selection-sort xs)
 ; (cond [(empty? xs) '()]
  ;      [else (define x0 (apply min xs))
   ;           (cons x0 (selection-sort (remove x0 xs)))]))

(define (ordena lista)
  (cond
    [(empty? lista) empty]
    [(cons? lista) (insere (first lista) (ordena (rest lista)))]))

(define (insere num lista)
  (cond
    [(empty? lista) (cons num empty)]
    [else (cond
            [(>= num (first lista)) (cons num lista)]
            [(< num (first lista)) (cons (first lista)
                                     (insere num (rest lista)))])
          ]))


;; resolução com quicksort - alto nivel

; maiores: Lista-de-numeros Numero -> Lista-de-numeros
; Cria lista com todos os numeros da lista de entrada
; estritamente maiores do que o numero recebido
; Exemplo:
; (maiores (list 11 8 14 7) 11) => (list 14)

; menores: Lista-de-numeros Numero -> Lista-de-numeros
; Cria lista com todos os numeros da lista de entrada
; estritamente menores do que o numero recebido
; Exemplo:
; (menores (list 11 8 14 7) 11) => (list 8 7)


; quick-sort: Lista-de-numeros -> Lista-de-numeros
; Obj.: Cria uma lista com os mesmos numeros da lista de entrada, mas ordenados em ordem decrescente.
; Exemplo:
; (quick-sort (list 2 5 3 6 1 4))

(define(quick-sort ldn)
  (local(
         (define(maiores ldn n)
           (cond
             [(empty? ldn)empty]
             [else(cond
                    [(> (first ldn) n) (cons(first ldn)(maiores (rest ldn) n))]
                    [else(maiores (rest ldn) n)])]))
         
         (define(menores ldn n)
           (cond
             [(empty? ldn)empty]
             [else
              (cond
                [(< (first ldn) n) (cons(first ldn)(menores (rest ldn) n))]
                [else(menores (rest ldn) n)])]))
         )
  (cond
    [(empty? ldn)empty]
    [else(append
          (quick-sort (maiores ldn (first ldn)))
          (list (first ldn))
          (quick-sort (menores ldn (first ldn))))])))

;custo(quicksort) : log(n)