;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista-email) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct e-mail(from data mensagem))

;;Um elemento e-mail de E-mail È uma estrutura
;;(make-e-mail um-from uma-data uma-msg) onde
;;um-from : Simbolo, e o remetente do e-mail
;;uma-data : Numero, e a data do e-mail
;;uma-msg : String, e o corpo do e-mail


;;um lista-email é:
;;1 - ou empty
;;2 - ou (cons e-mail lista-email)
;;onde:
;;e-mail : estrutura e-mail
;;lista-email : lista-email
;;exemplos:
;;(define lista-email1 empty)
;;(define lista-email2(cons(make-e-mail 'filipe 12 "ola") empty))
(define lista-email1(cons(make-e-mail 'filipe 5 "ola")
                            (cons(make-e-mail 'joao 3 "ola")
                            (cons(make-e-mail 'jonathan 27 "ola") empty))))



;;ordena : e-mail lista-email -> lista-email
;;obj: Gera uma lista ordenada (ordem decrescente cnforme a data) a partir de uma lista de emails
;;exemplo: (ordena lista-email1) -> (cons (make-e-mail 'jonathan 27 "ola") (cons (make-e-mail 'filipe 5 "ola") (cons (make-e-mail 'joao 3 "ola") empty)))

(define (ordena lista-email)
  (cond
    [(empty? lista-email) empty]
    [(cons? lista-email) (insere (first lista-email) (ordena (rest lista-email)))]))


;;insere : numero lista-email -> lista-email
;;obj: Dados um número e uma lista ordenada, insere o número na posição correta da lista, de forma que a lista final esteja ordenada

(define (insere e-mail lista-email)
  (cond
    [(empty? lista-email) (cons e-mail empty)]
    [else (cond
            [(>= (e-mail-data e-mail) (e-mail-data (first lista-email))) (cons e-mail lista-email)]
            [(< (e-mail-data e-mail) (e-mail-data (first lista-email))) (cons (first lista-email)
                                     (insere e-mail (rest lista-email)))])
          ]))
