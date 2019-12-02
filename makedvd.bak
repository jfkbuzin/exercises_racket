;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname makedvd) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct dvd(titulo autor produtor duracao ano preco))

;;muda-preco : dvd -> dvd
;;obj: mudar o preco de um dvd conforme a data de lancamento
;;exemplos:
;;(define dvd1(make-dvd "Blue Velvet" "Unknown" "David Lynch" 120 1986 45))
;;(define dvd2(make-dvd "Crash" "Unknown" "David Cronenberg" 90 1996 25))
;;(define dvd3(make-dvd "Black Swan" "Unknown" "Darren Aronofsky" 140 2011 60))
;;(muda-preco dvd1)-> (make-dvd "Blue Velvet" "Unknown" "David Lynch" 120 1986 40.50)
;;(muda-preco dvd2)-> (make-dvd "Crash" "Unknown" "David Cronenberg" 90 1996 23.75)
;;(muda-preco dvd3)-> (make-dvd "Black Swan" "Unknown" "Darren Aronofsky" 140 2011 61.20)

(define(muda-preco um-dvd)
  (cond
    [(<(dvd-ano um-dvd)1990)(make-dvd (dvd-titulo dvd-autor dvd-produtor dvd-duracao dvd-ano (* 0.90(dvd-preco um-dvd))))]
    [(<=(dvd-ano um-dvd)2010)(make-dvd (dvd-titulo dvd-autor dvd-produtor dvd-duracao dvd-ano (* 0.95(dvd-preco um-dvd))))]
    [(=(dvd-ano um-dvd)2011)(make-dvd (dvd-titulo dvd-autor dvd-produtor dvd-duracao dvd-ano (* 1.02(dvd-preco um-dvd))))]
    [else um-dvd]))