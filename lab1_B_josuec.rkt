;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab1_B_josuec) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;;robo: numero string numero -> numero
;;obj;; descobrir a posicao de um robo apos se mover(de 1 a 50)
;;exemplo (posicao 1 "esquerda" 23) -> 24
;;        (posicao 1 "esquerda" 55) -> 50
;;        (posicao 42 "direita" 23) -> 19
;;        (posicao 23 "direita" 28) -> 1

(define (posicao atual sentido num_pos)
  (cond
    [(and (>= atual 1)(<= atual 50)(string=? sentido "esquerda")(<= (+ atual num_pos) 50)) (+ atual num_pos)]
    [(and (>= atual 1)(<= atual 50)(string=? sentido "direita")(>= (- atual num_pos) 1)) (- atual num_pos)]
    [(and (>= atual 1)(<= atual 50)(string=? sentido "esquerda")(> (+ atual num_pos) 50)) 50]
    [(and (>= atual 1)(<= atual 50)(string=? sentido "direita")(< (- atual num_pos) 1)) 1]))

