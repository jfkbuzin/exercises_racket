;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname trabteste1) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")))))
;(start 600 600)
;(draw-solid-disk (make-posn 100 100) 5 'Blue)
;(draw-solid-disk (make-posn 130 110) 5 'Red)
;(draw-solid-rect (make-posn 0 200) 150 50 'Black)

;(draw-solid-rect (make-posn 200 200) 10 15 'Green)
;(draw-solid-rect (make-posn 250 250) 15 15 'Yellow)
;(draw-solid-line (make-posn 300 300 )(make-posn 300 150)'Black)

;; res-cor : simbolo -> simbolo

(define-struct disco(posn1 posn2 tamobj resist x y))
;
;posn1: numero, coordenadas do objeto
;posn2: numero, coordenadas do objeto
;tamobj: numero, tamanho do objeto
;resist: simbolo, pode ser 'forte ou 'fraco
;x: numero, variação de x
;y: numero, variação de y


(define-struct canhao(posn))
; 
;posn: estrutura, coordenadas do objeto

(define-struct obj(tipo posn1 posn2 larg alt resist x y))
;
;tipo: simbolo, tipo de objeto 'disco ou 'retangulo
;posn1: numero, coordenadas do objeto
;posn2: numero, coordenadas do objeto
;larg: numero, largura do objeto
;alt: numero, altura do objeto, sempre zero se não for retangulo
;resist: simbolo, pode ser 'forte ou 'fraco
;x: numero, variação de x
;y: numero, variação de y

(define tmp 0.10)

(define (gera-tela)
 (and(start 600 600)
 (cria-canhao 300)
 (cria-obj obj1)
 (cria-obj obj2)
 (cria-obj obj3)
 (cria-obj obj4)
 (cria-obj obj5)
 (move-ate-fora bolha1 obj1 obj2 obj3 obj4 obj5)
 (move-ate-fora bolha2 obj1 obj2 obj3 obj4 obj5)
 (move-ate-fora bolha3 obj1 obj2 obj3 obj4 obj5)))


(define(cria-canhao num)
  (draw-solid-rect (make-posn 0 num) 150 50 'Black))

(define(cria-obj obj)
  (cond
    [(symbol=? (obj-tipo obj) 'disco)(draw-solid-disk (make-posn(obj-posn1 obj)(obj-posn2 obj)) (obj-larg obj) 
                   (cond
                    [(symbol=? 'forte (obj-resist obj) ) 'Green]
                    [else 'Yellow]))]
    [else(draw-solid-rect (make-posn(obj-posn1 obj)(obj-posn2 obj)) (obj-larg obj)(obj-alt obj) 
                   (cond
                    [(symbol=? 'forte (obj-resist obj) ) 'Green]
                    [else 'Yellow]))])) 

(define bolha1(make-disco 155 325 5 'forte 5 1)) ;posn1 = 150, posn2 = 325 - boca do canhao
(define bolha2(make-disco 160 325 10 'fraco 15 5))
(define bolha3(make-disco 160 325 10 'fraco 5 5))
(define obj1(make-obj 'disco 400 400 25 0 'forte 10 5))
(define obj2(make-obj 'retangulo 350 350 25 15 'fraco 5 10))
(define obj3(make-obj 'retangulo 300 300 10 10 'forte 5 10))
(define obj4(make-obj 'disco 450 450 5 0 'fraco 5 10))
(define obj5(make-obj 'disco 500 500 10 0 'forte 5 10))

;;move-até-fora: Bolha -> Booleano
;;obj.: Função para criar e mover uma bolha até estourar no canto da tela ou ao colidir com um objeto(true)
;;exemplo:
;;(move-ate-fora bolha1 obj1 obj2)

(define (move-ate-fora bolha obj1 obj2 obj3 obj4 obj5)
  (cond
    [(fora-dos-limites? bolha) true]
    [(fim-bolha obj1 bolha) true]
    [(fim-bolha obj2 bolha) true]
    [(fim-bolha obj3 bolha) true]
    [(fim-bolha obj4 bolha) true]
    [(fim-bolha obj5 bolha) true]
    [else (and (cria-del-bolha bolha)
               (move-ate-fora (move-bolha bolha) obj1 obj2))]))

;;funções auxiliares de move-até-fora

(define(cria-del-bolha bolha)
  (and
  (draw-solid-disk (make-posn(disco-posn1 bolha)(disco-posn2 bolha)) (disco-tamobj bolha) 
                   (cond
                    [(symbol=? 'forte (disco-resist bolha) ) 'Red]
                    [else 'Blue]))
  (sleep-for-a-while tmp)
  (clear-solid-disk (make-posn(disco-posn1 bolha)(disco-posn2 bolha)) (disco-tamobj bolha) 
                   (cond
                    [(symbol=? 'forte (disco-resist bolha) ) 'Red]
                    [else 'Blue]))))  

(define (move-bolha bolha)
  (make-disco (+ (disco-posn1 bolha)
                (disco-x bolha))
             (+ (disco-posn2 bolha)
                (disco-y bolha))
             (disco-tamobj bolha)
             (disco-resist bolha)
             (disco-x bolha)
             (disco-y bolha)))

(define (fora-dos-limites? bolha)
  (not(and
   (<= 0 (disco-posn1 bolha) 600)
   (<= 0 (disco-posn2 bolha) 600))))

(define (fim-bolha obj bolha)
  (or
   (and (symbol=? 'fraco (disco-resist bolha))
       (= (obj-posn1 obj) (disco-posn1 bolha))
       (= (obj-posn2 obj) (disco-posn2 bolha))
       )
   (and (symbol=? 'forte (disco-resist bolha))
       (symbol=? 'forte (obj-resist obj))
       (= (obj-posn1 obj) (disco-posn1 bolha))
       (= (obj-posn2 obj) (disco-posn2 bolha))
       )))


;;objetos:

;(define(cria-del-obj obj)
 ; (cond
  ;  [(symbol=? (obj-tipo obj) 'disco)
 ; (and
 ; (draw-solid-disk (make-posn(obj-posn1 obj)(obj-posn2 obj)) (obj-larg obj) 
 ;                  (cond
 ;                   [(symbol=? 'forte (obj-resist obj) ) 'Green]
 ;                   [else 'Yellow]))
 ; (sleep-for-a-while tmp)
 ; (clear-solid-disk (make-posn(obj-posn1 obj)(obj-posn2 obj)) (obj-larg obj) 
 ;                  (cond
 ;                   [(symbol=? 'forte (obj-resist obj) ) 'Green]
 ;                   [else 'Yellow])))]
    
  ;  [else (and
  ;(draw-solid-rect (make-posn(obj-posn1 obj)(obj-posn2 obj)) (obj-larg obj)(obj-alt obj) 
  ;                 (cond
  ;                  [(symbol=? 'forte (obj-resist obj) ) 'Green]
  ;                  [else 'Yellow]))
  ;(sleep-for-a-while tmp)
  ;(clear-solid-rect (make-posn(obj-posn1 obj)(obj-posn2 obj)) (obj-larg obj)(obj-alt obj)
   ;                (cond
    ;                [(symbol=? 'forte (obj-resist obj) ) 'Green]
     ;               [else 'Yellow])))])) 

;; move-bolha
;(define (move-obj obj)
 ; (make-obj  (obj-tipo obj)
  ;           (+ (obj-posn1 obj)
   ;             (obj-x obj))
    ;         (+ (obj-posn2 obj)
     ;           (obj-y obj))
      ;       (obj-larg obj)
       ;      (obj-alt obj)
        ;     (obj-resist obj)
         ;    (obj-x obj)
          ;   (obj-y obj)))

  

;;move-e-volta: obj -> Booleano
;(define (move-e-volta obj)
;  (cond
;    [(not(and
;   (<= 0 (obj-posn1 obj) 600)
;   (<= 0 (obj-posn2 obj) 600))) (make-obj  (obj-tipo obj)
           ;                    300
          ;                     300
         ;                      (obj-larg obj)
        ;                       (obj-alt obj)
      ;                         (obj-resist obj)
       ;                        (obj-x obj)
     ;                          (obj-y obj))]
    ;[else (and (cria-del-obj obj)
      ;         (move-e-volta (move-obj obj)))]))



