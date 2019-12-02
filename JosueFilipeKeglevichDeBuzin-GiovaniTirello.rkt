;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname JosueFilipeKeglevichDeBuzin-GiovaniTirello) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")))))
(define-struct disco(posn1 posn2 tamobj resist x y))

;; um elemento novo do conjunto disco é:
;; (make-disco posn1 posn2 tamobj resist x y)
;; onde:
;; posn1: numero, coordenadas do objeto
;; posn2: numero, coordenadas do objeto
;; tamobj: numero, tamanho do objeto
;; resist: simbolo, pode ser 'forte(vermelho) ou 'fraco(azul)
;; x: numero, variação de x
;; y: numero, variação de y


(define-struct canhao(posn))

;; um elemento novo do conjunto canhao é:
;; (make-canhao posn)
;; onde:
;; posn: estrutura, coordenadas do objeto


(define-struct obj(tipo posn1 posn2 larg alt resist x y))

;; um elemento novo do conjunto obj é:
;; (make-obj tipo posn1 posn2 larg alt resist x y)
;; onde:
;; tipo: simbolo, tipo de objeto 'disco ou 'retangulo
;; posn1: numero, coordenadas do objeto
;; posn2: numero, coordenadas do objeto
;; larg: numero, largura do objeto
;; alt: numero, altura do objeto, sempre zero se não for retangulo
;; resist: simbolo, pode ser 'forte(verde) ou 'fraco(amarelo)
;; x: numero, variação de x
;; y: numero, variação de y


;;Definicão do tempo de espera e dos objetos

(define tmp 0.10)
(define bolha1(make-disco 155 325 5 'forte 5 1)) ;posn1 = 150, posn2 = 325 - boca do canhao
(define bolha2(make-disco 160 325 10 'fraco 15 5))
(define bolha3(make-disco 160 325 10 'fraco 5 5))
(define obj1(make-obj 'disco 400 400 25 0 'forte 10 5))
(define obj2(make-obj 'retangulo 350 300 25 15 'fraco 5 10))
(define obj3(make-obj 'retangulo 300 350 10 10 'forte 5 10))
(define obj4(make-obj 'disco 450 400 5 0 'fraco 5 10))
(define obj5(make-obj 'disco 500 500 10 0 'forte 5 10))



;;gera-tela : funcao para gerar o programa

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


;;funções para criar o canhao e os objetos

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
               (move-ate-fora (move-bolha bolha) obj1 obj2 obj3 obj4 obj5))]))

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

;problema:
(define (fim-bolha obj bolha)
  (or
   (and (symbol=? 'fraco (disco-resist bolha))
        (= (obj-posn1 obj) (disco-posn1 bolha))
       ;(= (obj-posn2 obj) (disco-posn2 bolha))
       )
   (and (symbol=? 'forte (disco-resist bolha))
        (symbol=? 'forte (obj-resist obj))
        (= (obj-posn1 obj) (disco-posn1 bolha))
       ;(= (obj-posn2 obj) (disco-posn2 bolha))
       )))


