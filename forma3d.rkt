;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname forma3d) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct cubo(lado))
 
;;um elemento novo do conjunto cubo é:
;;(make cubo lado)
;;lado: numero, comprimento do lado do cubo
  
(define-struct prisma(altura largura profundidade))

;;um elemento novo do conjunto prisma é:
;;(make prisma altura largura profundidade)
;;altura: numero, altura do objeto
;;largura: numero, largura do objeto
;;profundidade: numero, profundidade do objeto

(define-struct esfera(raio))
 
;;um elemento novo do conjunto esfera é:
;;(make esfera raio)
;;raio: numero, raio da esfera

;;um elemento de forma3d é:
;;1. ou um elemento de cubo
;;2. ou um elemento de prisma
;;3. ou um elemento de esfera

;;volume: forma3d -> numero
;;obj: medir o volume de um objeto
;;exemplo:
;;(define cubo1(make-cubo 45))
;;(define prisma1(make-prisma 30 12 50))
;;(define esfera1(make-esfera 5))
;;(volume cubo1) -> 91125
;;(volume prisma1) -> 18000
;;(volume esfera1) -> 523,33

(define(volume objeto)
  (cond
    [(cubo? objeto)(expt(cubo-lado objeto)3)]
    [(prisma? objeto)(*(prisma-altura objeto)(prisma-largura objeto)(prisma-profundidade objeto))]
    [(esfera? objeto)(*(/ 4 3)(* pi(expt(esfera-raio objeto)3)))]))
    
;;mesma-forma: forma3d forma3d -> booleano
;;obj: ver se 2 objetos sao iguais e possuem o mesmo volume
;;exemplo:
;;(define prisma1(make-prisma 30 12 50))
;;(define prisma2(make-prisma 30 12 50))
;;(mesma-forma prisma1 prisma2) -> true

(define(mesma-forma f1 f2)
  (cond
    [(and(cubo? f1)(cubo? f2))(=(cubo-lado f1)(cubo-lado f2))]
    [(and(prisma? f1)(prisma? f2))(and(=(prisma-altura f1)(prisma-altura f2))(=(prisma-largura f1)(prisma-largura f2))(=(prisma-profundidade f1)(prisma-profundidade f2)))]
    [(and(esfera? f1)(esfera? f2))(=(esfera-raio f1)(esfera-raio f2))]
    [else "false"]))





  