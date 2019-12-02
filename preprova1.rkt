;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname preprova1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct pessoa (nome ano pai mae))

;; Um parente é:
;; 1. empty;
;; 2. (make-pessoa n a p m), onde:
;; n: símbolo (nome da pessoa)
;; a: número (ano de nascimento da pessoa)
;; p: parente (pai desta pessoa)
;; m: parente (mãe desta pessoa)

(define fred(make-pessoa "fred" 1966 empty empty))

(define carl(make-pessoa "carl" 1926 empty empty))

(define betina(make-pessoa "betina" 1922 empty empty))

(define eva(make-pessoa "eva" 1965 carl betina))

(define gustav(make-pessoa "gustav" 1988 fred eva))

(define no1(make-pessoa "gustav" 1988 
                        (make-pessoa "fred" 1966 empty empty) 
                        (make-pessoa "eva" 1965 
                                     (make-pessoa "carl" 1926 empty empty) 
                                     (make-pessoa "betina" 1922 empty empty))))

;;idade-media : pessoa -> numero
;;obj.: calcular a media das idades dos ascendentes de uma pessoa
;;exemplo:
;;(idade-media gustav) -> 70.25

;(define(idade-media no)
;  (local
    (define(idade-media no)
         (cond
           [(empty? no) 0]
           [(and(empty? (pessoa-pai no))(empty? (pessoa-mae no))) 0]
           [else (calculo-media no)]))
       
       (define (calculo-media no)
    ( / (+ (soma-idades (pessoa-mae no))(soma-idades (pessoa-pai no))) 
        (+ (total-parentes (pessoa-mae no))(total-parentes (pessoa-pai no)))))
      
      (define(total-parentes no)
        (cond
          [(empty? no) 0]
          [else (+ 1 ( + (total-parentes (pessoa-pai no))
                       (total-parentes (pessoa-mae no))))]))
      (define(soma-idades no)
        (cond
          [(empty? no) 0]
          [else (+ ( - 2015 (pessoa-ano no)) ( +(soma-idades (pessoa-pai no)) 
                                                    (soma-idades (pessoa-mae no))))]))
    
 ;   (idade-media no)))
            
  
    

;(define(total-parentes no)
;  (cond
;    [(empty? no) 0]
;    [else (append( + 1 (total-parentes (pessoa-pai no)))
;                 ( + 1 (total-parentes (pessoa-mae no))))]

;(define(soma-idades no)
 ; (cond
 ;   [(empty? no) 0]
 ;   [else (+ ( - 2015 (pessoa-ano no)) (append(soma-idades (pessoa-pai no)) 
  ;                                            (soma-idades (pessoa-mae no))))]))
