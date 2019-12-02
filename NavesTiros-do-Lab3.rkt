;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname NavesTiros-do-Lab3) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")))))
;; EXERCICIOS EXTRA-CLASSE 3

;; Definição de de constantes.
(define ALTURA 400)
(define LARGURA 400)

;; Lista de tamanhos das naves invasoras:
(define LISTA-TAM (cons 17 (cons 13 (cons 10 empty))))
;; Lista de cores das naves invasoras:
(define LISTA-COR (cons 'CornflowerBlue (cons 'LightPink (cons 'Teal empty))))
;; Lista de tamanhos dos tiros:
(define LISTA-TIROS (cons 2 (cons 3 (cons 4 empty))))

(define-struct velocidade (dx dy))
;; Um elemento velocidade de Velocidade é (make-velocidade um-dx um-dy), onde:
;;  um-dx: Numero, variacao de x.
;;  um-dy: Numero, variacao de y.
(define VEL1 (make-velocidade 10 10))
(define VEL2 (make-velocidade 20 20))
(define VEL3 (make-velocidade 30 30))
;---------------------------------------------------------------------------------------------------------------------
;; Definições de objetos.
(define-struct nave-inv (centro vel tipo))

;----------------------------------------------------------------------------------------------------------------------
;; seleciona-tamanho: Número -> Elemento da lista de tamanho das naves (Número).
;; Dado um número de 1 a 3 a função acessa o elemento desejado na lista.
(define (seleciona-tamanho tipo)
  (cond
    [(= tipo 1) (first LISTA-TAM)]
    [(= tipo 2) (first (rest LISTA-TAM))]
    [(= tipo 3) (first (rest (rest LISTA-TAM)))]
    ))
;; seleciona-cor: Número -> Cor da lista de cores das naves (Símbolo).
;; Dado um número de 1 a 3 a função acessa a cor desejada na lista.
(define (seleciona-cor tipo)
  (cond
    [(= tipo 1) (first LISTA-COR)]
    [(= tipo 2) (first (rest LISTA-COR))]
    [(= tipo 3) (first (rest (rest LISTA-COR)))]
    ))

;; QUESTÃO 1 ------------------------------------------------------------------------------------------------------------

;; cria-invasora-aux: 

(define (cria-invasora-aux n tipo dist centro vel)
  (cond
    [(<= n 0) empty]
    [else (cons (make-nave-inv centro vel tipo)
                (cria-invasora-aux (- n 1) tipo dist (make-posn (+ dist (posn-x centro)) (posn-y centro)) vel))]))

;; cria-linha-invasora: Número Número Número Número -> n número de Coordenadas distribuidos em uma linha.
;;(define cria-linha-invasora(t y n velocidade))
;; t -> é um Número que representa o tipo de nave
;; y -> é um Posn que representa a coordenada da nave
;; n -> é um Número que representa a quantidade de naves
;; velocidade -> é um Número que representa a velocidade do conjunto de linha invasora.

(define (cria-linha-invasora n t y vel)
  (cria-invasora-aux n t (/ LARGURA (+ n 1)) (make-posn (/ LARGURA (+ n 1)) y) vel))


;; QUESTÃO 2 ---------------------------------------------------------------------------------------------------------
(start LARGURA ALTURA)

;; desenha-tipo-t: Lista Número Número Número -> Booleano
;; (define (desenha-tipo-t lista t y n)
;; Funcao auxiliar que desenha um tipo t de nave invasora n vezes.

(define (desenha-tipo-t lista t y n)
  (cond
    [(<= n 0) true]
    [else
     (and (draw-solid-disk
           (nave-inv-centro (first lista))
           (seleciona-tamanho t)
           (seleciona-cor t)) ;;desenhou
          (desenha-tipo-t (rest lista) t (muda-y t y) (- n 1)))]))

;; muda-y: Número Número -> Número
;; (define (muda-y tipo y)
;;Função auxiliar que dado um tipo de nave calcula a cordenada y das linhas invasoras.

(define (muda-y tipo y)
  (cond
    [(= tipo 1) y]
    [(= tipo 2) (+ y 100)]
    [(= tipo 3) (+ y 200)]))

;; Testes:
;(check-expect 
; (desenha-tipo-t (cria-linha-invasora 5 1 50  VEL1) 1 100 5) true)
;(check-expect 
; (desenha-tipo-t (cria-linha-invasora 5 2 100 VEL1) 2 100 5) true)
;(check-expect 
; (desenha-tipo-t (cria-linha-invasora 5 3 150 VEL1) 3 100 5) true)

;; Funcao principal: gera-cena
;; gera-cena: Numero -> Booleano
;; Recebe um numero de naves e desenha a cena do jogo.

(define (gera-cena n)
  (and 
   (desenha-tipo-t (cria-linha-invasora n 1 50 VEL1) 1 100 n)
   (desenha-tipo-t (cria-linha-invasora n 2 100 VEL2) 2 100 n)
   (desenha-tipo-t (cria-linha-invasora n 3 150 VEL3) 3 100 n)
   (desenha-barricada (make-barricada (make-tijolo (make-posn 85 250) true)
                   (make-tijolo (make-posn 95 250) true)
                   (make-tijolo (make-posn 105 250) true)
                   (make-tijolo (make-posn 105 260) true)
                   (make-tijolo (make-posn 85 260) true)))
    (desenha-barricada (make-barricada (make-tijolo (make-posn 185 250) true)
                   (make-tijolo (make-posn 195 250) true)
                   (make-tijolo (make-posn 205 250) true)
                   (make-tijolo (make-posn 205 260) true)
                   (make-tijolo (make-posn 185 260) true)))
    (desenha-barricada (make-barricada (make-tijolo (make-posn 280 250) true)
                   (make-tijolo (make-posn 290 250) true)
                   (make-tijolo (make-posn 300 250) true)
                   (make-tijolo (make-posn 300 260) true)
                   (make-tijolo (make-posn 280 260) true)))
   )
 )

(define-struct tijolo (pos-central destruido))
;; Um tijolo é uma estrutura (make-tijolo uma-pos-central um-destruido), onde:
;;  uma-pos-central: Posn, posicao central do tijolo no plano cartesiano.
;;  um-destruido: Booleano, true se o tijolo ja foi destruido, false se nao.

(define-struct barricada (tij-1 tij-2 tij-3 tij-4 tij-5))
;; Uma barricada é uma estrutura
;; (make-barricada um-tij-1 um-tij-2 um-tij-3 um-tij-4 um-tij-5), onde:
;;  um-tij-1, um-tij-2, um-tij-3, um-tij-4 e um-tij-5: Tijolo, tijolos da barricada.

;; desenha-barricada: Barricada -> Booleano
;; Retorna true se o desenho ocorreu com sucesso.
(define (desenha-barricada d-barricada)
  (and (draw-solid-rect (make-posn (posn-x (tijolo-pos-central (barricada-tij-1 d-barricada)))
                                   (posn-y (tijolo-pos-central (barricada-tij-1 d-barricada))))
                        10 10 'LimeGreen)
       (draw-solid-rect (make-posn (posn-x (tijolo-pos-central (barricada-tij-2 d-barricada)))
                                   (posn-y (tijolo-pos-central (barricada-tij-2 d-barricada))))
                        10 10 'LimeGreen)
       (draw-solid-rect (make-posn (posn-x (tijolo-pos-central (barricada-tij-3 d-barricada)))
                                   (posn-y (tijolo-pos-central (barricada-tij-3 d-barricada))))
                        10 10 'LimeGreen)
       (draw-solid-rect (make-posn (posn-x (tijolo-pos-central (barricada-tij-4 d-barricada)))
                                   (posn-y (tijolo-pos-central (barricada-tij-4 d-barricada))))
                        10 10 'LimeGreen)
       (draw-solid-rect (make-posn (posn-x (tijolo-pos-central (barricada-tij-5 d-barricada)))
                                   (posn-y (tijolo-pos-central (barricada-tij-5 d-barricada))))
                        10 10 'LimeGreen)))

(draw-solid-disk (make-posn 200 390) 20 'MediumPurple)
(gera-cena 4)

;movimenta bola
(define-struct bola (x y incr-x incr-y cor raio))

;;Um elemento bola de Bola é uma estrutura
;; (make-bola x y dx dy c r)

;;Definir constantes:
(define espera 0.10)
(define cor 'green)
(define raio 10)

;; apaga a bola
(define (desenha-e-apaga uma-bola)
  (and
   (draw-solid-disk (make-posn (bola-x uma-bola)
                               (bola-y uma-bola))
                    (bola-raio uma-bola)
                    (bola-cor uma-bola))
   (sleep-for-a-while espera)
   (clear-solid-disk (make-posn (bola-x uma-bola)
                                (bola-y uma-bola))
                     (bola-raio uma-bola)
                     (bola-cor uma-bola))))
;; move-bola
(define (move-bola uma-bola)
  (make-bola (+ (bola-x uma-bola)
                (bola-incr-x uma-bola))
             (+ (bola-y uma-bola)
                (bola-incr-y uma-bola))
             (bola-incr-x uma-bola)
             (bola-incr-y uma-bola)
             (bola-cor uma-bola)
             (bola-raio uma-bola)))

;;Para mover a bola algumas vezes, poderíamos escrever:
(define nossa-bola (make-bola 80 180 1 5 'black 5))

(define (fora-dos-limites? uma-bola)
  (not
  (and
   (<= 0 (bola-x uma-bola) LARGURA)
   (<= 0 (bola-y uma-bola) ALTURA))))

;;move-até-que-fora: Bola -> Booleano
(define (move-ate-que-fora uma-bola)
  (cond
    ;;testar se a bola está na mesa, se não estiver retornar true
    [(fora-dos-limites? uma-bola) true]
    ;; se estiver dentro da mesa, desenha-e-apaga, move a bola e continua movento
    [else (and (desenha-e-apaga uma-bola)
               (move-ate-que-fora (move-bola uma-bola)))]))

(move-ate-que-fora nossa-bola)
(move-ate-que-fora (make-bola 160 170 0 5 'black 5))
(move-ate-que-fora (make-bola 240 180 0 5 'black 5))
(move-ate-que-fora (make-bola 320 180 -2 5 'black 5))



;; QUESTÃO 3 --------------------------------------------------------------------------------------------------------
;; ListasTiroNave: guarda duas listas.
;; (define ListasTiroNave cria-linha-invasora lista-de-tiros) *necessita de uma struct que concatena as duas listas*

;; remove-colisões: Lista Lista -> Lista sem elementos que colidiram.



