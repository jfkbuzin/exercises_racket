;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname RodrigoOkido_252745) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")))))
; Cria uma janela onde dentro fará 3 desenhos.
; Um círculo vermelho localizado nas coordenadas (300,100)
; de raio 50 e cor vermelha. Um retangulo azul localizado
; nas coordenadas (150 300) tendo 300 de comprimento e 200
; de largura. E uma linha que conecta estes dois desenhos
; pegando a localização das coordenadas de cada um dos 
; objetos recem criados.

;(start 600 600)
;(draw-solid-disk (make-posn 300 100) 50 'Red)
;(draw-solid-rect (make-posn 150 300) 300 200 'Blue)
;(draw-solid-line (make-posn 300 300 )(make-posn 300 150)'Black)




; Define como será o objeto do jogo. 
; Tipo: 'Simbolo - Pode ser uma nave espacial, tiro ou inimigo.
; posn: Dois inteiros que definirá a posição central do objeto.
; dx e dy: Dois inteiros. Define a velocidade do tiro, tendo o 
;          dx representando o deslocamento no eixo x e o dy sendo
;          uma unidade do tempo.
; tamObjeto: Verificará a partir do seu tamanho, a possibilidade de 
; haver uma colisão ou não.
; EXEMPLO: (make-objeto 'naveEspacial (make-posn 355 500) 8 0 7)
; DESENHO: Naves Espacial - circulo azul ('naveEspacial)
;                           Velocidade:  8, 0 (dx,dy)
;                           TamanhoObjeto: 7
;          Tiro - Retangulo fino preto ('tiro), 
;                 Velocidade: 3,10 (dx,dy)
;                 TamanhoObjeto: 3
;          Inimigo - retangulos vermelhos ('inimigo)
;                    Velocidade:  5, 3 (dx,dy)
;                    TamanhoObjeto: 5
(define-struct objeto(tipo posn dx dy tamObjeto))



; Dentro-da-tela? : posn, a-objeto --> booleano
; a-posn: X,Y - Dimensões da tela (no caso 600 600)
; a-objeto: objeto
; Recebe uma posição que representa as dimensões da tela 
; do jogo, e um objeto qualquer.
(define (dentro-da-tela? a-posn a-objeto)
  (cond
     [(and (<= (posn-x (objeto-posn a-objeto)) (posn-x a-posn)) 
           (<= (posn-x (objeto-posn a-objeto)) (posn-y a-posn))) true]
     [else false]
    ))



; gera-tiro: a-posn, direcao --> objeto(tipo tiro)
; a-posn: POSN
; direcao: Simbolo -> 'N ou 'S (Norte ou Sul)
; Recebe uma posição inicial(x,y) e uma direção (símbolo) informando 
; para qual direção o tiro irá se deslocar. ('N ou 'S)
; EXEMPLO: (gera-tiro (make-posn (3,4) 'N))
(define (gera-tiro a-posn direcao)
  (cond 
  [ (symbol=? direcao 'N)
    (make-objeto 'Tiro (make-posn (posn-x(objeto-posn a-posn))  (posn-y(objeto-posn a-posn))) -3 -10 3)]
  [ (symbol=? direcao 'S)
    (make-objeto 'Tiro (make-posn (posn-x(objeto-posn a-posn))  (posn-y(objeto-posn a-posn))) 3 10 3)]
  [else 'DirecaoDesconhecida]
  ))
  
 


; desenha-objeto: objeto --> draw teachpack
; Desenha um objeto na tela de acordo com seu tipo.
; Nave Espacial - Objeto circular com raio 30 de cor azul
; Tiro - Objeto retangular fino preto de dimensões 50x100
; Inimigo - Objeto retangular de dimensões 100x80 da cor vermelha
; EXEMPLO: (desenha-objeto (make-objeto 'naveEspacial (make-posn 80 40) 8 4 7))
(define (desenha-objeto a-objeto)
  (cond
    [(symbol=? (objeto-tipo a-objeto) 'naveEspacial) 
     (draw-solid-disk (make-posn (posn-x(objeto-posn a-objeto))(posn-y(objeto-posn a-objeto))) 20 'Blue)]
    [(symbol=? (objeto-tipo a-objeto) 'tiro) 
     (draw-solid-rect (make-posn (posn-x(objeto-posn a-objeto))(posn-y(objeto-posn a-objeto))) 4 100 'Black)]
    [(symbol=? (objeto-tipo a-objeto) 'inimigo) 
     (draw-solid-rect (make-posn (posn-x(objeto-posn a-objeto))(posn-y(objeto-posn a-objeto))) 50 20 'Red)]
    [else 'objetoDesconhecido]
    ))



; objetos-colidem?: objeto, objeto --> booleano
; Dado dois objetos, esta função identifica se os dois objetos em questão se colidem.
; Naves Espaciais: Podem se colidir, portanto, mesmo se tocando, a função retornará false.
; Tiros: Não podem se colidir, portanto, caso haja colisão, a função retornará true.
; Inimigos: Como as naves espaciais, podem se colidir. Retornando false mesmo havendo colisão.
; Naves e tiros: Não podem se colidir, caso aconteça, a função retorna true. 
(define (objetos-colidem? a-objeto b-objeto)
  (cond
    [(and (symbol=? (objeto-tipo a-objeto) 'tiro)(symbol=? (objeto-tipo b-objeto) 'tiro))
     (cond
      [(< (sqrt(+(sqr(-(posn-x(objeto-posn a-objeto) (posn-x(objeto-posn b-objeto)))))
                 (sqr(-(posn-y(objeto-posn a-objeto) (posn-y(objeto-posn b-objeto)))))))
        (+(objeto-tamObjeto a-objeto)(objeto-tamObjeto b-objeto))) true])]
    [(and (symbol=? (objeto-tipo a-objeto) 'naveEspacial)(symbol=? (objeto-tipo b-objeto) 'tiro))
        (cond
         [(< (sqrt(+(sqr(-(posn-x(objeto-posn a-objeto) (posn-x(objeto-posn b-objeto)))))
                 (sqr(-(posn-y(objeto-posn a-objeto) (posn-y(objeto-posn b-objeto)))))))
          (+(objeto-tamObjeto a-objeto)(objeto-tamObjeto b-objeto))) true])]
    [(and (symbol=? (objeto-tipo a-objeto) 'tiro)(symbol=? (objeto-tipo b-objeto) 'naveEspacial))
        (cond
          [(< (sqrt(+(sqr(-(posn-x(objeto-posn a-objeto) (posn-x(objeto-posn b-objeto)))))
                 (sqr(-(posn-y(objeto-posn a-objeto) (posn-y(objeto-posn b-objeto)))))))
          (+(objeto-tamObjeto a-objeto)(objeto-tamObjeto b-objeto))) true])]
    [else false]
    )
  )



; avança-objeto: objeto --> novo objeto
; Recebe um objeto e altera sua posição. 
; Nave Espacial: Altera posição apenas horizontalmente.
; Tiro: Altera posição apenas verticalmente 
; Inimigo: Altera posição apenas horizontalmente com leves alterações verticais
(define (avança-objeto a-objeto)
  (cond
    [(symbol=? (objeto-tipo a-objeto) 'naveEspacial)
     (make-objeto 'naveEspacial (make-posn (+ (posn-x (objeto-posn a-objeto)) (objeto-dx a-objeto)) (-(posn-y (objeto-posn a-objeto))(objeto-dy a-objeto))) 8 0 7)]    
    [(symbol=? (objeto-tipo a-objeto) 'tiro) 
     (make-objeto 'tiro (make-posn (posn-x (objeto-posn a-objeto)) (-(posn-y (objeto-posn a-objeto))(objeto-dy a-objeto))) 3 10 3)]
    [(symbol=? (objeto-tipo a-objeto) 'inimigo) 
     (make-objeto 'inimigo (make-posn (+ (posn-x (objeto-posn a-objeto))(objeto-dx a-objeto)) (+(posn-y (objeto-posn a-objeto))(objeto-dy a-objeto))) 5 3 5)]
    [else 'objetoDesconhecido]
    ))

       


; gera-tela: -> start (tela inicial)
; Gera a tela com cinco componentes do game. Neste caso, foi gerado a tela principal,
; uma nave espacial, uma representação de tiro da nave espacial, e 4 inimigos. Todos
; os cinco objetos estão localizados em posições arbitrárias.

(define (gera-tela)
 (and(start 600 600)
 (desenha-objeto (make-objeto 'naveEspacial (make-posn 300 500) 8 0 7))
 (desenha-objeto (make-objeto 'tiro (make-posn 300 280)  3 10 3))
 (desenha-objeto (make-objeto 'inimigo (make-posn 30 50) 5 3 5))
 (desenha-objeto (make-objeto 'inimigo (make-posn 110 50) 5 3 5))
 (desenha-objeto (make-objeto 'inimigo (make-posn 190 50) 5 3 5))
 (desenha-objeto (make-objeto 'inimigo (make-posn 270 50) 5 3 5))
 (desenha-objeto (make-objeto 'inimigo (make-posn 350 50) 5 3 5))
 (desenha-objeto (make-objeto 'inimigo (make-posn 430 50) 5 3 5))
 (desenha-objeto (make-objeto 'inimigo (make-posn 510 50) 5 3 5))
 )
)
                    
                             
   