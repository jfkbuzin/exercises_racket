;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname JosueFilipeKeglevichDeBuzin) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")))))
(define-struct infracao(numero cnh placa hora data tipo))

;;um elemento novo do conjunto infracao é:
;;(make-infracao numero cnh placa hora data tipo)
;;onde:
;;numero: numero, numero sequencial de identificação do condutor envolvido na infração
;;cnh: string, numero da CNH do condutor envolvido na infração
;;placa: string, placa do veiculo envolvido na infração
;;hora: string, hora de ocorrencia da infração
;;data: numero, data de ocorrencia da infração, formato AAAAMMDD
;;tipo: simbolo, tipo de infração(leve, moderada, grave ou gravissima)

(define-struct condutor(nome cnh categoriaA categoriaB categoriaC categoriaD pontuacao ativo))

;;um elemento novo do conjunto condutor é:
;;(make-condutor nome cnh categoria pontuacao ativo)
;;onde:
;;nome: string, nome do condutor
;;cnh: string, numero da CNH do condutor
;;categoriaA: simbolo, se a categoria de habilitacao é A(sim ou nao)
;;categoriaB: simbolo, se a categoria de habilitacao é B(sim ou nao)
;;categoriaC: simbolo, se a categoria de habilitacao é C(sim ou nao)
;;categoriaD: simbolo, se a categoria de habilitacao é D(sim ou nao)
;;pontuacao: numero, pontos na carteira do condutor
;;ativo: , simbolo, define se o direito de dirigir está ativo ou não(sim ou nao)

;;um lista-condutores é:
;;1 - ou empty
;;2 - ou (cons condutor lista-condutores)
;;onde:
;;condutor : estrutura condutor
;;lista-condutores : lista-condutores
;;exemplos:
;;(define lista-condutores1 empty)
;;(define lista-condutores2(cons(make-condutor "filipe" "14BCV" 'sim 'nao 'nao 'nao 7 'sim) empty))
(define lista-condutores1(cons(make-condutor "filipe" "14BCV" 'sim 'nao 'nao 'nao 7 'sim)
                            (cons(make-condutor "joao" "17NNE" 'sim 'nao 'sim 'nao 3 'sim)
                            (cons(make-condutor "jonathan" "02VVA" 'nao 'sim 'nao 'sim 12 'sim) empty))))

;;um lista-infracoes é:
;;1 - ou empty
;;2 - ou (cons infracao lista-infracoes)
;;onde:
;;infracao : estrutura infracao
;;lista-infracoes : lista-infracoes
;;(define lista-infracoes1 empty)
;;(define lista-infracoes2(cons(make-infracao 200 "14EFD" "iar8825" "10:25" 20140104 'moderada) empty))
(define lista-infracoes1(cons(make-infracao 200 "17NNE" "iar8825" "10:25" 20140104 'moderada)
                            (cons(make-infracao 201 "02VVA" "var9988" "23:45" 20140104 'moderada)
                            (cons(make-infracao 202 "17NNE" "ted5050" "09:10" 20140104 'grave) empty))))

;;lab3 - 3)

;;acha-condutor : string lista-condutores: string
;;obj: acha o condutor correspondente ao cnh digitado
;;exemplo:
;;(acha-condutor "17NNE" lista-condutores1) -> "joao"

(define(acha-condutor cnh lista-condutores)
  (cond
    [(empty? lista-condutores) "nao encontrado"]
    [(string=? cnh (condutor-cnh(first lista-condutores)))(condutor-nome(first lista-condutores))]
    [else (acha-condutor cnh (rest lista-condutores))]))

;;acha-infracao : string lista-infracoes: string
;;obj: acha a infracao correspondente ao cnh digitado
;;exemplo:
;;(acha-infracao "17NNE" lista-infracoes1) -> "ted5050"

(define(acha-infracao cnh lista-infracoes)
  (cond
    [(empty? lista-infracoes) "nao encontrado"]
    [(string=? cnh (infracao-cnh(first lista-infracoes)))(infracao-placa(first lista-infracoes))]
    [else (acha-infracao cnh (rest lista-infracoes))]))

;;lista-resposta é:
;;1 - ou empty
;;2 - ou (cons nome (cons placa empty)), onde
;;nome : string
;;placa: string

;;norma-info : string lista-condutores lista-infracoes -> lista-resposta
;;obj: dado um numero de cnh, retorna o nome do condutor e a placa do veiculo correspondente
;;exemplo:
;;(norma-info "17NNE" lista-condutores1 lista-infracoes1) -> (cons "joao" (cons "ted5050" empty))

(define (norma-info cnh lista-condutores lista-infracoes)
  (cond
    [(or(string=? "nao encontrado"(acha-condutor cnh lista-condutores))
        (string=? "nao encontrado"(acha-infracao cnh lista-infracoes))) empty]
    [else(cons(acha-condutor cnh lista-condutores)
              (cons(acha-infracao cnh lista-infracoes) empty))]))

;;lab3 - 4)

;;inf-data : numero lista-infracoes -> numero
;;obj: identifica o numero de infracoes cometidas em uma data
;;exemplo:
;;(inf-data 20140104 lista-infracoes1) -> 2

(define (inf-data data lista-infracoes)
  (cond
    [(empty? lista-infracoes) 0]
    [(= data (infracao-data(first lista-infracoes)))(+ 1 (inf-data data(rest lista-infracoes)))]
    [else (inf-data data(rest lista-infracoes))]))

;;lab3 - 5)

;;lista-numeros é:
;;1 - ou empty
;;2 - ou (cons numeros lista-numeros), onde
;;numeros : numeros
;;lista-numeros: lista-numeros

;;inf-tipo : simbolo lista-infracoes -> lista-numeros
;;obj: retorna os numeros de identificacao de todas as infracoes de um determinado tipo
;;exemplo:
;;(inf-tipo 'moderada lista-infracoes1) -> (cons 200 (cons 201 empty))

(define(inf-tipo tipo lista-infracoes)
  (cond
    [(empty? lista-infracoes) empty]
    [(symbol=? (infracao-tipo (first lista-infracoes)) tipo)(cons(infracao-numero (first lista-infracoes))(inf-tipo tipo (rest lista-infracoes)))]
    [else (inf-tipo tipo (rest lista-infracoes))]))


;;lab3 - 6)

(define ptos-leve 1)
(define ptos-moderada 3)
(define ptos-grave 5)
(define ptos-gravissima 7)

;;tipo-ptos : simbolo -> numero
;;obj: tornar um simbolo em um numero correspondente
;;exemplo:
;;(tipo-ptos 'leve)

(define(tipo-ptos tipo)
  (cond
    [(symbol=? 'leve tipo) ptos-leve]
    [(symbol=? 'moderada tipo) ptos-moderada]
    [(symbol=? 'grave tipo) ptos-grave]
    [(symbol=? 'gravissima tipo) ptos-gravissima]))
  
(define infracao1(make-infracao 200 "02VVA" "iar8825" "10:25" 20140104 'moderada))

;;atualiza-ptos : infracao lista-condutores -> lista-condutores
;;obj: atualiza a pontuacao de um condutor conforme a gravidade da infracao cometida
;;exemplo:
;;(atualiza-ptos infracao1 lista-condutores1) - > (cons (make-condutor "filipe" "14BCV" 'sim 'nao 'nao 'nao 7 'sim)
;;- (cons (make-condutor "joao" "17NNE" 'sim 'nao 'sim 'nao 10 'sim) (cons (make-condutor "jonathan" "02VVA" 'nao 'sim 'nao 'sim 17 'sim) empty)))

(define(atualiza-ptos inf lista-condutores)
  (cond
    [(empty? lista-condutores) empty]
    [(string=? (infracao-cnh inf)(condutor-cnh(first lista-condutores)))(cons(make-condutor(condutor-nome(first lista-condutores))
                                                                                    (condutor-cnh(first lista-condutores))
                                                                                    (condutor-categoriaA(first lista-condutores))
                                                                                    (condutor-categoriaB(first lista-condutores))
                                                                                    (condutor-categoriaC(first lista-condutores))
                                                                                    (condutor-categoriaD(first lista-condutores))
                                                  (+ (tipo-ptos  (infracao-tipo inf))(condutor-pontuacao(first lista-condutores)))
                                                                                    (condutor-ativo(first lista-condutores)))
                                                                                    (atualiza-ptos inf(rest lista-condutores)))]
     [else (cons(first lista-condutores)(atualiza-ptos inf(rest lista-condutores)))]))                                                           
  

;;PROGRAMA1 :

;;tipo-inf-data : numero lista-infracoes -> simbolo
;;obj: mostra o tipo de infracao mais cometida em uma data
;;exemplo:
;;(tipo-inf-data 20140104 lista-infracoes1) -> 'moderada

(define (tipo-inf-data data lista-infracoes)
  (cond
    [(and(> (soma-leve data lista-infracoes) (soma-moderada data lista-infracoes))(and
                                    ( > (soma-leve data lista-infracoes)(soma-grave data lista-infracoes))
                                    ( > (soma-leve data lista-infracoes)(soma-gravissima data lista-infracoes)))) 'leve ]
    [(and(> (soma-moderada data lista-infracoes) (soma-leve data lista-infracoes))(and
                                    ( > (soma-moderada data lista-infracoes)(soma-grave data lista-infracoes))
                                    ( > (soma-moderada data lista-infracoes)(soma-gravissima data lista-infracoes)))) 'moderada ]
    [(and(> (soma-grave data lista-infracoes) (soma-leve data lista-infracoes))(and
                                    ( > (soma-grave data lista-infracoes)(soma-moderada data lista-infracoes))
                                    ( > (soma-grave data lista-infracoes)(soma-gravissima data lista-infracoes)))) 'grave ]
    [(and(> (soma-gravissima data lista-infracoes) (soma-leve data lista-infracoes))(and
                                    ( > (soma-gravissima data lista-infracoes)(soma-moderada data lista-infracoes))
                                    ( > (soma-gravissima data lista-infracoes)(soma-grave data lista-infracoes)))) 'gravissima ]
    [else " mais de um tipo de infracao com mesmo numero de infracoes na data ou nao ha infracoes nesta data"]))


;;soma-leve : numero lista-infracoes -> numero
;;obj: define a quantidade de infracoes leves cometidas em uma determinada data, auxiliar da funcao tipo-inf-data
;;exemplo: 
;;(soma-leve 20140104 lista-infracoes1) -> 0

(define(soma-leve data lista-infracoes)
  (cond
    [(empty? lista-infracoes) 0]
    [(and(symbol=? 'leve (infracao-tipo(first lista-infracoes)))(= data (infracao-data(first lista-infracoes))))
     (+ 1 (soma-leve  data (rest lista-infracoes)))]
    [else (soma-leve  data (rest lista-infracoes))]))


;;soma-moderada : numero lista-infracoes -> numero
;;obj: define a quantidade de infracoes moderadas cometidas em uma determinada data, auxiliar da funcao tipo-inf-data
;;exemplo: 
;;(soma-moderada 20140104 lista-infracoes1) -> 2

(define(soma-moderada data lista-infracoes)
  (cond
    [(empty? lista-infracoes) 0]
    [(and(symbol=? 'moderada (infracao-tipo(first lista-infracoes)))(= data (infracao-data(first lista-infracoes))))
     (+ 1 (soma-moderada  data (rest lista-infracoes)))]
    [else (soma-moderada  data (rest lista-infracoes))]))


;;soma-grave : numero lista-infracoes -> numero
;;obj: define a quantidade de infracoes graves cometidas em uma determinada data, auxiliar da funcao tipo-inf-data
;;exemplo: 
;;(soma-grave 20140104 lista-infracoes1) -> 1

(define(soma-grave data lista-infracoes)
  (cond
    [(empty? lista-infracoes) 0]
    [(and(symbol=? 'grave (infracao-tipo(first lista-infracoes)))(= data (infracao-data(first lista-infracoes))))
     (+ 1 (soma-grave  data (rest lista-infracoes)))]
    [else (soma-grave  data (rest lista-infracoes))]))


;;soma-gravissima : numero lista-infracoes -> numero
;;obj: define a quantidade de infracoes gravissimas cometidas em uma determinada data, auxiliar da funcao tipo-inf-data
;;exemplo: 
;;(soma-gravissima 20140104 lista-infracoes1) -> 0

(define(soma-gravissima data lista-infracoes)
  (cond
    [(empty? lista-infracoes) 0]
    [(and(symbol=? 'gravissima (infracao-tipo(first lista-infracoes)))(= data (infracao-data(first lista-infracoes))))
     (+ 1 (soma-gravissima  data (rest lista-infracoes)))]
    [else (soma-gravissima  data (rest lista-infracoes))]))



;;PROGRAMA2 :

;;atualiza-ptos2 : string lista-condutores lista-infracoes -> lista-condutores
;;obj: dado um numero de cnh, retorna a ficha do condutor com os dados atualizados conforme as infracoes cometidas, 
;;se a pontuacao total final do condutor for maior que 14 pontos, seu registro deixa de ser ativo e passa para 'nao
;;exemplos:
;;(atualiza-ptos2 "02VVA" lista-condutores1 lista-infracoes1) -> (cons
;;                                                               (make-condutor "filipe" "14BCV" 'sim 'nao 'nao 'nao 7 'sim)
;;                                                               (cons (make-condutor "joao" "17NNE" 'sim 'nao 'sim 'nao 3 'sim) 
;;                                                               (cons (make-condutor "jonathan" "02VVA" 'nao 'sim 'nao 'sim 15 'nao) empty)))
;;(atualiza-ptos2 "17NNE" lista-condutores1 lista-infracoes1) -> (cons
;;                                                               (make-condutor "filipe" "14BCV" 'sim 'nao 'nao 'nao 7 'sim)
;;                                                               (cons (make-condutor "joao" "17NNE" 'sim 'nao 'sim 'nao 11 'sim) 
;;                                                               (cons (make-condutor "jonathan" "02VVA" 'nao 'sim 'nao 'sim 12 'sim) empty)))

(define(atualiza-ptos2 cnh lista-condutores lista-infracoes)
  (cond
    [(empty? lista-condutores) empty]
    [(and(string=? cnh (condutor-cnh(first lista-condutores)))
         (>(+ (acha-infracao2 cnh lista-infracoes)(condutor-pontuacao(first lista-condutores))) 14))
                                                                 (cons(make-condutor(condutor-nome(first lista-condutores))
                                                                                    (condutor-cnh(first lista-condutores))
                                                                                    (condutor-categoriaA(first lista-condutores))
                                                                                    (condutor-categoriaB(first lista-condutores))
                                                                                    (condutor-categoriaC(first lista-condutores))
                                                                                    (condutor-categoriaD(first lista-condutores))
                                                  (+ (acha-infracao2 cnh lista-infracoes)(condutor-pontuacao(first lista-condutores)))
                                                                                     'nao ) 
                                                                      (atualiza-ptos2 cnh (rest lista-condutores) lista-infracoes))]
     [(and(string=? cnh (condutor-cnh(first lista-condutores)))(string=? cnh (retorna-cnh cnh lista-infracoes)))
                                                                 (cons(make-condutor(condutor-nome(first lista-condutores))
                                                                                    (condutor-cnh(first lista-condutores))
                                                                                    (condutor-categoriaA(first lista-condutores))
                                                                                    (condutor-categoriaB(first lista-condutores))
                                                                                    (condutor-categoriaC(first lista-condutores))
                                                                                    (condutor-categoriaD(first lista-condutores))
                                                  (+ (acha-infracao2 cnh lista-infracoes)(condutor-pontuacao(first lista-condutores)))
                                                                                    (condutor-ativo(first lista-condutores))) 
                                                                      (atualiza-ptos2 cnh (rest lista-condutores) lista-infracoes))]
    [(string=? cnh (condutor-cnh(first lista-condutores))) (cons(make-condutor(condutor-nome(first lista-condutores))
                                                                                    (condutor-cnh(first lista-condutores))
                                                                                    (condutor-categoriaA(first lista-condutores))
                                                                                    (condutor-categoriaB(first lista-condutores))
                                                                                    (condutor-categoriaC(first lista-condutores))
                                                                                    (condutor-categoriaD(first lista-condutores))
                                                                                    (condutor-pontuacao(first lista-condutores))
                                                                                    (condutor-ativo(first lista-condutores))) 
                                                           (atualiza-ptos2 cnh (rest lista-condutores) lista-infracoes))]
     [else (cons(first lista-condutores)(atualiza-ptos2 cnh (rest lista-condutores) lista-infracoes))]))                                                           
  

;;acha-infracao2 : string lista-infracoes: numero
;;obj: acha a pontuacao das infracoes correspondentes ao cnh digitado, auxiliar da funcao atualiza-ptos2
;;exemplo:
;;(acha-infracao2 "17NNE" lista-infracoes1) -> 8

(define(acha-infracao2 cnh lista-infracoes)
  (cond
    [(empty? lista-infracoes) 0]
    [(string=? cnh (infracao-cnh(first lista-infracoes)))
     (+ (tipo-ptos (infracao-tipo(first lista-infracoes)))(acha-infracao2 cnh (rest lista-infracoes)))]
    [else (acha-infracao2 cnh (rest lista-infracoes))]))

;;retorna-cnh : string lista-infracoes: string
;;obj: retorna o cnh digitado, auxiliar da funcao atualiza-ptos2
;;exemplo:
;;(retorna-cnh "17NNE" lista-infracoes1) -> "17NNE"

(define(retorna-cnh cnh lista-infracoes)
  (cond
    [(empty? lista-infracoes) "nao encontrado"]
    [(string=? cnh (infracao-cnh(first lista-infracoes)))(infracao-cnh(first lista-infracoes))]
    [else (retorna-cnh cnh (rest lista-infracoes))]))
  
