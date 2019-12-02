;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab2_B_josue2) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")))))
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

(define-struct dados(data hora pontuacao))

;;um elemento novo do conjunto dados é:
;;(make-dados data hora pontuacao)
;;onde:
;;data: numero, data de ocorrencia da infração, formato AAAAMMDD
;;hora: string, hora de ocorrencia da infração
;;pontuacao: simbolo, tipo de infração(leve, moderada, grave ou gravissima)

(define-struct controle(total leve moderada grave gravissima))

;;um elemento novo do conjunto controle é:
;;(make-controle total leve moderada grave gravissima)
;;onde:
;;total: numero, total de infracoes cadastradas
;;leve: numero, total de infracoes leves cadastradas
;;moderada: numero, total de infracoes moderadas cadastradas
;;grave: numero, total de infracoes graves cadastradas
;;gravissima: numero, total de infracoes gravissimas cadastradas

  
;;pontos-infracao : simbolo -> numero
;;obj: descobrir qual é a pontuação de uma infração conforme a gravidade da mesma
;;exemplos:
;;(define infracao1(make-infracao 200 "14EFD" "iar8825" "10:25" 20140104 'moderada))
;;(pontos-infracao infracao1)-> 3

(define(pontos-infracao tipo)
  (cond
    [(symbol=? (infracao-tipo tipo) 'leve ) 1]
    [(symbol=? (infracao-tipo tipo) 'moderada) 3]
    [(symbol=? (infracao-tipo tipo) 'grave) 5]
    [(symbol=? (infracao-tipo tipo) 'gravissima) 7]
    [else "tipo de gravidade de infração errado"]))

;;(check-expect(pontos-infracao infracao1)3) -> The test passed!

;;questao 3:

;;habilitado-categoria : condutor simbolo -> booleano
;;obj: descobrir se o condutor está habilitado em uma categoria de habilitacao
;;exemplos:
;;(define condutor1(make-condutor "filipe" "14BCV" 'sim 'nao 'nao 'nao 7 'sim))
;;(habilitado-categoria condutor1 'A)-> 'sim
;;(habilitado-categoria condutor1 'B)-> 'nao

(define(habilitado-categoria condutor categoria)
  (cond
    [(symbol=? categoria 'A) (condutor-categoriaA condutor)]
    [(symbol=? categoria 'B) (condutor-categoriaB condutor)]
    [(symbol=? categoria 'C) (condutor-categoriaC condutor)]
    [(symbol=? categoria 'D) (condutor-categoriaD condutor)]
    [else "tipo de categoria errado"]))

;;(check-expect(habilitado-categoria condutor1 'A)'sim) -> The test passed!
;;(check-expect(habilitado-categoria condutor1 'B)'nao) -> Both tests passed!


;;questao 4:

;;dados-infracao : infracao -> inf
;;obj: dada uma infracao, sejam apresentados suma data, hora e pontuacao correspondente
;;exemplos:
;;(define infracao1(make-infracao 200 "14EFD" "iar8825" "10:25" 20140104 'moderada))
;;(dados-infracao infracao1)-> (make-dados 20140104 "10:25" 'moderada)

(define(dados-infracao inf)(make-dados (infracao-data inf)(infracao-hora inf)(infracao-tipo inf)))

;;(check-expect(dados-infracao infracao1)(make-dados 20140104 "10:25" 'moderada)) -> The test passed!


;;questao 5:

;;atualiza-pontos : condutor infracao -> condutor
;;obj: atualiza os pontos na carteira de um condutor conforme uma nova infracao
;;exemplos:
;;(define condutor1(make-condutor "filipe" "14EFD" 'sim 'nao 'nao 'nao 7 'sim))
;;(define infracao1(make-infracao 200 "14EFD" "iar8825" "10:25" 20140104 'moderada))
;;(atualiza-pontos condutor1 infracao1)-> (make-condutor "filipe" "14EFD" 'sim 'nao 'nao 'nao 10 'sim))

(define(atualiza-pontos c inf)
  (cond
    [(string=?(condutor-cnh c)(infracao-cnh inf))
              (make-condutor (condutor-nome c)
                             (condutor-cnh c)
                             (condutor-categoriaA c)
                             (condutor-categoriaB c)
                             (condutor-categoriaC c)
                             (condutor-categoriaD c)
                             (+(condutor-pontuacao c)(pontos-infracao inf))
                             (condutor-ativo c))]
    [else c]))

;;(check-expect(atualiza-pontos condutor1 infracao1)(make-condutor "filipe" "14EFD" 'sim 'nao 'nao 'nao 10 'sim)) -> The test passed!


;;questao 6:

;;atualiza-total : controle infracao -> controle
;;obj:  atualiza as informações de totais de infrações com os dados da infração fornecida
;;exemplos:
;;(define controle1(make-controle 10 5 2 2 1))
;;(define infracao1(make-infracao 200 "14EFD" "iar8825" "10:25" 20140104 'moderada))
;;(atualiza-total controle1 infracao1)-> (make-controle 11 5 3 2 1)

(define(atualiza-total cont inf)
                (make-controle(+ 1(controle-total cont))
                 (cond
                  [(symbol=? (infracao-tipo inf) 'leve)(+ 1(controle-leve cont))]
                  [else(controle-leve cont)])
                  (cond
                  [(symbol=? (infracao-tipo inf) 'moderada)(+ 1(controle-moderada cont))]
                  [else(controle-moderada cont)])
                  (cond
                  [(symbol=? (infracao-tipo inf) 'grave)(+ 1(controle-grave cont))]
                  [else(controle-grave cont)])
                  (cond
                  [(symbol=? (infracao-tipo inf) 'gravissima)(+ 1(controle-gravissima cont))]
                  [else(controle-gravissima cont)])))

;;(check-expect(atualiza-total controle1 infracao1)(make-controle 11 5 3 2 1)) -> The test passed!


;;PROGRAMA1 :

;;programa-1 : controle -> porcentagem
;;obj: define o percentual de infracoes de cada tipo conforme o total de infracoes registradas
;;exemplos:
;;(define controle1(make-controle 10 5 2 2 1))
;;(programa-1 controle1 'leve) -> 50

(define(programa-1 cont tipo)
  (cond
    [(symbol=? 'leve tipo) (/(* (controle-leve cont) 100) (controle-total cont))] 
    [(symbol=? 'moderada tipo) (/(* (controle-moderada cont) 100) (controle-total cont))]
    [(symbol=? 'grave tipo) (/(* (controle-grave cont) 100) (controle-total cont))]
    [(symbol=? 'gravissima tipo) (/(* (controle-gravissima cont) 100) (controle-total cont))]))
    
;;(check-expect(programa-1 controle1 'leve)50) -> The test passed!
;;(check-expect(programa-1 controle1 'moderada)20) -> The test passed!
;;(check-expect(programa-1 controle1 'grave)20) -> Both tests passed!
;;(check-expect(programa-1 controle1 'gravissima)10) -> All 3 tests passed!


;;PROGRAMA2 :

(define-struct res3(inf numero placa))

;;um elemento novo do conjunto res3 é:
;;(make-res3 inf numero placa)
;;onde:
;;inf: string, nome da infracao
;;numero: numero, numero da infração
;;placa: string, placa do carro envolvido na infracao

;;programa-2 : infracao infracao infracao -> res3
;;obj: a partir de 3 infracoes, mostra qual custou mais pontos, o numero da mesma e a placa do carro envolvido
;;exemplos:
;;(define infracao1(make-infracao 200 "14EFD" "iar8825" "10:25" 20140104 'moderada))
;;(define infracao2(make-infracao 201 "14EFD" "var9988" "23:45" 20140310 'grave))
;;(define infracao3(make-infracao 202 "14EFD" "ted5050" "09:10" 20140401 'gravissima))
;;(programa-2 infracao1 infracao2 infracao3) -> "infracao3" 201 "ted5050"

(define(programa-2 inf1 inf2 inf3)
  (cond
    [(and (> (pontos-infracao inf1) (pontos-infracao inf2))(> (pontos-infracao inf1) (pontos-infracao inf3)))
     (make-res3 "infracao1" (infracao-numero inf1)(infracao-placa inf1))]
    
    [(and (> (pontos-infracao inf2) (pontos-infracao inf1))(> (pontos-infracao inf2) (pontos-infracao inf3)))
     (make-res3 "infracao2" (infracao-numero inf2)(infracao-placa inf2))]
    
    [(and (> (pontos-infracao inf3) (pontos-infracao inf1))(> (pontos-infracao inf3) (pontos-infracao inf2)))
     (make-res3 "infracao3" (infracao-numero inf3)(infracao-placa inf3))]
    
    [else "mais de uma infracao com o mesmo numero de pontos"]))

;;(check-expect(programa-2 infracao1 infracao2 infracao3)(make-res3 "infracao3" 202 "ted5050")) -> The test passed!


;;PROGRAMA3 :

(define-struct 2cond(nome_cond num_inf cnh_inf placa_inf hora_inf data_inf tipo_inf pontuacao_cond))

;;um elemento novo do conjunto 2cond é:
;;(make-2cond nome_cond num_inf cnh_inf placa_inf hora_inf data_inf tipo_inf pontuacao_cond)
;;onde:
;;nome_cond: string, nome do condutor com mais pontos na carteira
;;num_inf: numero, numero da infração cujos pontos estao sendo contabilizados na carteira do condutor
;;cnh_inf: string, numero da CNH do condutor envolvido na infração
;;placa_inf: string, numero da placa do carro envolvido na infracao
;;hora_inf: string, hora em que ocorreu a infracao
;;data_inf: numero, data de ocorrencia da infração, formato AAAAMMDD
;;tipo_inf: simbolo, tipo de infração(leve, moderada, grave ou gravissima)
;;pontuacao_cond: numero, pontuacao na carteira do motorista atualizada com os pontos da infracao

;;programa-3 : condutor condutor simbolo infracao -> 2cond
;;obj: entre 2 condutores, adicionar os pontos de uma multa ao que tiver mais pontos, mostrar o nome e os pontos do condutor atualizados e mostrar 
;;a multa que foi contabilizada nos pontos do condutor

;;exemplo1:
;;(define condutor1(make-condutor "filipe" "14BCV" 'sim 'nao 'nao 'nao 7 'sim))
;;(define condutor2(make-condutor "joao" "02VVA" 'sim 'nao 'nao 'nao 10 'sim))
;;(define infracao1(make-infracao 202 "14EFD" "ted5050" "09:10" 20140401 'gravissima))
;;(programa-3 condutor1 condutor2 'A infracao1) -> (make-2cond "joao" 202 "14EFD" "ted5050" "09:10" 20140401 'gravissima 17)

;;(check-expect(programa-3 condutor1 condutor2 'A infracao1)(make-2cond "joao" 202 "14EFD" "ted5050" "09:10" 20140401 'gravissima 17)) -> The test passed!

;;exemplo2:
;;(define condutor1(make-condutor "filipe" "14BCV" 'sim 'nao 'nao 'nao 7 'sim))
;;(define condutor2(make-condutor "joao" "02VVA" 'nao 'sim 'nao 'nao 10 'sim))
;;(define infracao1(make-infracao 202 "14EFD" "ted5050" "09:10" 20140401 'gravissima))
;;(programa-3 condutor1 condutor2 'A infracao1) -> "apenas 1 ou nenhum condutor esta habilitado na categoria indicada"

;;(check-expect(programa-3 condutor1 condutor2 'A infracao1) "apenas 1 ou nenhum condutor esta habilitado na categoria indicada") -> The test passed!

;;exemplo3:
;;(define condutor1(make-condutor "filipe" "14BCV" 'nao 'sim 'nao 'nao 5 'sim))
;;(define condutor2(make-condutor "batista" "02VVA" 'nao 'sim 'nao 'nao 7 'sim))
;;(define infracao1(make-infracao 200 "14EFD" "ted5050" "09:10" 20140401 'grave))
;;(programa-3 condutor1 condutor2 'B infracao1) -> (make-2cond "batista" 200 "14EFD" "ted5050" "09:10" 20140401 'grave 12)

;;(check-expect(programa-3 condutor1 condutor2 'B infracao1)(make-2cond "batista" 200 "14EFD" "ted5050" "09:10" 20140401 'grave 12)) -> The test passed!

;;exemplo4:
;;(define condutor1(make-condutor "filipe" "14BCV" 'nao 'nao 'sim 'nao 3 'sim))
;;(define condutor2(make-condutor "eduardo" "02VVA" 'nao 'nao 'sim 'nao 1 'sim))
;;(define infracao1(make-infracao 200 "14EFD" "ted5050" "09:10" 20140401 'moderada))
;;(programa-3 condutor1 condutor2 'C infracao1) -> (make-2cond "filipe" 200 "14EFD" "ted5050" "09:10" 20140401 'moderada 6)

;;(check-expect(programa-3 condutor1 condutor2 'C infracao1)(make-2cond "filipe" 200 "14EFD" "ted5050" "09:10" 20140401 'moderada 6)) -> The test passed!


;;exemplo5:
;;(define condutor1(make-condutor "filipe" "14BCV" 'nao 'nao 'nao 'sim 10 'sim))
;;(define condutor2(make-condutor "jonathan" "02VVA" 'nao 'nao 'nao 'sim 15 'sim))
;;(define infracao1(make-infracao 200 "14EFD" "ted5050" "09:10" 20140401 'leve))
;;(programa-3 condutor1 condutor2 'D infracao1) -> (make-2cond "jonathan" 200 "14EFD" "ted5050" "09:10" 20140401 'leve 16)

;;(check-expect(programa-3 condutor1 condutor2 'D infracao1)(make-2cond "jonathan" 200 "14EFD" "ted5050" "09:10" 20140401 'leve 16)) -> The test passed!

(define(programa-3 cond1 cond2 categoria inf)
  (cond
    [(and(symbol=? categoria 'A)(and(and(symbol=? (condutor-categoriaA cond1)'sim)(symbol=? (condutor-categoriaA cond2)'sim))
                                    (>(condutor-pontuacao cond1)(condutor-pontuacao cond2))))
     (make-2cond(condutor-nome cond1)(infracao-numero inf)(infracao-cnh inf)(infracao-placa inf)(infracao-hora inf)(infracao-data inf)
                (infracao-tipo inf)(+(condutor-pontuacao cond1)(pontos-infracao inf)))]
    
    [(and(symbol=? categoria 'A)(and(and(symbol=? (condutor-categoriaA cond1)'sim)(symbol=? (condutor-categoriaA cond2)'sim))
                                    (>(condutor-pontuacao cond2)(condutor-pontuacao cond1))))
     (make-2cond(condutor-nome cond2)(infracao-numero inf)(infracao-cnh inf)(infracao-placa inf)(infracao-hora inf)(infracao-data inf)
                (infracao-tipo inf)(+(condutor-pontuacao cond2)(pontos-infracao inf)))]
    
    [(and(symbol=? categoria 'B)(and(and(symbol=? (condutor-categoriaB cond1)'sim)(symbol=? (condutor-categoriaB cond2)'sim))
                                    (>(condutor-pontuacao cond1)(condutor-pontuacao cond2))))
     (make-2cond(condutor-nome cond1)(infracao-numero inf)(infracao-cnh inf)(infracao-placa inf)(infracao-hora inf)(infracao-data inf)
                (infracao-tipo inf)(+(condutor-pontuacao cond1)(pontos-infracao inf)))]
    
    [(and(symbol=? categoria 'B)(and(and(symbol=? (condutor-categoriaB cond1)'sim)(symbol=? (condutor-categoriaB cond2)'sim))
                                    (>(condutor-pontuacao cond2)(condutor-pontuacao cond1))))
     (make-2cond(condutor-nome cond2)(infracao-numero inf)(infracao-cnh inf)(infracao-placa inf)(infracao-hora inf)(infracao-data inf)
                (infracao-tipo inf)(+(condutor-pontuacao cond2)(pontos-infracao inf)))]
    
    [(and(symbol=? categoria 'C)(and(and(symbol=? (condutor-categoriaC cond1)'sim)(symbol=? (condutor-categoriaC cond2)'sim))
                                    (>(condutor-pontuacao cond1)(condutor-pontuacao cond2))))
     (make-2cond(condutor-nome cond1)(infracao-numero inf)(infracao-cnh inf)(infracao-placa inf)(infracao-hora inf)(infracao-data inf)
                (infracao-tipo inf)(+(condutor-pontuacao cond1)(pontos-infracao inf)))]
    
    [(and(symbol=? categoria 'C)(and(and(symbol=? (condutor-categoriaC cond1)'sim)(symbol=? (condutor-categoriaC cond2)'sim))
                                    (>(condutor-pontuacao cond2)(condutor-pontuacao cond1))))
     (make-2cond(condutor-nome cond2)(infracao-numero inf)(infracao-cnh inf)(infracao-placa inf)(infracao-hora inf)(infracao-data inf)
                (infracao-tipo inf)(+(condutor-pontuacao cond2)(pontos-infracao inf)))]
    
    [(and(symbol=? categoria 'D)(and(and(symbol=? (condutor-categoriaD cond1)'sim)(symbol=? (condutor-categoriaD cond2)'sim))
                                    (>(condutor-pontuacao cond1)(condutor-pontuacao cond2))))
     (make-2cond(condutor-nome cond1)(infracao-numero inf)(infracao-cnh inf)(infracao-placa inf)(infracao-hora inf)(infracao-data inf)
                (infracao-tipo inf)(+(condutor-pontuacao cond1)(pontos-infracao inf)))]
    
    [(and(symbol=? categoria 'D)(and(and(symbol=? (condutor-categoriaD cond1)'sim)(symbol=? (condutor-categoriaD cond2)'sim))
                                    (>(condutor-pontuacao cond2)(condutor-pontuacao cond1))))
     (make-2cond(condutor-nome cond2)(infracao-numero inf)(infracao-cnh inf)(infracao-placa inf)(infracao-hora inf)(infracao-data inf)
                (infracao-tipo inf)(+(condutor-pontuacao cond2)(pontos-infracao inf)))]
    
    [else "apenas 1 ou nenhum condutor esta habilitado na categoria indicada"]))
