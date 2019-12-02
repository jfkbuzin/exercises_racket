;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname JosueFilipeKeglevichDeBuzin) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct infracao(numero cnh placa hora data tipo))

;;um elemento novo do conjunto infracao é:
;;(make-infracao numero cnh placa hora data tipo)
;;onde:
;;numero: numero, numero sequencial de identificação do condutor envolvido na infração
;;cnh: numero, numero da CNH do condutor envolvido na infração
;;placa: string, placa do veiculo envolvido na infração
;;hora: string, hora de ocorrencia da infração
;;data: numero, data de ocorrencia da infração, formato AAAAMMDD
;;tipo: simbolo, tipo de infração(leve, moderada, grave ou gravissima)

(define-struct condutor(nome cnh categoriaA categoriaB categoriaC categoriaD pontuacao ativo esq dir))

;;um elemento novo do nó condutor é:
;;1. ou empty,
;;2. ou (make-condutor nome cnh categoria pontuacao ativo esq dir)
;;onde:
;;nome: string, nome do condutor
;;cnh: numero, numero da CNH do condutor
;;categoriaA: simbolo, se a categoria de habilitacao é A(sim ou nao)
;;categoriaB: simbolo, se a categoria de habilitacao é B(sim ou nao)
;;categoriaC: simbolo, se a categoria de habilitacao é C(sim ou nao)
;;categoriaD: simbolo, se a categoria de habilitacao é D(sim ou nao)
;;pontuacao: numero, pontos na carteira do condutor
;;ativo: , simbolo, define se o direito de dirigir está ativo ou não(sim ou nao)
;;esq e dir: esq e dir do no

(define abp1(make-condutor "filipe" 1400 'sim 'nao 'nao 'nao 7 'sim 
                               (make-condutor "joao" 1200 'sim 'nao 'sim 'nao 3 'sim empty empty)  
                           (make-condutor "jonathan" 1500 'nao 'sim 'nao 'sim 12 'sim empty empty) ))

(define infracao1(make-infracao 200 1200 "iar8825" "10:25" 20140104 'moderada))
(define infracao2(make-infracao 201 1400 "jer5545" "23:22" 20140512 'leve))
(define infracao3(make-infracao 202 1500 "for2234" "08:55" 20140521 'grave))

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

;;retorna-nome : numero no-condutor : string
;;obj: retorna o nome de um condutor conforme o cnh digitado
;;exemplo: (retorna-nome 1200 abp1) -> "joao"

(define(retorna-nome cnh abp)
  (cond
    [(empty? abp) "nao ha condutor com esse cnh"]
    [(= cnh (condutor-cnh abp)) (condutor-nome abp)]
    [(< cnh (condutor-cnh abp)) (retorna-nome cnh (condutor-esq abp))]
    [else (retorna-nome cnh (condutor-dir abp))]))

;;um lista-nomes é:
;;1 - ou empty
;;2 - ou (cons nome lista-nomes)
;;onde:
;;nome : string, nome de um condutor
;;lista-nomes : lista-nomes

;;ptos-nome : numero condutor : lista-nomes
;;obj: retorna os nomes de todos os condutores que tenham pontuação acima de um determinado valor passado como parâmetro
;;exemplo:
;;(ptos-nome 5 abp1) -> (cons "filipe"(cons "jonathan" empty))

(define(ptos-nome ptos abp)
  (cond
    [(empty? abp) empty]
    [(< ptos (condutor-pontuacao abp)) (cons(condutor-nome abp)(append(ptos-nome ptos (condutor-esq abp))
                                                                      (ptos-nome ptos (condutor-dir abp))))]
    [else (ptos-nome ptos (condutor-dir abp))]))


;;Extraclasse 1:

;;atualiza-ptos : infracao no-condutor -> no-condutor
;;obj: atualiza a pontuacao de um condutor conforme a gravidade da infracao cometida
;;exemplo:
;;(atualiza-ptos infracao1 abp1) - > (make-condutor "filipe" 1400 'sim 'nao 'nao 'nao 7 'sim(make-condutor "joao" 1200 'sim 'nao 'sim 'nao 6 'sim empty empty)  
;;(make-condutor "jonathan" 1500 'nao 'sim 'nao 'sim 12 'sim empty empty)

(define(atualiza-ptos inf abp)
  (cond
    [(empty? abp) empty]
    [(= (infracao-cnh inf)(condutor-cnh abp))
                                              (make-condutor (condutor-nome abp)(condutor-cnh abp)(condutor-categoriaA abp)(condutor-categoriaB abp)
                                                                (condutor-categoriaC abp)
                                                                (condutor-categoriaD abp)
                                                                (+ (tipo-ptos  (infracao-tipo inf))(condutor-pontuacao abp))
                                                                (condutor-ativo abp)
                                                                (atualiza-ptos inf (condutor-esq abp))
                                                                (atualiza-ptos inf (condutor-dir abp)))]
     [else (make-condutor (condutor-nome abp)(condutor-cnh abp)(condutor-categoriaA abp)(condutor-categoriaB abp)
                                                                (condutor-categoriaC abp)
                                                                (condutor-categoriaD abp)
                                                                (condutor-pontuacao abp)
                                                                (condutor-ativo abp)
                                                                (atualiza-ptos inf (condutor-esq abp))
                                                                (atualiza-ptos inf (condutor-dir abp)))]))          

;;(check-expect(atualiza-ptos infracao1 abp1)(make-condutor "filipe" 1400 'sim 'nao 'nao 'nao 7 'sim(make-condutor "joao" 1200 'sim 'nao 'sim 'nao 6 'sim empty empty)(make-condutor "jonathan" 1500 'nao 'sim 'nao 'sim 12 'sim empty empty)))
;;The test passed!

;;Extraclasse 2:

(define-struct condutor2(nome cnh categoriaA categoriaB categoriaC categoriaD pontuacao ativo))

;;um elemento novo do conjunto condutor2 é:
;;(make-condutor2 nome cnh categoria pontuacao ativo)
;;onde:
;;nome: string, nome do condutor
;;cnh: string, numero da CNH do condutor
;;categoriaA: simbolo, se a categoria de habilitacao é A(sim ou nao)
;;categoriaB: simbolo, se a categoria de habilitacao é B(sim ou nao)
;;categoriaC: simbolo, se a categoria de habilitacao é C(sim ou nao)
;;categoriaD: simbolo, se a categoria de habilitacao é D(sim ou nao)
;;pontuacao: numero, pontos na carteira do condutor
;;ativo: , simbolo, define se o direito de dirigir está ativo ou não(sim ou nao)

(define-struct no-condutor(cnh condutor2 esq dir))

;;um elemento novo do conjunto no-condutor é:
;;1. ou empty,
;;2. ou (make-no-condutor cnh condutor esq dir)
;;onde:
;;cnh: string, numero da CNH do condutor
;;condutor2: nome do condutor
;;esq e dir: esq e dir do no

;;um lista-condutores é:
;;1 - ou empty
;;2 - ou (cons condutor2 lista-condutores)
;;onde:
;;condutor2 : estrutura condutor2
;;lista-condutores : lista-condutores
;;exemplos:
;;(define lista-condutores1 empty)
;;(define lista-condutores2(cons(make-condutor2 "filipe" 1400 'sim 'nao 'nao 'nao 7 'sim) empty))
(define lista-condutores1(cons(make-condutor2 "Calebe" 1560 'sim 'nao 'nao 'nao 0 'sim)
                            (cons(make-condutor2 "Esdras" 1210 'sim 'nao 'sim 'nao 0 'sim)
                            (cons(make-condutor2 "Daniel" 1002 'nao 'sim 'nao 'sim 0 'sim) empty))))

;;ordena-condutores : cnh lista-condutores -> no-condutor
;;obj: cria uma arvore de condutores com o cnh digitado como raiz
;;exemplo:
;;(ordena-condutores 1560 lista-condutores1) -> (make-no-condutor 1560 "Calebe" (make-no-condutor 1210 "Esdras" (make-no-condutor 1002 "Daniel" empty empty) empty) empty)

(define (ordena-condutores cnh lista-condutores)
  (cond
    [(empty? lista-condutores) empty]
    [else (cond
            [(= cnh (condutor2-cnh (first lista-condutores)))(make-no-condutor(condutor2-cnh (first lista-condutores))
                                                                              (condutor2-nome (first lista-condutores))
                                                                              (insere-esq cnh (rest lista-condutores))
                                                                              (insere-dir cnh (rest lista-condutores)))]
            
            [else (ordena-condutores cnh (rest lista-condutores))])]))

;;(check-expect(ordena-condutores 1560 lista-condutores1)(make-no-condutor 1560 "Calebe" (make-no-condutor 1210 "Esdras" (make-no-condutor 1002 "Daniel" empty empty) empty) empty))
;;The test passed!


;;insere-esq : numero lista-condutores -> lista-condutores
;;obj: Dados um número e uma lista ordenada, insere o número na posição correta de um no, de forma que o no esteja ordenado

(define (insere-esq cnh lista-condutores)
  (cond
    [(empty? lista-condutores) empty]
    [else (cond
            [(> cnh (condutor2-cnh (first lista-condutores))) (make-no-condutor(condutor2-cnh (first lista-condutores))
                                                                              (condutor2-nome (first lista-condutores))
                                                                              (insere-esq (condutor2-cnh (first lista-condutores)) (rest lista-condutores))
                                                                              (insere-dir (condutor2-cnh (first lista-condutores)) (rest lista-condutores)))]
            [else (insere-esq cnh (rest lista-condutores))])]))

;;insere-dir : numero lista-condutores -> lista-condutores
;;obj: Dados um número e uma lista ordenada, insere o número na posição correta de um no, de forma que o no esteja ordenado

(define (insere-dir cnh lista-condutores)
  (cond
    [(empty? lista-condutores) empty]
    [else (cond
            [(< cnh (condutor2-cnh (first lista-condutores))) (make-no-condutor(condutor2-cnh (first lista-condutores))
                                                                              (condutor2-nome (first lista-condutores))
                                                                              (insere-esq (condutor2-cnh (first lista-condutores)) (rest lista-condutores))
                                                                              (insere-dir (condutor2-cnh (first lista-condutores)) (rest lista-condutores)))]
            [else (insere-dir cnh (rest lista-condutores))])]))



(define-struct funcionário (nome cargo salário equipe))

; Um elemento funcionário do conjunto Funcionário é uma estrutura:
; (make-funcionário n c s e), onde:
; - n : String, nome do funcionário
; - c : Símbolo, cargo do funcionário
; - s : Número, salário em reais do funcionário
; - e : Lista-de-funcionários

; Uma Lista-de-funcionários é:
; 1. ou empty
; 2. ou (cons f ldf) onde:
; - f : Funcionário
; - ldf : Lista-de-funcionários

;; Funcionários:
(define Silvia (make-funcionário "Sílvia" 'fiscal-de-rua 4000 empty))
(define Carlos (make-funcionário "Carlos" 'fiscal-de-rua 4500 empty))
(define Fernando (make-funcionário "Fernando" 'fiscal-de-rua 4500 empty))
(define Eq4 (list Silvia))
(define Jorge (make-funcionário "Jorge" 'chefe-de-equipe 8500 Eq4))
(define Eq3 (list Fernando Carlos))
(define Julio (make-funcionário "Júlio" 'chefe-de-equipe 10000 Eq3))
(define Gustavo (make-funcionário "Gustavo" 'vice-diretor 20000 empty))
(define Eq2 (list Julio Jorge))
(define Tatiana (make-funcionário "Tatiana" 'vice-diretor 22000 Eq2))
(define Eq1 (list Gustavo Tatiana))
(define Sergio (make-funcionário "Sérgio" 'diretor 30000 Eq1))

;;total-salario : funcionario -> numero
;;obj.: mostra o total pago de salario de um funcionario e de seus subordinados
;;exemplo:
;;(total-salario Silvia) -> 4000
;;(total-salario Tatiana) -> 53500

(define (total-salario func)
( + (funcionário-salário func)(total-equipe(funcionário-equipe func))))

;;total-equipe : lista-funcionarios -> numero
;;obj.: mostra o salario total de uma equipe de funcionarios
;;

(define (total-equipe ldf)
  (cond
    [(empty? ldf) 0]
    [else (+ (total-salario(first ldf))(total-equipe(rest ldf)))]))


;;Extraclasse 3:

; Uma Lista-nomes é:
; 1. ou empty
; 2. ou (cons n lista-nomes) onde:
; - n : string, nome do funcionario
; - lista-nomes : lista-nomes

;;retorna-nomes : simbolo no -> lista-nomes
;;obj: retorna os nomes de todos os funcionários de um nó que possuem um determinado cargo indicado
;;exemplo:
;;(retorna-nomes 'fiscal-de-rua Sergio) -> (list "Fernando" "Carlos" "Sílvia")

(define (retorna-nomes cargo abp)
  (cond
    [(empty? abp) empty]
    [(symbol=? cargo (funcionário-cargo abp)) (cons(funcionário-nome abp)
                                                   (nomes-equipe cargo (funcionário-equipe abp)))]
    [else (nomes-equipe cargo (funcionário-equipe abp))]))

;;nomes-equipe : simbolo lista-de-funcionarios -> lista-nomes
;;obj: retorna os nomes de todos os funcionários de uma lista que possuem um determinado cargo indicado, auxiliar de retorna-nomes

(define (nomes-equipe cargo ldf)
  (cond
    [(empty? ldf) empty]
    [(symbol=? cargo (funcionário-cargo (first ldf))) (cons(funcionário-nome (first ldf))(nomes-equipe cargo (rest ldf)))]
    [else (append(retorna-nomes cargo (first ldf))(nomes-equipe cargo (rest ldf)))]))

;;(check-expect(retorna-nomes 'fiscal-de-rua Sergio)(list "Fernando" "Carlos" "Sílvia"))
;;The test passed!
