
;;dados: string string -> string
;;obj;; testar se login e senha estao corretos
;;exemplo (dados "josue" "1234") -> dados foram aceitos
;;        (dados "josue" "2222") -> senha incorreta
;;        (dados "josh" "2222") -> login invalido

#lang racket
(define (dados login senha)
  (cond
    [(and (string=? login "josue")(string=? senha "1234")) (display "dados foram aceitos")]
    [(and (string=? login "josue")(not (string=? senha "1234"))) (display "senha incorreta")]
    [(not (string=? login "josue")) (display "login invalido")]))

