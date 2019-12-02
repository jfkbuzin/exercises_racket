;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab2_B_josue) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")))))
(define-struct registro(numero cnh placa hora data tipo))

;;um elemento novo do conjunto registro é:
;;(make-registro numero cnh placa hora data tipo)
;;onde:
;;numero: numero, numero sequencial de identificação do condutor envolvido na infração
;;cnh: numero, numero da CNH do condutor envolvido na infração
;;placa: string, placa do veiculo envolvido na infração
;;hora: string, hora de ocorrencia da infração
;;data: string, data de ocorrencia da infração
;;tipo: string, tipo de infração(leve, moderada, grave ou gravissima)

;;pontuacao-infracao : registro -> string
;;obj: descobrir qual é a pontuação de uma infração conforme o registro da mesma
;;exemplos:
;;(define registro1(make-registro 200 00014 "iar8825" "10:25" "01/04" "moderada"))
;;(pontuacao-tipo registro1)-> "3 pontos"

(define(pontuacao-tipo um-registro)
  (cond
    [(string=? (registro-tipo um-registro)"leve") "1 ponto"]
    [(string=? (registro-tipo um-registro)"moderada") "3 pontos"]
    [(string=? (registro-tipo um-registro)"grave") "5 pontos"]
    [(string=? (registro-tipo um-registro)"gravissima") "7 pontos"]
    [else "tipo de infração errado"]))