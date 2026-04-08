#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require "Funciones.rkt")

; ===============================
; ESTADOS DEL PROGRAMA
; ===============================

; El mundo será un símbolo:
; 'inicio   -> pantalla inicial
; 'jugando  -> tablero en juego
; 'salir    -> cerrar

; ===============================
; DIBUJO DE LA PANTALLA DE INICIO
; ===============================

(define ANCHO 600)
(define ALTO 500)

(define FONDO
  (rectangle ANCHO ALTO "solid" "white"))

(define TITULO
  (text "2048" 40 "black"))

(define MENSAJE-INICIO
  (text "Presione ENTER para iniciar" 20 "black"))

(define MENSAJE-SALIR
  (text "Presione ESC para salir" 18 "gray"))

(define AUTOR
  (text "Proyecto de Paradigmas" 16 "gray"))

(define (dibujar-inicio mundo)
  (place-image
   AUTOR 300 430
   (place-image
    MENSAJE-SALIR 300 340
    (place-image
     MENSAJE-INICIO 300 300
      (place-image
       TITULO 300 130
       FONDO)))))



; ===============================
; DIBUJO DE MATRIZ 
; ===============================

(define TAM-CELDA 80)

(define (dibujar-celda valor)
  (overlay
   (if (= valor 0)
       empty-image
       (text (number->string valor) 20 "black"))

   (rectangle TAM-CELDA TAM-CELDA "outline" "black")))

(define (dibujar-fila fila)
  (cond
    [(null? fila) empty-image]
    [else
     (beside
      (dibujar-celda (car fila))
      (dibujar-fila (cdr fila)))]))

(define (dibujar-tablero tablero)
  (cond
    [(null? tablero) empty-image]
    [else
     (above
      (dibujar-fila (car tablero))
      (dibujar-tablero (cdr tablero)))]))

(define TABLERO-PRUEBA
  '((2 0 0 2)
    (4 4 0 0)
    (0 2 2 0)
    (0 0 0 4)))


; ===============================
; MANEJO DE TECLAS
; ===============================

(define (manejar-teclado mundo tecla)
  (cond
    [(and (symbol=? mundo 'inicio)
          (key=? tecla "\r"))
     'jugando]

    [(key=? tecla "escape")
     'salir]

    [else mundo]))

; ===============================
; DIBUJO GENERAL
; ===============================

(define (dibujar mundo)
  (cond
    [(symbol=? mundo 'inicio)
     (dibujar-inicio mundo)]

    [(symbol=? mundo 'jugando)
     (place-image
      (dibujar-tablero TABLERO-PRUEBA)
      300 250
      FONDO)]

    [else
     (place-image
      (text "Saliendo..." 24 "black")
      300 250
      FONDO)]))

; ===============================
; DETENER EL JUEGO
; ===============================

(define (terminar? mundo)
  (symbol=? mundo 'salir))

; ===============================
; INICIO
; ===============================

(big-bang 'inicio
  [to-draw dibujar]
  [on-key manejar-teclado]
  [stop-when terminar?])





