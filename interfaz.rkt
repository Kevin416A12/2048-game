#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require "Funciones.rkt")

; ===============================
; ESTADOS DEL PROGRAMA
; ===============================

; El mundo será una lista:
; (list 'inicio tamano)
; (list 'jugando tablero tamano puntaje)
; (list 'victoria tablero tamano puntaje)
; (list 'game-over tablero tamano puntaje)
; (list 'salir)

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
    INSTRUCCION-SALIR 300 350
    (place-image
     INSTRUCCION-INICIAR 300 310
     (place-image
      INSTRUCCION-TAMANO 300 270
      (place-image
       (texto-tamano (estado-tamano mundo)) 300 220
        (place-image
         TITULO 300 100
         FONDO)))))))

(define (texto-tamano n)
  (text (string-append "Tamano del tablero: "
                       (number->string n)
                       "x"
                       (number->string n))
        24
        "darkgreen"))

(define INSTRUCCION-TAMANO
  (text "Use las flechas para cambiar el tamano" 18 "black"))

(define INSTRUCCION-INICIAR
  (text "Presione ENTER para iniciar" 20 "black"))

(define INSTRUCCION-SALIR
  (text "Presione ESC para salir" 18 "gray"))

; ===============================
; CAMBIAR TAMANO
; ===============================

(define (aumentar-tamano n)
  (if (< n 10)
      (+ n 1)
      n))

(define (disminuir-tamano n)
  (if (> n 4)
      (- n 1)
      n))

; ===============================
; COLOR SEGUN VALOR
; ===============================
(define (color-celda valor)
  (cond
    [(= valor 0)    (make-color 205 193 180)]

    [(= valor 2)    (make-color 238 228 218)]
    [(= valor 4)    (make-color 237 224 200)]

    [(= valor 8)    (make-color 242 177 121)]
    [(= valor 16)   (make-color 245 149 99)]

    [(= valor 32)   (make-color 246 124 95)]
    [(= valor 64)   (make-color 246 94 59)]

    [(= valor 128)  (make-color 237 207 114)]
    [(= valor 256)  (make-color 237 204 97)]
    [(= valor 512)  (make-color 237 200 80)]

    [(= valor 1024) (make-color 237 197 63)]
    [(= valor 2048) (make-color 237 194 46)]

    [else           (make-color 60 58 50)]))


; ===============================
; DIBUJO DE MATRIZ 
; ===============================

(define AREA-TABLERO 420)

(define (dibujar-celda valor tam)
  (overlay
   (if (= valor 0)
       empty-image
       (text (number->string valor)
             (max 12 (quotient tam 4))
             (if (<= valor 4)
                 "black"
                 "white")))
   (rectangle tam tam "solid" (color-celda valor))))

(define (dibujar-fila fila tam)
  (cond
    [(null? fila) empty-image]
    [else
     (beside
      (dibujar-celda (car fila) tam)
      (dibujar-fila (cdr fila) tam))]))

(define (dibujar-tablero tablero tam)
  (cond
    [(null? tablero) empty-image]
    [else
     (above
      (dibujar-fila (car tablero) tam)
      (dibujar-tablero (cdr tablero) tam))]))

;Calcula el tamano de la celda
(define (tam-celda mundo)
  (quotient AREA-TABLERO (estado-tamano mundo)))

; =================
; DIBUJO DEL PUNTAJE
; =================

(define (dibujar-puntaje puntaje)
  (text
   (string-append
    "Puntaje: "
    (number->string puntaje))
   24
   "black"))


; =================
; LECTURA DE ESTADO
; =================

(define (estado-modo mundo)
  (car mundo))

(define (estado-tamano mundo)
  (cond
    [(symbol=? (estado-modo mundo) 'inicio)
     (cadr mundo)]
    [else
     (caddr mundo)]))

(define (estado-tablero mundo)
  (cond
    [(or (symbol=? (estado-modo mundo) 'jugando)
         (symbol=? (estado-modo mundo) 'victoria)
         (symbol=? (estado-modo mundo) 'game-over))
     (cadr mundo)]
    [else
     '()]))

(define (estado-puntaje mundo)
  (cond
    [(symbol=? (estado-modo mundo) 'inicio)
     0]

    [else
     (cadddr mundo)]))
; ===============================
; MANEJO DE TECLAS
; ===============================

(define (manejar-teclado mundo tecla)
  (cond

    ; iniciar juego
    [(and (symbol=? (estado-modo mundo) 'inicio)
          (key=? tecla "\r"))
     (list 'jugando
      (crear-tablero-inicial
       (estado-tamano mundo)
       (estado-tamano mundo))
      (estado-tamano mundo)
      0)]

    ; cambiar tamaño en menú
    [(and (symbol=? (estado-modo mundo) 'inicio)
          (key=? tecla "right"))
     (list 'inicio
           (aumentar-tamano (estado-tamano mundo)))]

    [(and (symbol=? (estado-modo mundo) 'inicio)
          (key=? tecla "left"))
     (list 'inicio
           (disminuir-tamano (estado-tamano mundo)))]

    ; salir
    [(key=? tecla "escape")
     (list 'salir)]

    ; mover izquierda
    [(and (symbol=? (estado-modo mundo) 'jugando)
      (key=? tecla "left"))

 (local [(define resultado
           (mover-izquierda-con-puntaje
            (estado-tablero mundo)))

         (define nuevo-tablero
           (car resultado))

         (define puntaje-ganado
           (cadr resultado))

         (define tablero-final
           (if (equal? nuevo-tablero
                       (estado-tablero mundo))
               nuevo-tablero
               (colocar-valor-random nuevo-tablero)))

         (define puntaje-final
           (+ (estado-puntaje mundo)
              puntaje-ganado))]

   (list 'jugando
         tablero-final
         (estado-tamano mundo)
         puntaje-final))]

    ; mover derecha
    [(and (symbol=? (estado-modo mundo) 'jugando)
          (key=? tecla "right"))

     (local [(define resultado
           (mover-derecha-con-puntaje
            (estado-tablero mundo)))

         (define nuevo-tablero
           (car resultado))

         (define puntaje-ganado
           (cadr resultado))

         (define tablero-final
           (if (equal? nuevo-tablero
                       (estado-tablero mundo))
               nuevo-tablero
               (colocar-valor-random nuevo-tablero)))

         (define puntaje-final
           (+ (estado-puntaje mundo)
              puntaje-ganado))]
       
     (list 'jugando
         tablero-final
         (estado-tamano mundo)
         puntaje-final))]

    ; mover arriba
    [(and (symbol=? (estado-modo mundo) 'jugando)
          (key=? tecla "up"))

     (local [(define resultado
           (mover-arriba-con-puntaje
            (estado-tablero mundo)))

         (define nuevo-tablero
           (car resultado))

         (define puntaje-ganado
           (cadr resultado))

         (define tablero-final
           (if (equal? nuevo-tablero
                       (estado-tablero mundo))
               nuevo-tablero
               (colocar-valor-random nuevo-tablero)))

         (define puntaje-final
           (+ (estado-puntaje mundo)
              puntaje-ganado))]
       
     (list 'jugando
         tablero-final
         (estado-tamano mundo)
         puntaje-final))]

    ; mover abajo
    [(and (symbol=? (estado-modo mundo) 'jugando)
          (key=? tecla "down"))
     (local [(define resultado
           (mover-abajo-con-puntaje
            (estado-tablero mundo)))

         (define nuevo-tablero
           (car resultado))

         (define puntaje-ganado
           (cadr resultado))

         (define tablero-final
           (if (equal? nuevo-tablero
                       (estado-tablero mundo))
               nuevo-tablero
               (colocar-valor-random nuevo-tablero)))

         (define puntaje-final
           (+ (estado-puntaje mundo)
              puntaje-ganado))]
     (list 'jugando
         tablero-final
         (estado-tamano mundo)
         puntaje-final))]

    [else mundo]))
; ===============================
; DIBUJO GENERAL
; ===============================

(define (dibujar mundo)
  (cond
    [(symbol=? (estado-modo mundo) 'inicio)
     (dibujar-inicio mundo)]

    [(symbol=? (estado-modo mundo) 'jugando)
     (place-image
      (dibujar-tablero (estado-tablero mundo)
                       (tam-celda mundo))
      300 280
      (place-image
       (dibujar-puntaje (estado-puntaje mundo))
       300 50
      FONDO))]

    [else
     (place-image
      (text "Saliendo..." 24 "black")
      300 250
      FONDO)]))
; ===============================
; DETENER EL JUEGO
; ===============================

(define (terminar? mundo)
  (symbol=? (estado-modo mundo) 'salir))

; ===============================
; INICIO
; ===============================

(big-bang (list 'inicio 4)
  [to-draw dibujar]
  [on-key manejar-teclado]
  [stop-when terminar?])


