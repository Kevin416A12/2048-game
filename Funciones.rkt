#lang racket
(provide (all-defined-out))

; ============
; NUMERO NUEVO
; ============

; Crea una fila de m columnas con un valor dado (es auxiliar a crear-matriz)
(define (crear-fila m valor)
  (if (= m 0)
      '()
      (cons valor (crear-fila (- m 1) valor))))

; Crea una matriz de n filas y m columnas inicializada con 0
(define (crear-matriz n m)
  (if (= n 0)
      '()
      (cons (crear-fila m 0) (crear-matriz (- n 1) m))))


; Cuenta cuántos ceros hay en una fila
(define (contar-ceros-fila fila)
  (cond
    [(null? fila) 0]
    [(= (car fila) 0) (+ 1 (contar-ceros-fila (cdr fila)))]
    [else (contar-ceros-fila (cdr fila))]))

; Cuenta cuántos ceros hay en la matriz
(define (contar-ceros matriz)
  (cond
    [(null? matriz) 0]
    [else (+ (contar-ceros-fila (car matriz)) (contar-ceros (cdr matriz)))]))

; Reemplaza el n-ésimo cero de una fila con un valor
(define (reemplazar-cero-fila fila n valor)
  (cond
    [(null? fila) '()]
    [(and (= (car fila) 0) (= n 0)) (cons valor (cdr fila))]
    [(= (car fila) 0) (cons (car fila) (reemplazar-cero-fila (cdr fila) (- n 1) valor))]
    [else (cons (car fila) (reemplazar-cero-fila (cdr fila) n valor))]))

; Auxiliar para decidir si procesar la fila actual o seguir a la siguiente
(define (reemplazar-en-fila-o-siguiente matriz n valor)
  (cond
    [(< n (contar-ceros-fila (car matriz)))
     (cons (reemplazar-cero-fila (car matriz) n valor) (cdr matriz))]
    [else
     (cons (car matriz) (reemplazar-cero-matriz (cdr matriz) (- n (contar-ceros-fila (car matriz))) valor))]))

; Reemplaza el n-ésimo cero de la matriz con un valor
(define (reemplazar-cero-matriz matriz n valor)
  (cond
    [(null? matriz) '()]
    [else (reemplazar-en-fila-o-siguiente matriz n valor)]))

; Elige aleatoriamente 2 o 4
(define (valor-random)
  (cond
    [(= (random 2) 0) 2]
    [else 4]))

; Función principal
(define (colocar-valor-random matriz)
  (cond
    [(= (contar-ceros matriz) 0) matriz]
    [else (reemplazar-cero-matriz matriz (random (contar-ceros matriz)) (valor-random))]))


; =====================
; CREAR TABLERO INICIAL 
; =====================

(define (colocar-valor matriz valor)
  (cond
    [(= (contar-ceros matriz) 0) matriz]
    [else
     (reemplazar-cero-matriz
      matriz
      (random (contar-ceros matriz))
      valor)]))

(define (crear-tablero-inicial n m)
  (colocar-valor
   (colocar-valor (crear-matriz n m) 2)
   2))

; =================================
; MOVER A LA IZQUIERDA SIN COMBINAR
; =================================

;cuenta cuántos elementos tiene una lista
(define (contar-elementos lista)
  (cond
    [(null? lista) 0]
    [else (+ 1 (contar-elementos (cdr lista)))]))

; quita los ceros de una fila
(define (quitar-ceros fila)
  (cond
    [(null? fila) '()]
    [(= (car fila) 0)
     (quitar-ceros (cdr fila))]
    [else
     (cons (car fila)
           (quitar-ceros (cdr fila)))]))

; agrega ceros al final hasta llegar al tamaño indicado
(define (rellenar-con-ceros fila tamano)
  (cond
    [(= (contar-elementos fila) tamano) fila]
    [else
     (rellenar-con-ceros
      (append fila '(0))
      tamano)]))

; mueve una fila a la izquierda sin combinar
(define (mover-fila-izquierda-simple fila)
  (rellenar-con-ceros
   (quitar-ceros fila)
   (contar-elementos fila)))


; ========================
; COMBINAR FILAS IZQUIERDA
; ========================

; ; combina una fila ya corrida a la izquierda
(define (combinar-fila fila)
  (cond
    [(null? fila) '()]
    [(null? (cdr fila)) fila]
    [(= (car fila) (cadr fila))
     (cons (+ (car fila) (cadr fila))
           (combinar-fila (cddr fila)))]
    [else
     (cons (car fila)
           (combinar-fila (cdr fila)))]))

; movimiento completo de una fila hacia la izquierda
(define (mover-fila-izquierda fila)
  (rellenar-con-ceros
   (combinar-fila (mover-fila-izquierda-simple fila))
   (contar-elementos fila)))

; ======================
; COMBINAR FILAS DERECHA
; ======================

; invierte una lista
(define (invertir lista)
  (cond
    [(null? lista) '()]
    [else
     (append (invertir (cdr lista))
             (list (car lista)))]))

; mueve una fila hacia la derecha
(define (mover-fila-derecha fila)
  (invertir
   (mover-fila-izquierda
    (invertir fila))))
; toma el primer elemento de cada fila
(define (primeros matriz)
  (cond
    [(null? matriz) '()]
    [else
     (cons (caar matriz)
           (primeros (cdr matriz)))]))


; =================
; TRANSPONER MATRIS
; =================
; toma el resto de cada fila
(define (restos matriz)
  (cond
    [(null? matriz) '()]
    [else
     (cons (cdar matriz)
           (restos (cdr matriz)))]))

; transpone una matriz
(define (transponer matriz)
  (cond
    [(null? matriz) '()]
    [(null? (car matriz)) '()]
    [else
     (cons (primeros matriz)
           (transponer (restos matriz)))]))

; ===============================
; MOVIMIENTOS COMPLETOS DE MATRIS
; ===============================
; aplica una función a cada fila de la matriz
(define (aplicar-a-matriz matriz f)
  (cond
    [(null? matriz) '()]
    [else
     (cons (f (car matriz))
           (aplicar-a-matriz (cdr matriz) f))]))

; mueve todo el tablero hacia la izquierda
(define (mover-izquierda matriz)
  (aplicar-a-matriz matriz mover-fila-izquierda))

; mueve todo el tablero hacia la derecha
(define (mover-derecha matriz)
  (aplicar-a-matriz matriz mover-fila-derecha))

; mueve todo el tablero hacia arriba
(define (mover-arriba matriz)
  (transponer
   (mover-izquierda
    (transponer matriz))))

; mueve todo el tablero hacia abajo
(define (mover-abajo matriz)
  (transponer
   (mover-derecha
    (transponer matriz))))


