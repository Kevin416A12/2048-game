;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Funciones) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;El siguiente bloque de codigo corresponde a la funcion crear tablero KAN-1

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


;lo siguiente corresponde a la funcion de numero nuevo KAN-2

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