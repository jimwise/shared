#lang typed/racket

(require typed/racket/draw)
(require racket/fixnum)

(: escape (-> Float-Complex (#:cap Natural) Natural))
(define (escape c #:cap [cap 1000])
  (for/fold : Natural ([step : Natural 0]
                       [z : Float-Complex 0.0+0.0i]
                       #:result (if (= step cap) 0 step))
                       ([i (in-range cap)])
                       #:break (> (magnitude z) 2.0)
                       (values (add1 step) (+ c (* z z)))))

(: red (-> Integer Byte))
(define (red n) (assert (bitwise-and n #x0000ff) byte?))
(: green (-> Integer Byte))
(define (green n) (assert (arithmetic-shift (bitwise-and n #x00ff00) -8) byte?))
(: blue (-> Integer Byte))
(define (blue n) (assert (arithmetic-shift (bitwise-and n #xff0000) -16) byte?))

(: image : Positive-Integer Positive-Integer (#:cap Natural) (#:xmin Float) (#:xmax Float) (#:ymin Float) (#:ymax Float) -> (Instance Bitmap%))
(define (image width height #:cap [cap 1000]
               #:xmin [xmin -2.5] #:xmax [xmax 1.0] #:ymin [ymin -1.0] #:ymax [ymax 1.0])
  (let* ([img (make-bitmap width height)]
         [dc (make-object bitmap-dc% img)]
         [xstep : Float (/ (abs (- xmax xmin)) (exact->inexact width))]
         [ystep : Float (/ (abs (- ymax ymin)) (exact->inexact height))]
         [scalex (lambda ([n : Real]) (+ (* n xstep) xmin))]
         [scaley (lambda ([n : Real]) (+ (* n ystep) ymin))]
         [density-factor 4]
         [color (lambda ([i : Natural])
                  (let ([c (modulo (* density-factor i (floor (/ #xffffff cap))) #xffffff)])
                              (make-object color% (red c) (green c) (blue c))))])
    (for* ([x : Natural (in-range width)]
           [y : Natural (in-range height)])
          (let* ([c (make-rectangular (scalex x) (scaley y))]
                 [i (escape c)])
            (send dc set-pixel x y (color i))))
    img))

(send (time (image 1280 800)) save-file "./mandelbrot-rkt.png" 'png)
(send (time (image 1280 800 #:xmin -0.5 #:xmax 0.5 #:ymin 0.0 #:ymax 0.75))
      save-file "./mandelzoom1-rkt.png" 'png)
