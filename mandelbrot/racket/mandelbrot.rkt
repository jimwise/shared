#lang racket

(require racket/draw)
(require racket/fixnum)

;; given a complex number, return iterations to escape, up to cap,
;; or zero if no escape occurs

(define (escape c #:cap [cap 1000])
  (call/cc (lambda (break)
             (do ((z 0+0i (+ c (expt z 2)))
                  (i 0 (add1 i)))
                 ((= i cap) 0)
               (when (> (magnitude z) 2.0)
                 (break i))))))

(define (red n) (bitwise-and n #x0000ff))
(define (green n) (arithmetic-shift (bitwise-and n #x00ff00) -8))
(define (blue n) (arithmetic-shift (bitwise-and n #xff0000) -16))

(define (image width height #:cap [cap 1000]
               #:xmin [xmin -2.5] #:xmax [xmax 1.0] #:ymin [ymin -1.0] #:ymax [ymax 1.0])
  (let* ([img (make-bitmap width height)]
         [dc (make-object bitmap-dc% img)]
         [xstep (/ (abs (- xmax xmin)) width)]
         [ystep (/ (abs (- ymax ymin)) height)]
         [scalex (lambda (n) (+ (* n xstep) xmin))]
         [scaley (lambda (n) (+ (* n ystep) ymin))]
         [density-factor 4]
         [color (lambda (i) 
                  (let ([c (modulo (* density-factor i (floor (/ #xffffff cap))) #xffffff)])
                              (make-object color% (red c) (green c) (blue c))))])
    (for* ([x (in-range width)]
           [y (in-range height)])
          (let* ([c (make-rectangular (scalex x) (scaley y))]
                 [i (escape c)])
            (send dc set-pixel x y (color i))))
    img))
     
(send (image 1280 800 #:cap (expt 2 12)) save-file "./mandelbrot-rkt.png" 'png)
(send (image 1280 800 #:cap (expt 2 12) #:xmin -0.5 #:xmax 0.5 #:ymin 0 #:ymax 0.75)
      save-file "./mandelzoom1-rkt.png" 'png)
