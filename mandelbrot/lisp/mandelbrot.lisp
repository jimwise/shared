;; given a complex number, return iterations to escape, up to cap,
;; or zero if no escape occurs

(ql:quickload "zpng")
(use-package :zpng)

(defun escape (c &key (cap 1000))
  "given a complex number, return iterations to escape, up to cap, or zero if no escape occurs"
  (declare (type (complex double-float) c)
           (optimize (speed 3) (safety 0)))
  (do ((z #c(0d0 0d0) (+ c (* z z)))
       (i 0 (1+ i)))
      ((= i cap) 0)
    (when (> (abs z) 2.0d0)
      (return i))))

(defun red (c) (ldb (byte 8 0) c))
(defun green (c) (ldb (byte 8 1) c))
(defun blue (c) (ldb (byte 8 2) c))

(defun image (width height &key (cap 1000)
	            (xmin -2.5d0) (xmax 1.0d0) (ymin -1.0d0) (ymax 1.0d0))
  (declare (type fixnum width height cap)
           (type double-float xmin xmax ymin ymax)
           (optimize (speed 3) (safety 0)))
  "return a png image of the mandelbrot set from xmin + ymin * i to xmax + ymax * i"
  (let* ((png (make-instance 'png
			     :color-type :truecolor
			     :width width
			     :height height))
	 (image (data-array png))
	 (xstep (/ (abs (- xmax xmin)) width))
	 (ystep (/ (abs (- ymax ymin)) height))
	 (density-factor 16))
  (flet ((color (i) (mod (* density-factor i (floor #xffffff cap)) #xffffff))
	 (scalex (n) (+ (* n xstep) xmin))
	 (scaley (n) (+ (* n ystep) ymin)))
    (dotimes (x width png)
      (dotimes (y height)
	(let ((c (color (escape (complex (scalex x) (scaley y)) :cap cap))))
	  (setf (aref image y x 2) (red c))
	  (setf (aref image y x 1) (green c))
	  (setf (aref image y x 0) (blue c))))))))

(with-open-file (f "mandelbrot-lisp.png" :direction :output :if-exists :supersede)
  (write-png (time (image 1280 800)) f))

(with-open-file (f "mandelzoom1-lisp.png" :direction :output :if-exists :supersede)
  (write-png (time (image 1280 800 :xmin -0.5d0 :xmax 0.5d0 :ymin 0d0 :ymax 0.75d0)) f))
