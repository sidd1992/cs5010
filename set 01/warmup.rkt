;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(define TIME-ON-TASK 2)

;; Q13 
;; distance-formula : Number Number -> Number
;; a function that consumes two numbers, x and y, 
;; and that computes the distance of point (x,y) to the origin.
;; Examples and Tests
 (begin-for-test
  (check-equal? (distance-formula 6 8)
                10))
 
 (define (distance-formula x y)
  (sqrt (+ (sqr (- x 0)) (sqr (- y 0)))))


;; Q14
;; cube-volume : Number -> Number
;; function which accepts the length of a side of a cube
;; and computes its volume
;; Examples and tests
(begin-for-test
  (check-equal? (cube-volume 2) 8)
  (check-equal? (cube-surface 2) 24))

 (define (cube-volume e)
   (expt e 3))


 (define (cube-surface e)
   (* 6 e e))


;; Q15
;; string-first : String -> String
;; function which extracts the first character from a non-empty string
;; EXAMPLE AND TEST
 (begin-for-test
  (check-equal? (string-first "demo") "d"))
 
 (define (string-first str)
   (string-ith str 0))

;; q16
;; string-last : String -> String
;; function which extracts the last character from a non-empty string.
;; Example and tests
 (begin-for-test 
   (check-equal? (string-last "demo") "o"))
    
 (define (string-last str)
   (substring str (- (string-length str) 1)))
 
;; q17
;; bool-imply : Boolean Boolean -> Boolean
;; It consumes two Boolean values, call them b1 and b2.
;; The answer of the function is #true if b1 is false 
;; or b2 is true.
;; Example and tests
 (begin-for-test
   (check-equal? (bool-imply #t #t) #f)
   (check-equal? (bool-imply #f #t) #t))               
 (define (bool-imply b1 b2)
   (if (and (boolean=? b1 #f) (boolean=? b2 #t))
       #t #f)) 
 
;; q18
;; image-area : image -> number
;; function image-area,
;; which counts the number of pixels in a given image 
;; Examples and tests
 (begin-for-test
  (check-equal? 
   (image-area (bitmap "cat1.png")) 8775))
 
 (define (image-area i)
   (* (image-width i) (image-height i)))
 
  
 ;; q19
 ;; image-classify : Image -> String
 ;; image-area, which counts the number of pixels in a 
 ;; given image.
 ;; Examples and test
 (begin-for-test 
  (check-equal? (image-classify (square 50 "solid" "blue" )) "square")
  (check-equal? (image-classify (rectangle 50 30 "solid" "blue" )) "wide")
  (check-equal? (image-classify (rectangle 20 50 "solid" "blue" )) "tall")) 
  
 (define (image-classify img)
  (cond
    [(< (image-width img) (image-height img)) "tall" ]
    [(> (image-width img) (image-height img)) "wide" ]
    [(= (image-width img) (image-height img)) "square" ]))
 
 
 ;; q 20 
 ;; string-join : String String -> String
 ;; string-join, which consumes two strings and appends them with "_" 
 ;; in between.
 ;; Examples and tests
 
 (begin-for-test
   (check-equal? (string-join "demo" "test") "demo_test"))

 (define (string-join s1 s2)
  (string-append s1 "_" s2))
 

;; q21
;; string-insert : String Number -> String
;; string-insert, which consumes a string and a number i and
;; which inserts "_" at the ith position of the string.
;; Examples and tests
 (begin-for-test
   (check-equal? (string-insert "abcde" 2) "ab_cde"))
 
(define (string-insert str pos)
  (string-append (substring str 0 pos)
                 "_"
                 (substring str pos (string-length str))))

;; q22
;; string-delete : String -> String 
;; string-delete, which consumes a string and a number i 
;; and which deletes the ith position from str.
;; examples and test
(begin-for-test
  (string-delete "abcde" 2) "abde")

(define (string-delete str pos)
  (string-append (substring str 0 pos)
                 (substring str (+ pos 1) (string-length str))))