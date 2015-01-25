;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
;; template for direction

(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

(provide initial-robot)
(provide robot-left)
(provide robot-right)
(provide robot-x)
;(provide robot-where)
(provide robot-y)
(provide robot-forward)

;(check-location "02" "robot.rkt")

;;;;;;;;;;;;;;;;;
(define RIGHT "right")
(define LEFT "left")
(define UP "up")
(define DOWN "down")
(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 400)
(define ROBOT-RIGHT-EDGE 185)
(define ROBOT-LEFT-TOP-EDGE 15)

(define ROBOT-BOTTOM-EDGE 385) 
(define RADIUS 15)

;;;;;;;;;;;;;;;
;; data definitions for the robot

(define-struct robot (x y direction))


;; An robot is (make-robot Coordinate Coordinate Direction)
;; Interp:
;; x is the x coordinate of the robot's position
;; y is the y coordinate of the robot's postion
;; Direction is the direction robot's face is facing

;;Template
;; robot-fn : Robot -> ??
(define (robot-fn r)
  (...
   (robot-x r)
   (robot-y r)
   (robot-direction r)))

;; Robot direction can be one of the following:
; - "right" (robot is facing right) 
;           (a robot moving "right" would increase it’s x position.)
; - "left"  (robot is facing left)  
;           (a robot moving "left" would decrease it’s x position.)
; - "up"    (robot is facing up)    
;           (a robot moving "up" would decrease it’s y position.)
; - "down"  (robot is facing down) 
;           (a robot moving "down" would increase it’s y position.)


; TEMPLATE
; direction-fn : Direction -> ???
;(define (direction-fn dir)
;  (cond
;    [(right? dir) ...]
;    [(left? dir) ...]
;    [(up? dir) ...]
;    [(right? dir) ...]))


; initial-robot : Coordinate Coordinate -> Robot
; Returns a Robot located at (x,y), facing up.
; STRATEGY: Function Composition
; Examples and Tests
(begin-for-test
  (check-equal? (initial-robot 30 20) (make-robot 30 20 "up"))
  "robot like r but with facing up")
;;;; STRATEGY: function composition  

(define (initial-robot x y)
  (make-robot x y "up"))


;; up?: Robot -> Boolean
;; whether the robot is facing the "up" direction.
;; EXAMPLES:
(begin-for-test
  (check-equal? (up? (make-robot 40 50 "up")) true)
  "true because robot is facing up")
;;;; STRATEGY: Data Decomposition
(define (up? robo)
  (equal? (robot-direction robo) "up"))

;;down?: Robot -> Boolean
;; whether the robot is facing the "down" direction.
;;EXAMPLES:
(begin-for-test
  (check-equal? (down? (make-robot 40 50 "down")) true)
  "true because robot is facing down")
;;;; STRATEGY: Data Decompistion

(define (down? robo)
  (equal? (robot-direction robo) "down"))


;;right?: Robot -> Boolean
;; whether the robot is facing the "right" direction.

;;EXAMPLES:
(begin-for-test
  (check-equal? (right? (make-robot 40 50 "right")) true)
  "true because robot is facing right")
;;;; STRATEGY: Data Decompistion

(define (right? robo)
  (equal? (robot-direction robo) "right"))

;;left?: Robot -> Boolean
;; whether the robot is facing the "left" direction.

;;EXAMPLES:
(begin-for-test
  (check-equal? (left? (make-robot 40 50 "left")) true)
  "true because robot is facing left")
;;;; STRATEGY: Data Decompistion

(define (left? robo)
  (equal? (robot-direction robo) "left"))



;; robot-left : Robot -> Robot
;; Returns a Robot like r, but turned 90 degrees
;;         left.

;;Tests and Examples
(begin-for-test 
  (check-equal? 
   (robot-left  (make-robot 90 100 "up")) (make-robot 90 100 "left")
   "a Robot like r, but turned either 90 degrees left")
  
  (check-equal? 
   (robot-left  (make-robot 90 100 "down")) (make-robot 90 100 "right")
   "a Robot like r, but turned either 90 degrees left")
  
  (check-equal? 
   (robot-left  (make-robot 90 100 "right")) (make-robot 90 100 "up")
   "a Robot like r, but turned either 90 degrees left")
  
  (check-equal?
   (robot-left  (make-robot 90 100 "left")) (make-robot 90 100 "down")
   "a Robot like r, but turned either 90 degrees left")
  
  (check-equal? 
   (robot-right (make-robot 90 100 "up")) (make-robot 90 100 "right")
   "a Robot like r, but turned either 90 degrees left")
  
  (check-equal? 
   (robot-right (make-robot 90 100 "down")) (make-robot 90 100 "left")
   "a Robot like r, but turned either 90 degrees left")
  
  (check-equal?
   (robot-right (make-robot 90 100 "right")) (make-robot 90 100 "down")
   "a Robot like r, but turned either 90 degrees left.")
  
  (check-equal? 
   (robot-right (make-robot 90 100 "left")) (make-robot 90 100 "up"))
   "a Robot like r, but turned either 90 degrees left.")

;; strategy : data decomposition on r : Robot
(define (robot-left r) 
  (cond
    [(up? r)   (make-robot (robot-x r) (robot-y r) "left")]
    [(down? r) (make-robot (robot-x r) (robot-y r) "right")]
    [(right? r)(make-robot (robot-x r) (robot-y r) "up")]
    [(left? r) (make-robot (robot-x r) (robot-y r) "down")]))



;; robot-right : Robot -> Robot
;; a robot like the original, but turned 90 degrees right.
;; Examples and tests
(begin-for-test 
  (check-equal?
   (robot-right (make-robot 100 40 RIGHT)) (make-robot 100 40 "down")
   "a Robot like r, but turned either 90 degrees right.")
  (check-equal?
   (robot-right (make-robot 100 40 LEFT)) (make-robot 100 40 "up"))
  "a Robot like r, but turned either 90 degrees right.")

;; Design strategy : Function Composition

(define (robot-right r)
  (cond
    [(up? r)    (make-robot (robot-x r) (robot-y r) "right")]
    [(down? r)  (make-robot (robot-x r) (robot-y r) "left")]
    [(right? r) (make-robot (robot-x r) (robot-y r) "down")]
    [(left? r)  (make-robot (robot-x r) (robot-y r) "up")]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; robot-goup : Robot -> Robot
;; a robot with moved up according to the required conditions.
;; Examples and tests
(begin-for-test
  (check-equal?
   (robot-goup (make-robot 100 200 UP) 1000) (make-robot 100 15 "up")
   "a Robot moved up with given distance or stopped at wall 
    if present position and distance would make it move outside canvas")
  
  (check-equal?
   (robot-goup (make-robot 100 200 UP) 10) (make-robot 100 190 "up"))
  "a Robot moved up with given distance or stopped at wall 
    if present position and distance would make it move outside canvas")

;; Design strategy : Function Composition
(define (robot-goup robo dis)
  (make-robot (robot-x robo)
              (max (- (robot-y robo) dis) ROBOT-LEFT-TOP-EDGE) "up"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; robot-godown : Robot -> Robot
;; RETURNS: a robot moved down according to the required conditions.
;; Examples and tests:
;(begin-for-test
;(check-equal?
;(robot-godown (make-robot 50 500 "down") 100) (make-robot 50 600 "down")
; "a Robot moved down with given distance or stopped at wall 
;;    if present position and distance would make it move outside canvas"

; (check-equal?
;  (robot-godown (make-robot 50 300 "down") 1000) (make-robot 50 385 "down"))
; "a Robot moved down with given distance or stopped at wall 
;   if present position and distance would make it move outside canvas" 


;; Design strategy : Function Composition
(define (robot-godown robo dis)
  (cond
    [(> (robot-y robo) ROBOT-BOTTOM-EDGE)
     (robot-increase-y robo dis)]
    
    [else 
     (move/stop-at-bottom-wall robo dis)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; robot-goleft : Robot -> Robot
;; RETURNS: a robot moving towards left with the required conditions.
;; Examples and tests
(begin-for-test
  (check-equal? 
   (robot-goleft (make-robot -100 50 LEFT) 10) (make-robot -110 50 "left")
   "a Robot moved left with given distance or stopped at wall 
    if present position and distance would make it move outside canvas")
  
  (check-equal? 
   (robot-goleft (make-robot 100 50 LEFT) 10)(make-robot 90 50 "left"))
  "a Robot moved left with given distance or stopped at wall 
    if present position and distance would make it move outside canvas")

;; Design strategy : Function Composition

(define (robot-goleft robo dis)
  (cond
    
    [(< (robot-x robo) ROBOT-LEFT-TOP-EDGE)
     (robot-decrease-x robo dis)]
    
    [(left? robo) 
     (move/stop-at-left-wall robo dis)])) 

;; CORRECT
(begin-for-test
  (check-equal? (move/stop-at-bottom-wall (make-robot 100 50 DOWN) 1000) 
                (make-robot 100 385 "down"))
  (check-equal? (move/stop-at-bottom-wall (make-robot 100 50 DOWN) 1000) 
                (make-robot 100 385 "down")))
;; strategy : CORRECT    
(define (move/stop-at-bottom-wall robo dis)
  (make-robot 
   (robot-x robo)(min (+ (robot-y robo) dis) ROBOT-BOTTOM-EDGE) "down"))



(define (move/stop-at-left-wall robo dis)
  (make-robot
   (max (- (robot-x robo) dis) ROBOT-LEFT-TOP-EDGE) (robot-y robo) "left"))

(define (robot-increase-y robo dis)
  (make-robot (robot-x robo) (+ (robot-y robo) dis) (robot-direction robo)))

(define (robot-decrease-x robo dis)
  (make-robot (- (robot-x robo) dis) (robot-y robo) (robot-direction robo)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; robot-goright : Robot -> Robot
;; RETURNS a robot moving towards right with the required conditions.
;; Examples and tests
(begin-for-test
  (check-equal?
   (robot-goright (make-robot 100 50 RIGHT) 20) (make-robot 120 50 "right"))
  "a Robot moved right with given distance or stopped at wall 
    if present position and distance would make it move outside canvas")

;; Design strategy : Function Composition

(define (robot-goright robo dis)
  (make-robot (min (+ (robot-x robo) dis) ROBOT-RIGHT-EDGE) 
              (robot-y robo) "right"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; vertical : Robot PosInt -> Robot
;; Returns A robot with the updated y coordinate after covering 
;; the required distance or being stopped at wall if distance 
;; to cover would make it outside canvas.
;; Examples and tests:
(begin-for-test
  (check-equal?
   (robot-vertical (make-robot 100 50 UP) 10)   (make-robot 100 40 "up")
   "robot being moved up with the given distance ")
  
  (check-equal?
   (robot-vertical (make-robot 100 50 DOWN) 10) (make-robot 100 60 "down"))
  "robot being moved down with the given distance")

;; Design Strategy : CORRECT

(define (robot-vertical robo dis)
  (cond 
    [(and 
      (and (< (robot-x robo) ROBOT-RIGHT-EDGE)
           (> (robot-x robo) ROBOT-LEFT-TOP-EDGE)) (up? robo))
     (robot-goup robo dis)]
    
    [(and 
      (and (< (robot-x robo) ROBOT-RIGHT-EDGE) 
           (> (robot-x robo) ROBOT-LEFT-TOP-EDGE)) (down? robo)) 
     (robot-godown robo dis)]
    
    [else (robot-outside robo dis)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; horizontal : Robot PosInt -> Robot
;; Returns A robot with the updated x coordinate after covering 
;; the required distance or being stopped at wall if distance 
;; to cover would make it outside canvas.
;; Examples and tests:
(begin-for-test
  (check-equal? 
   (robot-horizontal (make-robot 100 50 RIGHT ) 50) (make-robot 150 50 "right")
   "robot being moved down with the given distance")
  
  (check-equal? 
   (robot-horizontal (make-robot 100 50 LEFT ) 50) (make-robot 50 50 "left"))
  "robot being moved down with the given distance") 

;; Design Strategy : Function Composition

(define (robot-horizontal robo dis)
  (cond 
    [(and 
      (and (< (robot-y robo) ROBOT-BOTTOM-EDGE) 
           (> (robot-y robo) ROBOT-LEFT-TOP-EDGE)) (right? robo)) 
     (robot-goright robo dis)]
    
    [(and 
      (and (< (robot-y robo) ROBOT-BOTTOM-EDGE)
           (> (robot-y robo) ROBOT-LEFT-TOP-EDGE)) (left? robo))
     (robot-goleft robo dis)]
    
     
    [else (robot-outside robo dis)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; outside-room-position : Robot PosInt  -> Robot
;; Return : A robot (which is completely outside the canvas
;; moved after covering the required distance.
;; Examples and tests            
(begin-for-test
  (check-equal? 
   (robot-outside (make-robot -10 2 "up") 20)(make-robot -10 -18 "up")
   "robot with new position after moved by distance 'dis'")
  
  (check-equal? 
   (robot-outside (make-robot 400 500 "left") 10)(make-robot 390 500 "left"))
  "robot with new position after moved by distance 'dis'")

;; Design strategy : CORRECT

(define (robot-outside robo dis)
  (cond
    [(up? robo) 
     (make-robot (robot-x robo) (- (robot-y robo) dis) (robot-direction robo))]
    [(down? robo)
     (make-robot (robot-x robo) (+ (robot-y robo) dis) (robot-direction robo))]
    [(right? robo)
     (make-robot (+ (robot-x robo) dis) (robot-y robo) (robot-direction robo))]
    [(left? robo) 
     (make-robot (- (robot-x robo) dis) (robot-y robo) (robot-direction robo))]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; robot-forward : Robot NonNegReal -> Robot
; Returns a Robot like r, but moved forward by d pixels.  
; If the robot is inside the room and moving would put any part of the
; robot outside the room, the robot should stop at the wall that it's facing.
; Examples and Tests

(begin-for-test
  (check-equal? (robot-forward (make-robot 100 100 "up") 50)  
                (make-robot 100 50 "up")
                "robot moved up with 'd' distance") 
  (check-equal? (robot-forward (make-robot 100 100 "up") 300) 
                (make-robot 100 15 "up")
                "robot moved up with 'd' distance but stopped at wall")
  
  ;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (check-equal? 
   (robot-forward (make-robot 100 100 "down") 50)  (make-robot 100 150 "down")
   "robot moved down with 'd' distance")
  (check-equal? 
   (robot-forward (make-robot 100 100 "down") 400) (make-robot 100 385 "down")
   "robot moved down but stopped at wall")
;  (check-equal? 
;   (robot-forward (make-robot 100 100 "down") 285) (make-robot 100 385 "down"))
;  (check-equal?
;   (robot-forward (make-robot 10  100 "down") 400) (make-robot 10 500 "down"))
;  (check-equal?
;   (robot-forward (make-robot 15  15 "down")  390) (make-robot 15 405 "down"))
;  (check-equal?
;   (robot-forward (make-robot 100 14 "down")  50)  (make-robot 100 64 "down"))
  ;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (check-equal?
   (robot-forward (make-robot 100 100 "right") 50) (make-robot 150 100 "right")
   "robot moved right with 'd' distance")
  (check-equal? 
   (robot-forward (make-robot 100 100 "right") 100)(make-robot 185 100 "right")
   "robot moved right but stopped at wall")
  ;(check-equal?
   ;(robot-forward (make-robot 100 100 "right") 85)(make-robot 185 100 "right"))
  ;(check-equal? 
  ;(robot-forward (make-robot 200 100 "right") 85) (make-robot 285 100 "right"))
  ;(check-equal?
   ;(robot-forward (make-robot 15 385 "right") 85) (make-robot 100 385 "right"))
  ;(check-equal?
   ;(robot-forward (make-robot 0 200 "right") 85)  (make-robot 85 200 "right"))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (check-equal? 
   (robot-forward (make-robot 100 100 "left") 50)   (make-robot 50 100 "left"))
  ;(check-equal?
  ;(robot-forward (make-robot 100 100 "left") 100)  (make-robot 15 100 "left")) 
  ;(check-equal?
  ;(robot-forward (make-robot 100 100 "left") 150)  (make-robot 15 100 "left"))
  (check-equal?
   (robot-forward (make-robot 200 100 "left") 10)   (make-robot 190 100 "left"))
  ;(check-equal?
  ;(robot-forward (make-robot 185 100 "left") 100)  (make-robot 85 100 "left"))
  ;(check-equal?
  ;(robot-forward (make-robot 15 100 "left") 20)    (make-robot -5 100 "left"))
  ;(check-equal?
  ; (robot-forward (make-robot 300 125 "left") 1000) (make-robot 15 125 "left"))
  (check-equal?
   (robot-forward (make-robot -500 -725 "up") 100) 
   (make-robot -500 -825 "up")))


; Strategy: Function Composition

(define (robot-forward robo d)
  (cond
    [(or  (up? robo) (down? robo))    (robot-vertical robo d)]
    [(or  (left? robo) (right? robo)) (robot-horizontal robo d)]))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



