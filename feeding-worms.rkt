#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

(define-struct worm [dir head body])
; Worm is a structure:
;  (make-worm Direction Posn [List-of Posn])
; Direction is either:
; - "up"
; - "down"
; - "left"
; - "right"

(define-struct world [worm fruit])
; World is a structure:
;  (make-world Worm Posn)

;; CONSTANTS

(define BOARD-SIZE 20)
(define CELL-SIZE 20)
(define OFFSET-X (/ CELL-SIZE 2))
(define OFFSET-Y (/ CELL-SIZE 2))

(define VALID-RANGE (range 0 BOARD-SIZE 1))
(define VALID-POSNS
  (foldr
   (lambda (i acc)
     (append (foldr
              (lambda (j acc)
                (cons (make-posn i j) acc))
              '()
              VALID-RANGE)
             acc))
   '()
   VALID-RANGE))

(define BG-CLR (color 240 240 240))
(define CELL-CLR (color 212 212 212))
(define WORM-CLR (color 77 77 77))
(define FRUIT-CLR (color 235 64 52))

(define TICK-RATE 0.15)

(define CELL
  (circle (quotient CELL-SIZE 8) "solid" CELL-CLR)) 
(define WORM-HEAD
  (circle (quotient CELL-SIZE 2) "solid" WORM-CLR))
(define WORM-SEGMENT
  (circle (quotient CELL-SIZE 3) "solid" WORM-CLR))
(define FRUIT
  (circle (quotient CELL-SIZE 3) "solid" FRUIT-CLR))

(define BOARD
  (empty-scene
   (* BOARD-SIZE CELL-SIZE)
   (* BOARD-SIZE CELL-SIZE)
   BG-CLR))


;; DATA EXAMPLES

(define w01
  (make-worm
   "right"
   (make-posn 0 0)
   '()))

(define w02
  (make-worm
   "right"
   (make-posn 1 0)
   '()))

(define w11
  (make-worm
   "down"
   (make-posn 0 0)
   (list (make-posn 1 0) (make-posn 2 0))))

(define w12
  (make-worm
   "down"
   (make-posn 0 1)
   (list (make-posn 0 0) (make-posn 1 0))))

(define invalid-w0
  (make-worm
   "left"
   (make-posn -1 0)
   '()))

(define invalid-w1
  (make-worm
   "down"
   (make-posn 5 BOARD-SIZE)
   '()))

(define invalid-w2
  (make-worm
   "down"
   (make-posn 5 5)
   (list (make-posn 5 4)
         (make-posn 5 3)
         (make-posn 6 3)
         (make-posn 7 3)
         (make-posn 7 4)
         (make-posn 7 5)
         (make-posn 6 5)
         (make-posn 5 5)
         (make-posn 4 5))))

;; MAIN FUNCTION

; N N -> World
; creates world worm programm with the worm
; head initialized at the given coordinates
; assumption x0 and y0 are declared in relative units
(define (run-worm x0 y0)
  (local
    ((define worm0 (make-worm "down" (make-posn x0 y0) '()))
     (define fruit0 (spawn-fruit worm0))
     (define world0 (make-world worm0 fruit0))
     (define board (create-board BOARD-SIZE BOARD-SIZE))
     ; World -> Image
     (define (draw ws)
       (local
         ((define w (world-worm ws))
          (define f (world-fruit ws)))
         (draw-worm+scene
          w
          (draw-fruit+scene f board))))
     ; World KeyEvent -> World
     (define (ke-handler ws k)
       (cond
         [(arrow-key? k)
          (local
            ((define w (world-worm ws))
             (define f (world-fruit ws)))
            (make-world (next-dir w k) f))]
         [else ws]))
     ; World -> World
     (define (tock ws)
       (local
         ((define w (world-worm ws))
          (define f (world-fruit ws)))
         (cond
           [(can-eat? w f)
            (local
              ((define next-w (eat w f)))
              (make-world
               next-w
               (spawn-fruit next-w)))]
           [else
            (make-world
             (move-worm w) f)])))
     ; World -> Boolean
     (define (game-over? ws)
       (local
         ((define w (world-worm ws)))
         (or (out-of-bounds? w)
             (hit-self? w))))
     ; World Clause -> World
     (define final-world
       (big-bang world0
         [on-tick tock TICK-RATE]
         [on-key ke-handler]
         [to-draw draw]
         [stop-when game-over?]))
     (define final-worm (world-worm final-world))
     (define final-body (worm-body final-worm)))
    (string-append
     "GAME OVER!!! YOUR SCORE: "
     (number->string (add1 (length final-body)))
     "!!!")))

;; HELPERS

; Posn Image Image -> Image
; draws the given image on the scene
; at the given position (in relative units)

(define (draw-img+scene p im sc)
  (local
   ((define abs-p (rel->abs p))
    (define abs-x (posn-x abs-p))
    (define abs-y (posn-y abs-p)))
   (place-image im abs-x abs-y sc)))

; Posn Image -> Image
; draws a fruit on the scene at the posn

(define (draw-fruit+scene p sc)
  (draw-img+scene p FRUIT sc))

; Worm Image -> Image
; draws the worm head on the given scene

(define (draw-worm+scene w sc)
  (local
    ((define head (worm-head w))
     (define body (worm-body w)))
    (draw-head+scene
     head
     (draw-body+scene body sc))))

; Posn Image -> Image
; draws the worm head on the given scene
; at the given posn

(define (draw-head+scene p sc)
  (draw-img+scene p WORM-HEAD sc))

; [List-of Posn] Image -> Image
; draws the worm body on the given scene
; at the given posn

(define (draw-body+scene alop sc0)
  (foldr
   (lambda (p sc) (draw-img+scene p WORM-SEGMENT sc))
   sc0
   alop))

; N N -> Image
; creates board image from the given
; width and height
; assumption w and h are declared in relative units

(define (create-board w h)
  (local
    ((define board0
       (empty-scene
        (* w CELL-SIZE)
        (* h CELL-SIZE)
        BG-CLR)))
    (foldr
     (lambda (i outer-acc)
       (foldr
        (lambda (j inner-acc)
          (draw-img+scene
           (make-posn i j)
           CELL
           inner-acc))
        outer-acc
        (range 0 w 1)))
     board0
     (range 0 h 1))))

; Worm -> Boolean
; determines whether the worm hits itself

(check-expect (hit-self? w11) #false)
(check-expect (hit-self? invalid-w2) #true)

(define (hit-self? w)
  (local
    ((define head (worm-head w))
     (define body (worm-body w)))
    (member? head body)))

; Worm -> Boolean
; determines whether the worm is out of board bound

(check-expect (out-of-bounds? w01) #false)
(check-expect (out-of-bounds? invalid-w0) #true)
(check-expect (out-of-bounds? invalid-w1) #true)

(define (out-of-bounds? w)
  (local
    ((define head (worm-head w))
     (define x (posn-x head))
     (define y (posn-y head)))
    (or
     (< x 0) (>= x BOARD-SIZE)
     (< y 0) (>= y BOARD-SIZE))))

; Worm -> Posn
; generates such new fruit posn that
; is not collide with the worm

(check-satisfied
 (spawn-fruit w11)
 (lambda (p) (not-collide-worm? p w11)))

(define (spawn-fruit w)
  (local
    ((define free-posns
       (filter
        (lambda (p) (not-collide-worm? p w))
        VALID-POSNS))
     (define n (length free-posns))
     (define i (random n)))
    (list-ref free-posns i)))

; Posn Worm -> Boolean
; determines whether the given posn colliding with the worm

(check-expect (not-collide-worm? (make-posn 0 0) w01)
              #false)
(check-expect (not-collide-worm? (make-posn 1 1) w01)
              #true)
(check-expect (not-collide-worm? (make-posn 1 0) w11)
              #false)
(check-expect (not-collide-worm? (make-posn 5 5) w11)
              #true)

(define (not-collide-worm? p w)
  (local
    ((define head (worm-head w))
     (define body (worm-body w)))
    (not (or (equal? p head) (member? p body)))))

; Worm Posn -> Worm
; eats the fruit by placing its posn on the worm head

(check-expect (eat w01 (make-posn 1 0))
              (make-worm
               (worm-dir w01)
               (make-posn 1 0)
               (cons (worm-head w01)
                     (worm-body w01))))

(define (eat w f)
  (local
    ((define dir (worm-dir w))
     (define head (worm-head w))
     (define body (worm-body w)))
    (make-worm dir f (cons head body))))

; Worm Posn -> Boolean
; determines whether the worm is about to eat a fruit

(check-expect (can-eat? w01 (make-posn 0 1))
              #false)
(check-expect (can-eat? w01 (make-posn 1 0))
              #true)

(define (can-eat? w f)
  (local
    ((define next-head (update-head w)))
    (equal? next-head f)))

; Worm -> Worm
; moves worm to the next position

(check-expect (move-worm w01) w02)
(check-expect (move-worm w11) w12)

(define (move-worm w)
  (local
    ((define dir (worm-dir w)))
    (make-worm dir (update-head w) (update-body w))))

; Worm -> Posn
; updates worm head to place it on the next posn

(check-expect (update-head w01)
              (make-posn 1 0))

(define (update-head w)
  (local
    ((define dir (worm-dir w))
     (define head (worm-head w))
     (define mask (create-mask dir)))
    (posn-add head mask)))

; Worm -> [List-of Posn]
; updates worm body to place it on the next posn

(check-expect (update-body w01)
              '())
(check-expect (update-body w11)
              (list
               (make-posn 0 0)
               (make-posn 1 0)))

(define (update-body w)
  (local
    ((define head (worm-head w))
     (define body (worm-body w))
     (define head+body (cons head body)))
    (remove-last head+body)))

; Worm Direction -> Worm
; updates the worm direction according to the given one
; ignores new direction if it's opposite to the current one

(check-expect (next-dir w01 "down")
              (make-worm "down"
                         (worm-head w01)
                         (worm-body w01)))
(check-expect (next-dir w01 "left") w01)

(define (next-dir w new-dir)
  (local
    ((define dir (worm-dir w))
     (define head (worm-head w))
     (define body (worm-body w)))
    (cond
      [(opposite? dir new-dir) w]
      [else (make-worm new-dir head body)])))

; Direction Direction -> Boolean
; determines whether dir1 is the opposite of dir2

(check-expect (opposite? "up" "down") #true)
(check-expect (opposite? "down" "up") #true)
(check-expect (opposite? "left" "right") #true)
(check-expect (opposite? "right" "left") #true)
(check-expect (opposite? "up" "left") #false)
(check-expect (opposite? "right" "right") #false)

(define (opposite? dir1 dir2)
  (local
    ((define mask1 (create-mask dir1))
     (define mask2 (create-mask dir2))
     (define mask+ (posn-add mask1 mask2)))
    (= 0 (posn-x mask+) (posn-y mask+))))

; Direction -> Posn
; creates mask according to the given posn

(check-expect (create-mask "up")
              (make-posn 0 -1))
(check-expect (create-mask "down")
              (make-posn 0 1))
(check-expect (create-mask "left")
              (make-posn -1 0))
(check-expect (create-mask "right")
              (make-posn 1 0))

(define (create-mask dir)
  (cond
    [(string=? dir "up") (make-posn 0 -1)]
    [(string=? dir "down") (make-posn 0 1)]
    [(string=? dir "left") (make-posn -1 0)]
    [(string=? dir "right") (make-posn 1 0)]))

; Posn Posn -> Posn
; adds up posn by their coordinates

(check-expect (posn-add (make-posn 1 2)
                        (make-posn 3 5))
              (make-posn 4 7))

(define (posn-add a b)
  (make-posn
   (+ (posn-x a) (posn-x b))
   (+ (posn-y a) (posn-y b))))

; Posn -> Posn
; converts the given position in relative units to absolute units

(check-expect (rel->abs (make-posn 0 0))
              (make-posn OFFSET-X OFFSET-Y))
(check-expect (rel->abs (make-posn 1 1))
              (make-posn (+ CELL-SIZE OFFSET-X)
                         (+ CELL-SIZE OFFSET-Y)))

(define (rel->abs p)
  (local
    ((define x (posn-x p))
     (define y (posn-y p)))
    (make-posn
     (+ (* x CELL-SIZE) OFFSET-X)
     (+ (* y CELL-SIZE) OFFSET-Y))))

; KeyEvent -> Boolean
; determines whether the pressed key is an arrow key

(check-expect (arrow-key? "up") #true)
(check-expect (arrow-key? "down") #true)
(check-expect (arrow-key? "left") #true)
(check-expect (arrow-key? "right") #true)
(check-expect (arrow-key? "a") #false)

(define (arrow-key? k)
  (or (key=? k "up") (key=? k "down")
      (key=? k "left") (key=? k "right")))

; [X] [NEList-of X] -> [List-of X]
; removes the last item from a non-empty list

(check-expect (remove-last '(1)) '())
(check-expect (remove-last '(1 2)) '(1))

(define (remove-last l)
  (cond
    [(empty? (rest l)) '()]
    [else
     (cons (first l) (remove-last (rest l)))]))