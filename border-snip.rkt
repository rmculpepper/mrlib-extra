#lang racket/base
(require racket/class
         racket/match
         racket/gui/base)
(provide oxford-brackets-border-snip-mixin)

(define (replace-border-snip-mixin %)
  (class %
    (init-field [with-alt-border? #t])
    (super-new (with-border? #f))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (super draw dc x y left top right bottom dx dy draw-caret)
      (when with-alt-border?
        (draw-alt-border dc x y left top right bottom)))

    (abstract draw-alt-border)
    ))

(define (oxford-brackets-border-snip-mixin %)
  (class (replace-border-snip-mixin %)
    (inherit get-extent get-inset)
    (super-new)

    (define/override (draw-alt-border dc x y left top right bottom)
      (define-values (li ti ri bi) (get-inset*))
      (define-values (x2 y2) (get-lower-right-position dc x y))
      (draw-brackets dc (+ x li) (+ y ti) (- x2 ri) (- y2 bi)))

    (define/public (draw-brackets dc x1 y1 x2 y2)
      (define W 2)
      (define WW 8)
      (send* dc
        (draw-line (+ x1 0) y1 (+ x1 0) y2)
        (draw-line (+ x1 W) y1 (+ x1 W) y2)
        (draw-line (- x2 W) y1 (- x2 W) y2)
        (draw-line (- x2 0) y1 (- x2 0) y2)
        (draw-line x1 y1 (+ x1 WW) y1)
        (draw-line x1 y2 (+ x1 WW) y2)
        (draw-line x2 y1 (- x2 WW) y1)
        (draw-line x2 y2 (- x2 WW) y2)))

    (define/private (get-inset*)
      (define lb (box 0)) (define tb (box 0)) (define rb (box 0)) (define bb (box 0))
      (get-inset lb tb rb bb)
      (values (unbox lb) (unbox tb) (unbox rb) (unbox bb)))

    (define/private (get-lower-right-position dc x y)
      (define wb (box 0))
      (define hb (box 0))
      (get-extent dc x y wb hb)
      (values (+ x (unbox wb)) (+ y (unbox hb))))
    ))
