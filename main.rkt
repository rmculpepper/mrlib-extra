#lang racket/base
(require racket/class
         racket/match
         racket/gui/base
         )
(provide (all-defined-out))

(define TARGET-W 4)
(define TARGET-H 4)

(define resize-cursor (make-object cursor% 'size-nw/se))
(define grabbed-cursor resize-cursor)

;; resizable-editor-snip%
(define resizable-editor-snip%
  (class* editor-snip% ()
    (inherit get-flags set-flags)
    (super-new)
    (set-flags (append '(handles-events handles-all-mouse-events) (get-flags)))

    ;; dragging : #f or (cons Real Real)
    (define dragging #f)

    (define/private (get-lower-right-position [xadj 0] [yadj 0])
      (define owner (send (send this get-admin) get-editor))
      (define xb (box 0))
      (define yb (box 0))
      (send owner get-snip-location this xb yb #t)
      (values (- (unbox xb) xadj) (- (unbox yb) yadj)))

    ;; the snip's top-left corner is at (x, y) wrt the enclosing area (canvas)
    ;; the snip's top-left corner is at (edx, edy) wrt the enclosing editor
    ;; the mouse is currently at (mx, my)

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (super draw dc x y left top right bottom dx dy draw-caret)
      (when dragging
        (define saved-region (send dc get-clipping-region))
        (define saved-brush (send dc get-brush))
        (define saved-pen (send dc get-pen))
        (define-values (x2 y2) (values (car dragging) (cdr dragging)))
        (send dc set-clipping-region #f)
        (send dc set-brush "black" 'transparent)
        (send dc set-pen "gray" 2 'dot)
        (send dc draw-rectangle x y (- x2 x) (- y2 y))
        (send dc set-clipping-region saved-region)))

    (define/override (adjust-cursor dc x y edx edy event)
      (define (call-super) (super adjust-cursor dc x y edx edy event))
      (define mx (send event get-x))
      (define my (send event get-y))
      (define-values (x2 y2) (get-lower-right-position 0 0))
      (cond [(and dragging (send event dragging?))
             grabbed-cursor]
            [(and (<= (max x (- x2 TARGET-W)) mx x2)
                  (<= (max y (- y2 TARGET-H)) my y2))
             resize-cursor]
            [else (call-super)]))

    (define/override (on-event dc x y edx edy event)
      (define (call-super) (super on-event dc x y edx edy event))
      (define mx (send event get-x))
      (define my (send event get-y))
      (define-values (x2 y2) (get-lower-right-position 0 0))
      (eprintf "event; ed: ~s,~s type: ~s ~a\n" edx edy
               (send event get-event-type)
               (if (send event dragging?) "dragging" ""))
      (eprintf "  mouse at ~s,~s\n" mx my)
      (eprintf "  snip top-left ~s,~s bottom-right ~s,~s\n"
               x y x2 y2)
      (eprintf "  size min ~s,~s max ~s,~s\n"
               (send this get-min-width) (send this get-min-height)
               (send this get-max-width) (send this get-max-height))
      (cond [(and (eq? (send event get-event-type) 'left-down)
                  (<= (max x (- x2 TARGET-W)) mx x2)
                  (<= (max y (- y2 TARGET-H)) my y2))
             (eprintf "start drag\n")
             (set! dragging (cons mx my))
             #;(call-super)]
            [(eq? (send event get-event-type) 'leave)
             (set! dragging #f)
             #;(call-super)]
            [(and dragging (eq? (send event get-event-type) 'motion))
             (set! dragging (cons mx my))
             (send (send (send this get-admin) get-editor) invalidate-bitmap-cache
                   0 0 'display-end 'display-end)
             (call-super)]
            [(and dragging (eq? (send event get-event-type) 'left-up))
             (eprintf "end drag\n")
             (set! dragging #f)
             (send this resize (- mx x) (- my y))
             (void)]
            [else (call-super)]))
    ))

;; ;; clicky-snip%
;; (define clicky-snip%
;;   (class* editor-snip% ()
;;     (init-field [open-style '(border)]
;;                 [closed-style '(tight-text-fit)])
;;     (inherit set-margin
;;              set-inset
;;              set-snipclass
;;              set-tight-text-fit
;;              show-border
;;              get-admin)
;;     (define -outer (new text%))
;;     (super-new (editor -outer) (with-border? #f))
;;     (set-margin 2 2 2 2)
;;     (set-inset 2 2 2 2)
;;     ;;(set-margin 3 0 0 0)
;;     ;;(set-inset 1 0 0 0)
;;     ;;(set-margin 0 0 0 0)
;;     ;;(set-inset 0 0 0 0)
;;     (define/public (closed-contents) null)
;;     (define/public (open-contents) null)
;;     (define open? #f)
;;     (define/public (refresh-contents)
;;       (with-unlock -outer
;;         (send -outer erase)
;;         (do-style (if open? open-style closed-style))
;;         (outer:insert (if open? (hide-icon) (show-icon))
;;                       style:hyper
;;                       (if open?
;;                           (lambda _
;;                             (set! open? #f)
;;                             (refresh-contents))
;;                           (lambda _
;;                             (set! open? #t)
;;                             (refresh-contents))))
;;         (for-each (lambda (s) (outer:insert s))
;;                   (if open? (open-contents) (closed-contents)))
;;         (send -outer change-style top-aligned 0 (send -outer last-position))))
;;     (define/private (do-style style)
;;       (show-border (memq 'border style))
;;       (set-tight-text-fit (memq 'tight-text-fit style)))
;;     (define/private outer:insert
;;       (case-lambda
;;        [(obj)
;;         (if (styled? obj)
;;             (outer:insert (styled-contents obj)
;;                           (styled-style obj)
;;                           (styled-clickback obj))
;;             (outer:insert obj style:normal))]
;;        [(text style)
;;         (outer:insert text style #f)]
;;        [(text style clickback)
;;         (let ([start (send -outer last-position)])
;;           (send -outer insert text)
;;           (let ([end (send -outer last-position)])
;;             (send -outer change-style style start end #f)
;;             (when clickback
;;                   (send -outer set-clickback start end clickback))))]))
;;     (send -outer hide-caret #t)
;;     (send -outer lock #t)
;;     (refresh-contents)
;;     ))

(define (show-icon)
  (make-object image-snip%
    (collection-file-path "turn-up.png" "icons")))
(define (hide-icon)
  (make-object image-snip%
    (collection-file-path "turn-down.png" "icons")))

(define-syntax-rule (style-delta [command arg ...] ...)
  (let ([sd (make-object style-delta%)])
    (cond [(eq? 'command 'color)
           (send sd set-delta-foreground (car (list arg ...)))]
          [else
           (send sd set-delta 'command arg ...)])
    ...
    sd))

(define rule-sd (style-delta [change-bold] [color "blue"]))
(define error-sd (style-delta [change-italic] [color "red"]))
(define code-sd (style-delta [change-family 'modern]))
(define meta-code-sd (style-delta [change-family 'modern] [change-italic]))
(define meta-sd (style-delta [change-family 'modern] [color "blue"]
                             [change-bold] [change-bigger 2]))
(define hrule-sd (style-delta [change-size 4]))


;; ============================================================

(define f (new frame% (label "test") (height 400) (width 600)))
(define t (new text%))
(define ec (new editor-canvas% (editor t) (parent f)))
(send f show #t)

(send t insert "Here's what's I'm talking about:\n")

(define t2 (new text%))
(define es (new resizable-editor-snip% (editor t2)))
(send t2 insert "abcdefg hijklmno pqrstuv wxyz")
(send t insert es)

(send t2 hide-caret #t)
(send t2 lock #t)

(send t hide-caret #t)
(send t lock #t)

