#lang racket/base
(require racket/class
         racket/match
         racket/gui/base)
(provide (all-defined-out))

(define TARGET-W 4)
(define TARGET-H 4)

(define resize-wh-cursor (make-object cursor% 'size-nw/se))
(define resize-w-cursor (make-object cursor% 'size-e/w))
(define resize-h-cursor (make-object cursor% 'size-n/s))

;; A DragState is (list DragSym Real Real)
;; A DragSym is one of 'w, 'h, 'wh
;; where 'w means adjust width only, etc

;; resizable-editor-snip%
(define resizable-editor-snip%
  (class* editor-snip% ()
    (inherit get-extent get-editor resize get-flags set-flags)
    (super-new)
    (set-flags (append '(handles-events handles-all-mouse-events) (get-flags)))

    ;; dragging : #f or DragState
    (define dragging #f)

    (define/private (get-lower-right-position/old dc x y)
      (define owner (send (send this get-admin) get-editor))
      (define xb (box 0))
      (define yb (box 0))
      (send owner get-snip-location this xb yb #t)
      (values (unbox xb) (unbox yb)))

    (define/private (get-lower-right-position dc x y)
      (define wb (box 0))
      (define hb (box 0))
      (get-extent dc x y wb hb)
      (values (+ x (unbox wb)) (+ y (unbox hb))))

    ;; the snip's top-left corner is at (x, y) wrt the enclosing area (canvas)
    ;; the snip's top-left corner is at (edx, edy) wrt the enclosing editor
    ;; the mouse is currently at (mx, my)

    (define MAGIC-X 0 #;6)
    (define MAGIC-Y 0 #;5)

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (super draw dc x y left top right bottom dx dy draw-caret)
      (match dragging
        [(list drag-sym x2 y2)
         (define w (- x2 x))
         (define h (- y2 y))
         (when (and (> w 0) (> h 0))
           (call/save-dc-state dc
             (lambda ()
               (send dc set-clipping-region #f)
               (send dc set-brush "black" 'transparent)
               (send dc set-pen "red" 1 'dot)
               (send dc draw-rectangle x y w h))))]
        [#f (void)]))

    (define/override (adjust-cursor dc x y edx edy event)
      (define (call-super) (super adjust-cursor dc x y edx edy event))
      (define mx (send event get-x))
      (define my (send event get-y))
      (define-values (x2 y2) (get-lower-right-position dc x y))
      (match dragging
        [(list 'w _ _) resize-w-cursor]
        [(list 'h _ _) resize-h-cursor]
        [(list 'wh _ _) resize-wh-cursor]
        [#f 
         (define-values (drag-w? drag-h?) (get-drag-w/h? x y x2 y2 mx my))
         (cond [(and drag-w? drag-h?) resize-wh-cursor]
               [drag-w? resize-w-cursor]
               [drag-h? resize-h-cursor]
               [else (call-super)])]))

    (define/override (on-event dc x y edx edy event)
      (define (call-super) (super on-event dc x y edx edy event))
      (define-values (mx my) (values (send event get-x) (send event get-y)))
      (define-values (x2 y2) (get-lower-right-position dc x y))
      (debug-mouse-event dc x y edx edy event)
      (define event-type (send event get-event-type))
      (when (eq? event-type 'motion)
        (dragging:update mx my))
      (when (eq? event-type 'leave)
        (set! dragging #f))
      (cond [(eq? event-type 'left-down)
             (define-values (drag-w? drag-h?) (get-drag-w/h? x y x2 y2 mx my))
             (cond [(and drag-w? drag-h?)
                    (set! dragging (list 'wh mx my))]
                   [drag-w?
                    (set! dragging (list 'w mx y2))]
                   [drag-h?
                    (set! dragging (list 'h x2 my))]
                   [else (call-super)])]
            [(and dragging (eq? event-type 'left-up))
             (dragging:resize x y dragging)
             (set! dragging #f)]
            [else (call-super)]))

    (define/public (dragging:resize x y dragging)
      (match dragging
        [(list _ nx ny)
         (define w (- nx x))
         (define h (- ny y))
         (resize w h)
         ;; Re-adjust the editor's width because resize doesn't un-off-by-1
         (send* (get-editor)
           [set-min-width (+ 1 w)]
           [set-max-width (+ 1 w)])
         ;; FIXME: Updating whole display is antisocial ...
         (send (send (send this get-admin) get-editor) invalidate-bitmap-cache
               0 0 'display-end 'display-end)]
        [#f (void)]))

    (define/public (get-drag-w/h? x y x2 y2 mx my)
      (define drag-w? (<= (max x (- x2 TARGET-W)) mx x2))
      (define drag-h? (<= (max y (- y2 TARGET-H)) my y2))
      (values drag-w? drag-h?))

    (define/public (dragging:update mx my)
      (when dragging
        (set! dragging (dragging:update-state dragging mx my))
        ;; FIXME: Updating whole display is antisocial ...
        (send (send (send this get-admin) get-editor) invalidate-bitmap-cache
              0 0 'display-end 'display-end)))

    (define/public (dragging:update-state dragging mx my)
      (match dragging
        [(list 'w  _  y2)
         (list 'w  mx y2)]
        [(list 'h  x2 _ )
         (list 'h  x2 my)]
        [(list 'wh _  _ )
         (list 'wh mx my)]
        [#f #f]))

    (define/private (debug-mouse-event dc x y edx edy event)
      (unless (eq? (send event get-event-type) 'motion)
        (define mx (send event get-x))
        (define my (send event get-y))
        (define-values (x2 y2) (get-lower-right-position dc x y))
        (eprintf "event; ed: ~s,~s type: ~s ~a\n" edx edy
                 (send event get-event-type)
                 (if (send event dragging?) "dragging" ""))
        (eprintf "  mouse at ~s,~s\n" mx my)
        (eprintf "  snip top-left ~s,~s bottom-right ~s,~s\n"
                 x y x2 y2)
        (eprintf "  size min ~s,~s max ~s,~s\n"
                 (send this get-min-width) (send this get-min-height)
                 (send this get-max-width) (send this get-max-height))))
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

(define (call/save-dc-state dc proc)
  (define saved-region (send dc get-clipping-region))
  (define saved-brush (send dc get-brush))
  (define saved-pen (send dc get-pen))
  (begin0 (proc)
    (send dc set-clipping-region saved-region)
    (send dc set-brush saved-brush)
    (send dc set-pen saved-pen)))

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

