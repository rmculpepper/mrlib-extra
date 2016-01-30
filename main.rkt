#lang racket/base
(require racket/class
         racket/match
         racket/gui/base)
(provide (all-defined-out))

(define TARGET-W 4)
(define TARGET-H 4)

(define resize-n-cursor (make-object cursor% 'size-n/s))
(define resize-e-cursor (make-object cursor% 'size-e/w))
(define resize-nw-cursor (make-object cursor% 'size-nw/se))
(define resize-ne-cursor (make-object cursor% 'size-ne/sw))

;; A DragState is (list DragSym Real Real)
;; A DragSym is one of 'w, 'h, 'wh
;; where 'w means adjust width only, etc

(define drag-state%
  (class object%
    (init-field type x1 y1 x2 y2)
    (define minx x1)
    (define miny y1)
    (define maxx x2)
    (define maxy y2)
    (super-new)

    (define/public (call proc)
      (proc x1 y1 x2 y2))

    (define/public (update mx my)
      (case type
        [(w nw sw) (set! x1 mx) (set! minx (min minx mx))]
        [(e ne se) (set! x2 mx) (set! maxx (max maxx mx))])
      (case type
        [(n nw ne) (set! y1 my) (set! miny (min miny my))]
        [(s sw se) (set! y2 my) (set! maxy (max maxy my))]))

    (define/public (draw-box dc)
      (define w (- x2 x1))
      (define h (- y2 y1))
      (when (and (> w 0) (> h 0))
        (call/save-dc-state dc
          (lambda ()
            (send dc set-clipping-region #f)
            (send dc set-brush "black" 'transparent)
            (send dc set-pen "red" 1 'dot)
            (send dc draw-rectangle x1 y1 w h)))))

    (define/public (refresh editor)
      ;; This doesn't quite work: leaves artifacts along edges
      ;; (let ([minx (max (- minx 2) 0)] [miny (max (- miny 2) 0)]
      ;;       [maxx (+ maxx 2)] [maxy (+ maxy 2)])
      ;;   (send editor invalidate-bitmap-cache minx miny maxx maxy))
      (send editor invalidate-bitmap-cache 0 0 'display-end 'display-end))

    (define/public (get-cursor)
      (case type
        [(n s) resize-n-cursor]
        [(e w) resize-e-cursor]
        [(ne sw) resize-ne-cursor]
        [(nw se) resize-nw-cursor]
        [else (error 'get-cursor "bad type: ~e" type)]))
    ))

;; resizable-editor-snip%
(define resizable-editor-snip%
  (class* editor-snip% ()
    (inherit get-extent get-editor get-margin get-admin
             resize get-flags set-flags)
    (super-new)
    (set-flags (append '(handles-events handles-all-mouse-events) (get-flags)))

    ;; dragging : #f or DragState
    (define dragging #f)

    ;; the snip's top-left corner is at (x, y) wrt the enclosing area (canvas)
    ;; the snip's top-left corner is at (edx, edy) wrt the enclosing editor
    ;; the mouse is currently at (mx, my)

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (super draw dc x y left top right bottom dx dy draw-caret)
      (when dragging (send dragging draw-box dc)))

    (define/override (adjust-cursor dc x y edx edy event)
      (define (call-super) (super adjust-cursor dc x y edx edy event))
      (define mx (send event get-x))
      (define my (send event get-y))
      (define-values (x2 y2) (get-lower-right-position dc x y))
      (cond [dragging (send dragging get-cursor)]
            [else
             (case (get-edge/corner x y x2 y2 mx my)
               [(nw se) resize-nw-cursor]
               [(ne sw) resize-ne-cursor]
               [(n s) resize-n-cursor]
               [(e w) resize-e-cursor]
               [else (call-super)])]))

    (define/override (on-event dc x y edx edy event)
      (define (call-super) (super on-event dc x y edx edy event))
      (define-values (mx my) (values (send event get-x) (send event get-y)))
      (define-values (x2 y2) (get-lower-right-position dc x y))
      (debug-mouse-event dc x y edx edy event)
      (define event-type (send event get-event-type))
      (when (and dragging (eq? event-type 'motion))
        (send dragging update mx my)
        (send dragging refresh (get-owner-editor)))
      (when (eq? event-type 'leave)
        (set! dragging #f))
      (cond [(eq? event-type 'left-down)
             (cond [(get-edge/corner x y x2 y2 mx my)
                    => (lambda (where)
                         (define d
                           (new drag-state% (type where) (x1 x) (y1 y) (x2 x2) (y2 y2)))
                         (send d update mx my)
                         (set! dragging d))]
                   [else (call-super)])]
            [(and dragging (eq? event-type 'left-up))
             (let ([d dragging])
               (set! dragging #f)
               (send d call (lambda (x1 y1 x2 y2) (do-resize d x1 y1 x2 y2))))]
            [else (call-super)]))

    (define/private (do-resize dragging x1 y1 x2 y2)
      (define w (- x2 x1))
      (define h (- y2 y1))
      (resize w h)
      ;; Re-adjust the editor's width because resize doesn't un-off-by-1
      (define-values (lm tm rm bm) (get-margin*))
      (send* (get-editor)
        [set-min-width (- w lm rm -1)]
        [set-max-width (- w lm rm -1)])
      ;; FIXME: indicate whether editor is completely displayed
      (unless (editor-is-completely-displayed? (- w lm rm -1) (- h tm bm))
        (eprintf "editor is not completely displayed\n"))
      ;; FIXME: Updating whole display is antisocial ...
      (send dragging refresh (get-owner-editor)))

    (define/public (get-edge/corner x1 y1 x2 y2 mx my)
      (define on-e? (<= (max x1 (- x2 TARGET-W)) mx x2))
      (define on-s? (<= (max y1 (- y2 TARGET-H)) my y2))
      (define on-w? (<= x1 mx (min (+ x1 TARGET-W) x2)))
      (define on-n? (<= y1 my (min (+ y1 TARGET-H) y2)))
      (cond [on-e? (cond [on-s? 'se] [on-n? 'ne] [else 'e])]
            [on-w? (cond [on-s? 'sw] [on-n? 'nw] [else 'w])]
            [on-n? 'n]
            [on-s? 's]
            [else #f]))

    (define/public (editor-is-completely-displayed? w h)
      (define editor (get-editor))
      (define last-pos (send editor last-position))
      (define xb (box 0))
      (define yb (box 0))
      (send editor position-location last-pos xb yb #f)
      (eprintf "editor interior: ~s x ~s; last position at ~s, ~s\n"
               w h (unbox xb) (unbox yb))
      (define complete? (and (<= (unbox xb) w) (<= (unbox yb) h)))
      complete?)

    (define/public (get-margin*)
      (define lb (box 0)) (define tb (box 0)) (define rb (box 0)) (define bb (box 0))
      (get-margin lb tb rb bb)
      (values (unbox lb) (unbox tb) (unbox rb) (unbox bb)))

    (define/private (get-owner-editor)
      (define admin (get-admin))
      (and admin (send admin get-editor)))

    (define/private (get-lower-right-position dc x y)
      (define wb (box 0))
      (define hb (box 0))
      (get-extent dc x y wb hb)
      (values (+ x (unbox wb)) (+ y (unbox hb))))

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

