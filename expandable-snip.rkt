#lang racket/base
(require racket/class
         racket/match
         racket/gui/base
         mrlib/include-bitmap
         "util.rkt")
(provide (all-defined-out))

(define turn-snip%
  (class snip%
    (inherit get-admin get-flags set-flags)
    (init-field state ; (U 'up 'down 'up-click 'down-click)
                on-up
                on-down)
    (super-new)
    (set-flags (cons 'handles-events (get-flags)))

    (define/override (copy)
      (instantiate turn-snip% ()
        (on-up on-up)
        (on-down on-down)
        (state state)))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define bitmap
        (case state
          [(up) up-bitmap]
          [(down) down-bitmap]
          [(up-click) up-click-bitmap]
          [(down-click) down-click-bitmap]))
      (cond [(send bitmap ok?)
             (send dc draw-bitmap bitmap x y)]
            [(send dc draw-rectangle x y 10 10)
             (send dc drawline x y 10 10)]))

    (define/override (get-extent dc x y w h descent space lspace rspace)
      (set-box/f! descent 0)
      (set-box/f! space 0)
      (set-box/f! lspace 0)
      (set-box/f! rspace 0)
      (set-box/f! w arrow-snip-width)
      (set-box/f! h arrow-snip-height))

    (define/override (on-event dc x y editorx editory evt)
      (let ([snip-evt-x (- (send evt get-x) x)]
            [snip-evt-y (- (send evt get-y) y)])
        (cond
         [(send evt button-down? 'left)
          (set-state (case state
                       [(up) 'up-click]
                       [(down) 'down-click]
                       [else 'down-click]))]
         [(and (send evt button-up? 'left)
               (<= 0 snip-evt-x arrow-snip-width)
               (<= 0 snip-evt-y arrow-snip-height))
          (set-state (case state
                       [(up up-click) 
                        (on-down)
                        'down]
                       [(down down-click)
                        (on-up)
                        'up]
                       [else 'down]))]
         [(send evt button-up? 'left)
          (set-state (case state
                       [(up up-click) 'up]
                       [(down down-click) 'down]
                       [else 'up]))]
         [(and (send evt get-left-down)
               (send evt dragging?)
               (<= 0 snip-evt-x arrow-snip-width)
               (<= 0 snip-evt-y arrow-snip-height))
          (set-state (case state
                       [(up up-click) 'up-click]
                       [(down down-click) 'down-click]
                       [else 'up-click]))]
         [(and (send evt get-left-down)
               (send evt dragging?))
          (set-state (case state
                       [(up up-click) 'up]
                       [(down down-click) 'down]
                       [else 'up-click]))]
         [else
          (super on-event dc x y editorx editory evt)])))

    (define/private (set-state new-state)
      (unless (eq? state new-state)
        (set! state new-state)
        (let ([admin (get-admin)])
          (when admin
            (send admin needs-update this 0 0 arrow-snip-width arrow-snip-height)))))

    (define/override (adjust-cursor dc x y editorx editory event)
      arrow-snip-cursor)
    ))

(define (set-box/f! b v) (when (box? b) (set-box! b v)))
(define down-bitmap (include-bitmap (lib "icons/turn-down.png") 'png))
(define up-bitmap (include-bitmap (lib "icons/turn-up.png") 'png))
(define down-click-bitmap (include-bitmap (lib "icons/turn-down-click.png") 'png))
(define up-click-bitmap (include-bitmap (lib "icons/turn-up-click.png") 'png))

(define arrow-snip-height
  (max 10
       (send up-bitmap get-height)
       (send down-bitmap get-height)
       (send up-click-bitmap get-height)
       (send down-click-bitmap get-height)))
(define arrow-snip-width
  (max 10
       (send up-bitmap get-width)
       (send down-bitmap get-width)
       (send up-click-bitmap get-width)
       (send down-click-bitmap get-width)))
(define arrow-snip-cursor (make-object cursor% 'arrow))

;; ============================================================

;; clicky-snip%
(define clicky-snip%
  (class* editor-snip% ()
    (inherit show-border
             get-editor
             get-admin)
    (init [open-editor (new text%)]
          [closed-editor (new text%)])
    (init-field [open-callback void]
                [closed-callback void])
    (super-new)

    (define open? #f)

    (define open-es (new editor-snip% (editor open-editor) (with-border? #f)))
    (send open-es set-margin 0 0 0 0)
    (send open-es set-inset 0 0 0 0)

    (define closed-es (new editor-snip% (editor closed-editor) (with-border? #f)))
    (send closed-es set-margin 0 0 0 0)
    (send closed-es set-inset 0 0 0 0)

    (let ([outer-t (get-editor)])
      (send outer-t insert
            (new turn-snip%
                 [state 'up]
                 [on-up (λ () (show-closed-contents))]
                 [on-down (λ () (show-open-contents))]))
      (send outer-t hide-caret #t)
      (send outer-t lock #t))

    (define/public (get-open-editor) (send open-es get-editor))
    (define/public (get-closed-editor) (send closed-es get-editor))

    (define (get-open-state) open?)
    (define (set-open-state v) (if v (show-open-contents) (show-closed-contents)))

    ;; The editor contents are 0[turn-snip]1[open/closed-editor-snip]2.

    (define/private (show-closed-contents [force? #f])
      (define outer-t (get-editor))
      (when (or open? force?)
        (set! open? #f)
        (with-unlock outer-t
          (send outer-t release-snip open-es)
          (send outer-t delete 1 2 #f)
          (send outer-t insert closed-es 1)
          (reset-top-alignment))
        (closed-callback this)))

    (define/private (show-open-contents)
      (define outer-t (get-editor))
      (unless open?
        (set! open? #t)
        (with-unlock outer-t
          (send outer-t release-snip closed-es)
          (send outer-t delete 1 2 #f)
          (send outer-t insert open-es 1)
          (reset-top-alignment))
        (open-callback this)))

    (define/private (reset-top-alignment)
      (define t (get-editor))
      (send t change-style top-aligned 0 (send t last-position)))

    (show-closed-contents #t)
    ))

(define top-aligned (style-delta [change-alignment 'top]))

;; ============================================================

(define f (new frame% (label "test") (height 400) (width 600)))
(define t (new text%))
(define ec (new editor-canvas% (editor t) (parent f)))
(send f show #t)

(send t insert "Here's what's I'm talking about,\na nice clicky snip: ")

(define es (new clicky-snip% (with-border? #f)))
(send es set-margin 0 0 0 0)

(send (send es get-closed-editor) insert "alphabet")
(send (send es get-open-editor) insert "abcdefg\nhijklmno\npqrstuv\nwxyz")

(send t insert es)
(send t hide-caret #t)
(send t lock #t)
