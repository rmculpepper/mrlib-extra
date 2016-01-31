#lang racket/base
(require racket/class
         racket/match
         racket/gui/base
         mrlib/include-bitmap
         "util.rkt"
         "private/turn-snip.rkt")
(provide (all-defined-out))

;; two-state-snip%
(define two-state-snip%
  (class editor-snip%
    (inherit get-editor
             get-admin)
    (init-field [open-callback void]
                [closed-callback void])
    (super-new)

    (field [open? #f])

    (let ([outer-t (get-editor)])
      (send outer-t insert
            (new turn-snip%
                 [state 'up]
                 [on-up (λ () (closed-callback))]
                 [on-down (λ () (open-callback))]))
      (send outer-t change-style top-aligned 0 (send t last-position))
      (send outer-t hide-caret #t)
      (send outer-t lock #t))
    ))

;; clicky-snip%
(define clicky-snip%
  (class two-state-snip%
    (inherit get-editor
             get-admin)
    (inherit-field open?)
    (init [open-editor (new text%)]
          [closed-editor (new text%)]
          [open-callback void]
          [closed-callback void])

    (field [open-es (new editor-snip% (editor open-editor) (with-border? #f))])
    (send open-es set-margin 0 0 0 0)
    (send open-es set-inset 0 0 0 0)

    (field [closed-es (new editor-snip% (editor closed-editor) (with-border? #f))])
    (send closed-es set-margin 0 0 0 0)
    (send closed-es set-inset 0 0 0 0)

    (super-new [open-callback
                (lambda () (and (show-open-contents) (open-callback)))]
               [closed-callback
                (lambda () (and (show-closed-contents) (closed-callback)))])

    (define/public (get-open-editor) (send open-es get-editor))
    (define/public (get-closed-editor) (send closed-es get-editor))

    (define (get-open-state) open?)
    (define (set-open-state v) (if v (show-open-contents) (show-closed-contents)))

    ;; The editor contents are 0[turn-snip]1[open/closed-editor-snip]2.

    (define/private (show-closed-contents [force? #f])
      (define outer-t (get-editor))
      (cond [(or open? force?)
             (set! open? #f)
             (with-unlock outer-t
               (send outer-t release-snip open-es)
               (send outer-t delete 1 2 #f)
               (send outer-t insert closed-es 1))
             #t]
            [else #f]))

    (define/private (show-open-contents)
      (define outer-t (get-editor))
      (cond [(not open?)
             (set! open? #t)
             (with-unlock outer-t
               (send outer-t release-snip closed-es)
               (send outer-t delete 1 2 #f)
               (send outer-t insert open-es 1))
             #t]
            [else #f]))

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
