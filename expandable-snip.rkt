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
    (init-field [open/close-callback void])
    (super-new)

    (let ([outer-t (get-editor)])
      (send* outer-t
        [insert (new turn-snip%
                     [state 'up]
                     [on-up (λ () (open/close-callback #f))]
                     [on-down (λ () (open/close-callback #t))])]
        [change-style top-aligned 0 (send outer-t last-position)]
        [hide-caret #t]
        [lock #t]))
    ))

;; expandable-snip%
(define expandable-snip%
  (class two-state-snip%
    (inherit get-editor
             get-admin)
    (init [closed-editor (new text%)]
          [open-editor (new text%)]
          [open/close-callback void])
    (init-field [layout 'replace]) ;; (U 'replace 'append)

    (field [open? #f])

    (field [open-es (new editor-snip% (editor open-editor) (with-border? #f))])
    (send open-es set-margin 0 0 0 0)
    (send open-es set-inset 0 0 0 0)

    (field [closed-es (new editor-snip% (editor closed-editor) (with-border? #f))])
    (send closed-es set-margin 0 0 0 0)
    (send closed-es set-inset 0 0 0 0)

    (super-new [open/close-callback
                (lambda (now-open?)
                  (unless (eq? now-open? open?)
                    (set! open? now-open?)
                    (refresh-contents)
                    (open/close-callback this now-open?)))])

    (define/public (get-open-editor) (send open-es get-editor))
    (define/public (get-closed-editor) (send closed-es get-editor))

    (define (get-open-state) open?)
    (define (set-open-state v)
      (let ([v (and v #t)])
        (unless (eq? open? v)
          (set! open? v)
          (refresh-contents))))

    ;; if layout is 'replace, editor contains
    ;;  - open? = #f : [turn-snip][closed-es]
    ;;  - open? = #t : [turn-snip][open-es]
    ;; if layout is 'append, editor contains
    ;;  - open? = #f : [turn-snip][closed-es]
    ;;  - open? = #t : [turn-snip][closed-es]\n[open-es]

    (define/private (refresh-contents)
      (define outer-t (get-editor))
      (with-unlock outer-t
        (send outer-t release-snip closed-es)
        (send outer-t release-snip open-es)
        (send outer-t delete 1 (send outer-t last-position))
        (when (or (not open?) (eq? layout 'append))
          (send outer-t insert closed-es (send outer-t last-position)))
        (when (and open? (eq? layout 'append))
          (send outer-t insert "\n" (send outer-t last-position)))
        (when open?
          (send outer-t insert open-es (send outer-t last-position)))))

    (refresh-contents)
    ))

(define top-aligned (style-delta [change-alignment 'top]))

;; ============================================================

(define f (new frame% (label "test") (height 400) (width 600)))
(define t (new text%))
(define ec (new editor-canvas% (editor t) (parent f)))
(send f show #t)

(send t insert "Here's what's I'm talking about,\na nice clicky snip: ")

(require "resizable-snip.rkt")
(define clicky-snip% (resizable-editor-snip-mixin expandable-snip%))

(define es (new clicky-snip% (with-border? #t) (layout 'replace)))
;(send es set-margin 0 0 0 0)

(send* (send es get-closed-editor)
  [insert "alphabet"]
  ;; [hide-caret #t]
  [lock #t])

(send* (send es get-open-editor)
  [insert "abcdefg\nhijklmno\npqrstuv\nwxyz"]
  ;; [hide-caret #t]
  [lock #t])

(send t insert es)
(send t hide-caret #t)
(send t lock #t)
