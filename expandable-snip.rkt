#lang racket/base
(require racket/class
         racket/match
         racket/gui/base
         "util.rkt")
(provide (all-defined-out))

;; clicky-snip%
(define clicky-snip%
  (class* editor-snip% ()
    (init-field [open-style '(border align-top-line)]
                [closed-style '(tight-text-fit)]
                [update-callback void])
    (inherit set-margin
             set-inset
             set-tight-text-fit
             set-align-top-line
             show-border
             get-editor
             get-admin)

    (super-new (with-border? #f))
    (set-margin 5 2 5 2)
    (set-inset 2 2 2 2)
    (send (get-editor) set-styles-sticky #f)

    ;;(set-margin 3 0 0 0)
    ;;(set-inset 1 0 0 0)
    ;;(set-margin 0 0 0 0)
    ;;(set-inset 0 0 0 0)

    (define open? #f)

    (define/public (refresh-contents)
      (define t (get-editor))
      (with-unlock t
        (send t erase)
        (restyle-snip (if open? open-style closed-style))
        (text:insert t (if open? (hide-icon) (show-icon))
                     #:style style:hyper
                     #:clickback (lambda _ (set! open? (not open?)) (refresh-contents)))
        (update-callback (get-editor) open?)
        (when #f
          (send t change-style top-aligned 0 (send t last-position)))))

    (define/private (restyle-snip style)
      (show-border (memq 'border style))
      (set-tight-text-fit (memq 'tight-text-fit style))
      (set-align-top-line (memq 'align-top-line style)))

    (send* (get-editor)
      [hide-caret #t]
      [lock #t])
    (refresh-contents)
    ))

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

(define top-aligned (style-delta [change-alignment 'top]))
(define style:hyper (style-delta [change-toggle-underline] [color "blue"]))

;; ============================================================

(define f (new frame% (label "test") (height 400) (width 600)))
(define t (new text%))
(define ec (new editor-canvas% (editor t) (parent f)))
(send f show #t)

(send t insert "Here's what's I'm talking about,\na nice clicky snip: ")

(define es (new clicky-snip%
                (update-callback
                 (lambda (t2 open?)
                   (if open?
                       (send t2 insert "abcdefg\nhijklmno\npqrstuv\nwxyz")
                       (send t2 insert "alphabet"))))))

(send t insert es)
(send t hide-caret #t)
(send t lock #t)
