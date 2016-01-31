#lang racket/base
(require racket/class
         racket/gui/base)
(provide (all-defined-out))

(define-syntax-rule (style-delta [command arg ...] ...)
  (let ([sd (make-object style-delta%)])
    (cond [(eq? 'command 'color)
           (send sd set-delta-foreground (car (list arg ...)))]
          [else
           (send sd set-delta 'command arg ...)])
    ...
    sd))

(define (text:insert t s
                     #:style [styles null]
                     #:clickback [clickback #f])
  (define start (send t get-start-position))
  (send t insert s)
  (define end (send t get-start-position))
  (let ([styles (cond [(list? styles) styles]
                      [(eq? styles #f) null]
                      [else (list styles)])])
    (for ([style (in-list styles)])
      (send t change-style style start end #f)))
  (when clickback
    (send t set-clickback start end clickback)))

(define (text:insertf t
                      #:style [styles null]
                      #:clickback [clickback #f]
                      fmt . args)
  (text:insert t (apply format fmt args) #:style styles #:clickback clickback))

;; with-unlock SYNTAX (expression)
;; (with-unlock text-expression . body)
(define-syntax with-unlock
  (syntax-rules ()
    [(with-unlock text . body)
     (let* ([t text]
            [locked? (send t is-locked?)])
       (dynamic-wind
         (lambda ()
           (send* t
             (begin-edit-sequence #f)
             (lock #f)))
         (lambda () . body)
         (lambda ()
           (send* t
             (lock locked?)
             (end-edit-sequence)))))]))
