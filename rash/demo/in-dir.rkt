#lang racket/base

(provide in-dir)

(require rash
         shell/utils/bourne-expansion-utils
         (for-syntax
          racket/base
          syntax/parse))

(define-line-macro in-dir
  (syntax-parser
    [(_ dirs:id body)
     #`(in-dir (values #,(dollar-expand-syntax #'dirs #:glob-expand? #t)) body)]
    [(_ dirs:str body)
     #`(in-dir (values #,(dollar-expand-syntax #'dirs #:glob-expand? #t)) body)]
    [(_ dirs body)
     #`(let* ([edirs dirs]
              [err (λ (p) (error 'in-dir "directory doesn't exist: ~a" p))]
              [do-body (λ (d)
                         (define dp (if (not (path-string? d))
                                        (format "~a" d)
                                        d))
                         (parameterize
                             ([current-directory (if (directory-exists? dp)
                                                     dp
                                                     (err dp))])
                           body))])
         (if (list? edirs)
             (for/list ([d edirs])
               (do-body d))
             (do-body edirs)))]))