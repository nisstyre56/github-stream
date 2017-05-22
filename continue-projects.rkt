#lang racket
(require net/url)
(require json)
(require net/head)
(require srfi/41)

(define (get-next link)
  (let* ([splitted (string-split link ";")]
         [url (substring (car splitted)
                         1
                         (sub1 (string-length (car splitted))))]
         [query (make-hash
                 (url-query
                  (string->url url)))])
    (string->number
     (hash-ref query 'since 1))))

(define (get-project-names since)
  (define resp
    (get-impure-port
    (string->url
     (format
      "https://api.github.com/repositories?since=~a"
      since))))
  
  (define headers
    (make-hash
     (extract-all-fields
      (purify-port resp))))

  (cond
    [(string=?
      (hash-ref headers "Status" "")
      "403 Forbidden")
     (stream)]
    
    [else
     (stream-concat
      (stream
       (list->stream
        (filter
         (compose not (curry equal? 'null))
         (map
          (lambda (p)
            (hash-ref p 'description))
          (read-json resp))))
       
       (get-project-names
        (get-next (hash-ref headers "Link")))))]))

(define
  projects-stream
  (get-project-names 1))