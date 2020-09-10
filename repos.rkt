#lang racket

(require racket/date
         racket/serialize)

(provide (all-defined-out))

(struct machine
  (id ;; some uuid
   name))

(serializable-struct repo
  (name
   url
   remotes
   commit
   location
   notes)
  #:mutable
  #:transparent)

(serializable-struct remote
  (name
   url)
  #:transparent)

(struct location
  (path
   machine))

(serializable-struct collection
  (name
   last-updated
   repos)
  #:transparent)


(define (get-git-remotes)
  (let* ([remote-names (git "remote")]
         [h (for/hash ([r (string-split remote-names)])
              (values r (remote r (string-trim (git/format "remote get-url ~a" r)))))])
    h))
         
(define (git command)
  (with-output-to-string (lambda () (system (string-append "git " command) ))))

(define (git/format command ...args)
  (git (format command ...args)))

(define (make-git-repo root-folder repo-dotgit-path)
  (define repo-folder (simple-form-path (path-only (build-path repo-dotgit-path 'up))))
  (parameterize
      ([current-directory repo-folder])
    (let* ([folder-name (path->string (car (reverse (explode-path repo-folder))))]
           [remotes (get-git-remotes)]
           [origin (hash-ref remotes 'origin #f)]
           [commit (string-trim (git "rev-parse --verify HEAD"))])
      (repo folder-name origin remotes commit (find-relative-path root-folder repo-folder) #f))))
         

(define (get-git-repositories root-folder)
  (let ([collection-name (path->string (car (reverse (explode-path root-folder))))])
    (collection
     collection-name
     (current-date)
     (for/fold
      ([repos '()])
      ([repo-path (in-directory root-folder)] #:when (regexp-match? "\\.git$" repo-path))
       (cons (make-git-repo root-folder repo-path) repos)))))

(module+ main
  (define col (get-git-repositories "C:\\Users\\andre\\prog\\racket"))
  (with-output-to-file
      (build-path "C:\\Users\\andre\\prog\\racket" "repos.rkt")
    (lambda () (write col))))