#!/usr/bin/guile-2.0 \
-e main -s
!#

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (ice-9 receive))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9 gnu))
(use-modules (srfi srfi-26))


(define (tokenize)
 (let ((line (read-line)))
  (if (eof-object? line)
    line
    (string-split line #\space))))

(define (backtrace address size frames) `(backtrace ,address ,size ,frames))


(define (display-backtrace backtrace)
 (match backtrace
  (('backtrace address size frames)
   (format #t "~A: ~A bytes\n" address size)
   (for-each (cut format #t "~A\n" <>) frames))))


(define (parse-fold proc seed)
  (define (index? str)
   (string-prefix? "#" str))

 (let lp ((current-address #f)
          (current-size #f)
          (current-frames '())
          (tokens (tokenize))
          (seed seed))
  (match tokens
   ((? eof-object? eof) seed)

   ((size "bytes" "at" address)
    (lp address size '() (tokenize)
     (if current-address
      (proc (backtrace current-address current-size (reverse current-frames)) seed)
      seed)))

   (((? index? _) function "at" location)
    (lp current-address current-size
     (cons (cons function location) current-frames) (tokenize) seed))

   (("Backtrace" "end")
    (let ((new-seed (proc
                     (backtrace current-address current-size (reverse current-frames))
                     seed)))
    (lp #f #f '() (tokenize) new-seed)))

   (((or "[GC:" "FullCollection," "EddenCollection,") args ...)
    ; these messages tend to be "out of line", so we "eat" them here
    (lp current-address current-size current-frames args seed))

   (_ ; ignore everything else
    (lp current-address current-size current-frames (tokenize) seed)))))
    
(define (parse backtrace-action)
 (parse-fold
  (lambda (bt seed) (backtrace-action bt))
  #f))

(define (fold-backtraces proc seed)
 (let lp ((seed seed))
  (let ((backtrace (read)))
   (if (eof-object? backtrace)
    seed
    (lp (proc backtrace seed))))))

(define (parse-dat backtrace-action)
 (fold-backtraces
  (lambda (backtrace seed) (backtrace-action backtrace))
  #f))

; (vector node location total hits children)
(define (insert-backtrace root reversed-frames alloc-size)
 "For a backtrace represented by REVERSED-FRAMES (frames from bottom to top), insert
 it in the children of the tree ROOT, considering that it did allocate
 ALLOC-SIZE.
 This procedure is not tail-recursive, but you shouldn't have that many frames
 anyway, right?"
 (define (node location total hits children) (vector 'node location total hits children))

 (define (new-subtree frames)
  (match frames
   (() '())
   ((bottom-frame other-frames ...)
    (list (node bottom-frame alloc-size 1 (new-subtree other-frames))))))

 (define (find-matching-child loc children)
  (define (valid-child? child)
   (equal? (vector-ref child 1) loc))

  (receive (matching other-children)
   (partition valid-child? children)
   (match matching
    (() (values #f children))
    ((matching-child) (values matching-child other-children))
    (_ (error "More than one child with matching location!")))))

 (match root

  (#('node loc total hits ()) ; no children
   (node loc (+ total alloc-size) (1+ hits) (new-subtree reversed-frames)))

  (#('node loc total hits children)
   (match reversed-frames
    (() (node loc total hits children))
    ((bottom-frame other-frames ...)
     (node loc (+ total alloc-size) (1+ hits)
      (receive (child more-children)
       (find-matching-child bottom-frame children)
       (if child
        (cons (insert-backtrace child other-frames alloc-size) more-children)
        (append (new-subtree reversed-frames) children)))))))))

(define (make-tree fold-proc)
 (fold-proc
  (lambda (backtrace root)
   (match backtrace
    (('backtrace _ sizestr frames)
     (insert-backtrace root (reverse frames) (string->number sizestr)))))
  (vector 'node #f 0 0 '())))

(define (print-tree root)
 (define (compare-nodes node1 node2)
  (match (cons node1 node2)
   ((#('node _ total1 _ _) . #('node _ total2 _ _))
    (>= total1 total2))))

 (define (print-node node depth)
  (let print-prefix ((depth depth))
   (cond
    ((> depth 1)
     (display "| ")
     (print-prefix (1- depth)))
    ((equal? depth 1)
     (display "->"))
    (else (error "Invalid depth in print-prefix"))))
  (match node
   (#('node (function . loc) total hits _)
    (format #t "~A bytes in ~A calls ~A (~A)\n" total hits function loc))))

 (let lp ((root root) (depth 0))
  (match root
   (#('node #f total hits children) (format #t "Total allocations: ~A bytes\n" total))
   (node (print-node node depth)))

  (match root
   (#('node _ _ _ children)
    (for-each
     (lambda (child) (lp child (1+ depth)))
     (sort children compare-nodes))))))


(define (main args)
 (match args

  ((_ "show-backtraces" filename)
   (with-input-from-file filename (lambda () (parse display-backtrace))))

  ((_ "write-backtraces" logfile outfile)
   (with-output-to-file outfile
    (lambda ()
     (with-input-from-file logfile (lambda () (parse write))))))

  ((_ "show-data" datafile)
   (with-input-from-file datafile (lambda () (parse-dat display-backtrace))))

  ((_ "print-tree" datafile)
   (with-input-from-file datafile (lambda () (print-tree (make-tree fold-backtraces)))))

  ((_ "print-tree-from-log" logfile)
   (with-input-from-file logfile (lambda () (print-tree (make-tree parse-fold)))))



  ((cl . _)
   (format (current-error-port) "usage ~A COMMAND ARG...\n" cl)
   (format (current-error-port) "Available commands\n")
   (format (current-error-port) "  show-backtraces LOGFILE\n")
   (format (current-error-port) "  write-backtraces LOGFILE OUTDATAFILE\n")
   (format (current-error-port) "  show-data DATAFILE\n")
   (format (current-error-port) "  print-tree DATAFILE\n")
   (format (current-error-port) "  print-tree-from-log LOGFILE\n")
   (exit 1))))
