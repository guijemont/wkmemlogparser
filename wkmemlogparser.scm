#!/usr/bin/guile-2.0 \
-e main -s
!#

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (srfi srfi-9 gnu))
(use-modules (srfi srfi-26))


(define (tokenize)
 (let ((line (read-line)))
  (if (eof-object? line)
    line
    (string-tokenize line))))

(define-immutable-record-type <backtrace>
 (backtrace address size frames)
 backtrace?
 (address backtrace-address backtrace-set-address)
 (size backtrace-size backtrace-set-size)
 (frames backtrace-frames backtrace-set-frames))

(define (display-backtrace backtrace)
 (match backtrace
  (($ <backtrace> address size frames)
   (format #t "~A: ~A bytes\n" address size)
   (for-each (cut format #t "~A\n" <>) frames))))

(define (backtrace-sexpr backtrace)
 (match backtrace
  (($ <backtrace> address size frames)
   `(backtrace ,address ,size ,frames))))

(define (backtrace-from-sexpr sexpr)
 (match sexpr
  (('backtrace address size frames)
   (backtrace address size frames))))


(define (parse backtrace-action)
  (define (index? str)
   (string-prefix? "#" str))

 (let lp ((current-address #f)
          (current-size #f)
          (current-frames '())
          (tokens (tokenize)))
  (match tokens
   ((? eof-object? eof) *unspecified*)

   ((size "bytes" "at" address)
    (and current-address
     (backtrace-action
      (backtrace current-address current-size (reverse current-frames))))
    (lp address size '() (tokenize)))

   (((? index? _) function "at" location)
    (lp current-address current-size
     (cons (cons function location) current-frames) (tokenize)))

   (("Backtrace" "end")
    (backtrace-action
     (backtrace current-address current-size (reverse current-frames)))
    (lp #f #f '() (tokenize)))

   (((or "[GC:" "FullCollection," "EddenCollection,") args ...)
    ; these messages tend to be "out of line", so we "eat" them here
    (lp current-address current-size current-frames args))

   (_ ; ignore everything else
    (lp current-address current-size current-frames (tokenize))))))
    



(define (main args)
 (match args

  ((_ "show-backtraces" filename)
   (with-input-from-file filename (lambda () (parse display-backtrace))))

  ((_ "write-backtraces" logfile outfile)
   (with-output-to-file outfile
    (lambda ()
     (with-input-from-file logfile (lambda () (parse (compose write backtrace-sexpr)))))))

  ((_ "show-data" datafile)
   (define (read-and-display)
     (let ((sexpr (read)))
      (and (not (eof-object? sexpr))
       (begin
        (display-backtrace (backtrace-from-sexpr sexpr))
        (read-and-display)))))
   (with-input-from-file datafile read-and-display))


  ((cl . _)
   (format (current-error-port) "usage ~A COMMAND ARG...\n" cl)
   (format (current-error-port) "Available commands\n")
   (format (current-error-port) "  show-backtraces LOGFILE\n")
   (format (current-error-port) "  write-backtraces LOGFILE OUTDATAFILE\n")
   (format (current-error-port) "  show-data DATAFILE\n")
   (exit 1))))
