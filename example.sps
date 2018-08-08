#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; R6RS Scheme version of example.c.
#!r6rs

(import
  (rnrs)
  (only (srfi :13 strings) string-prefix?)
  (linenoise))

(when (file-exists? "history.txt")
  (linenoise-history-load "history.txt"))

(linenoise-set-completion-callback
 (lambda (line add-completion add-history-completions)
   (add-history-completions)
   (when (string-prefix? "h" line)
     (add-completion "hello こんにちは")
     (add-completion "hello こんにちは there"))))

(linenoise-set-hints-callback
 (lambda (line)
   (cond ((string-ci=? "hello" line)
          (values " World" 'magenta #f))
         ((string-ci=? "こんにちは" line)
          (values " 世界" 'magenta #f))
         (else
          (values #f #f #f)))))

(for-each
 (lambda (arg)
   (cond ((string=? "--multiline" arg)
          (linenoise-set-multi-line #t))
         ((string=? "--keycodes" arg)
          (linenoise-print-key-codes))
         (else
          (display "Usage: example.sps [--multiline] [--keycodes]\n"
                   (current-error-port))
          (exit 1))))
 (cdr (command-line)))

(define (main)
  (let ((line (linenoise "\x1b;[32mこんにちは\x1b;[0m> ")))
    (unless (eof-object? line)
      (cond
        ((and (not (string=? line "")) (not (string-prefix? "/" line)))
         (display "echo: ")
         (write line)
         (newline)
         (linenoise-history-add line)
         (linenoise-history-save "history.txt"))
        ((string-prefix? "/historylen" line)
         (let ((len (string->number
                     (substring line 12 (string-length line)))))
           (linenoise-history-set-max-len len)))
        ((string-prefix? "/" line)
         (display "Unrecognized command: ")
         (display line)
         (newline)))
      (main))))

(main)
