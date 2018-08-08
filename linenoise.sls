;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2018 Göran Weinholt <goran@weinholt.se>

;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;; * Redistributions of source code must retain the above copyright notice,
;;   this list of conditions and the following disclaimer.

;; * Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#!r6rs

(library (linenoise)
  (export
    linenoise

    linenoise-set-completion-callback
    linenoise-set-hints-callback

    linenoise-history-add
    linenoise-history-set-max-len
    linenoise-history-save
    linenoise-history-load

    linenoise-clear-screen
    linenoise-set-multi-line
    linenoise-print-key-codes)
  (import
    (rnrs)
    (pffi)
    (only (pffi procedure) lookup-shared-object))

(define lib (open-shared-object "liblinenoise.so"))

(define (c-string->string ptr)
  (utf8->string
   (call-with-bytevector-output-port
     (lambda (p)
       (let lp ((i 0))
         (let ((c (pointer-ref-c-uint8 ptr i)))
           (unless (fxzero? c)
             (put-u8 p c)
             (lp (fx+ i 1)))))))))

(define (string->c-string str)
  (string->utf8 (string-append str "\x0;")))

;; Present a prompt to the user and return the entered data.
(define linenoise
  (let ((%linenoise (foreign-procedure lib pointer linenoise (pointer)))
        (%linenoiseFree (foreign-procedure lib void linenoiseFree (pointer))))
    (lambda (prompt)
      (let ((line-ptr (%linenoise (bytevector->pointer (string->c-string prompt)))))
        (if (zero? (pointer->integer line-ptr))
            (eof-object)
            (let ((line (c-string->string line-ptr)))
              (%linenoiseFree line-ptr)
              line))))))

;; Scheme callback should return three values:
;; * hint string (or #f)
;; * color symbol (or #f)
;; * bold bool
(define linenoise-set-hints-callback
  (let ((%linenoiseSetHintsCallback
         (foreign-procedure lib void linenoiseSetHintsCallback
                            ((callback pointer (pointer pointer pointer))))))
    (lambda (callback)
      (%linenoiseSetHintsCallback
       (c-callback pointer ((pointer buf) (pointer *color) (pointer *bold))
         (lambda (buf *color *bold)
           (let-values (((hint color bold?) (callback (c-string->string buf))))
             (when color
               (let ((c (if (number? color)
                            color
                            (case color
                              ((black) 30)
                              ((red) 31)
                              ((green) 32)
                              ((brown) 33)
                              ((blue) 34)
                              ((magenta) 35)
                              ((cyan) 36)
                              ((white) 37)
                              (else
                               (assertion-violation 'linenoise-set-hints-callback
                                                    "Unknown color" color))))))
                 (pointer-set-c-int! *color 0 c)))
             (when bold?
               (pointer-set-c-int! *bold 0 1))
             (if hint
                 (bytevector->pointer (string->c-string hint))
                 (integer->pointer 0)))))))))

;; TODO: Needs dynamic-wind, or should be a parameter? Either way it's
;; not thread-safe.
(define linenoise-set-completion-callback
  (let ((%linenoiseSetCompletionCallback
         (foreign-procedure lib void linenoiseSetCompletionCallback
                            ((callback void (pointer pointer)))))
        (%linenoiseAddCompletion
         (foreign-procedure lib void linenoiseAddCompletion
                            (pointer pointer)))
        (%linenoiseAddHistoryCompletions
         (foreign-procedure lib void linenoiseAddHistoryCompletions
                            (pointer pointer))))
    (lambda (callback)
      (%linenoiseSetCompletionCallback
       (c-callback void ((pointer buf) (pointer lc))
         (lambda (buf lc)
           (let ((line (c-string->string buf)))
             (callback line
                       (lambda (completion)
                         (let ((str (bytevector->pointer
                                     (string->c-string completion))))
                           (%linenoiseAddCompletion lc str)))
                       (lambda ()
                         (%linenoiseAddHistoryCompletions buf lc))))))))))

(define linenoise-history-add
  (let ((%linenoiseHistoryAdd
         (foreign-procedure lib int linenoiseHistoryAdd (pointer))))
    (lambda (line)
      (let ((status (%linenoiseHistoryAdd (bytevector->pointer (string->c-string line)))))
        ;; #t if the line was added, otherwise #f (perhaps due to a duplicate line)
        (= status 1)))))

(define linenoise-history-set-max-len
  (let ((%linenoiseHistorySetMaxLen
         (foreign-procedure lib int linenoiseHistorySetMaxLen (int))))
    (lambda (len)
      (let ((status (%linenoiseHistorySetMaxLen len)))
        (unless (= status 1)
          (error 'linenoise-history-set-max-len
                 "Unable to set maximum history length" len))))))

(define linenoise-history-save
  (let ((%linenoiseHistorySave
         (foreign-procedure lib int linenoiseHistorySave (pointer))))
    (lambda (filename)
      (let ((status (%linenoiseHistorySave (bytevector->pointer (string->c-string filename)))))
        (unless (= status 0)
          (error 'linenoise-history-save "Unable to save history file" filename))))))

(define linenoise-history-load
  (let ((%linenoiseHistoryLoad
         (foreign-procedure lib int linenoiseHistoryLoad (pointer))))
    (lambda (filename)
      (let ((status (%linenoiseHistoryLoad (bytevector->pointer (string->c-string filename)))))
        (unless (= status 0)
          (error 'linenoise-history-load "Unable to load history file" filename))))))

(define linenoise-clear-screen
  (foreign-procedure lib void linenoiseClearScreen ()))

(define linenoise-set-multi-line
  (let ((%linenoise-set-multi-line
         (foreign-procedure lib void linenoiseSetMultiLine (int))))
    (lambda (enabled) (%linenoise-set-multi-line (if enabled 1 0)))))

(define linenoise-print-key-codes
  (foreign-procedure lib void linenoisePrintKeyCodes ()))

;; Enable UTF-8.
(let ()
  (define %linenoiseSetEncodingFunctions
    (foreign-procedure lib void linenoiseSetEncodingFunctions
                       (pointer pointer pointer)))
  (define %linenoiseUtf8PrevCharLen
    (lookup-shared-object lib "linenoiseUtf8PrevCharLen"))
  (define %linenoiseUtf8NextCharLen
    (lookup-shared-object lib "linenoiseUtf8NextCharLen"))
  (define %linenoiseUtf8ReadCode
    (lookup-shared-object lib "linenoiseUtf8ReadCode"))
  (%linenoiseSetEncodingFunctions %linenoiseUtf8PrevCharLen
                                  %linenoiseUtf8NextCharLen
                                  %linenoiseUtf8ReadCode)))
