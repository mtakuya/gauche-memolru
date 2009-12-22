;;;
;;; memolru.scm
;;; Memoizing macro of a least-recentry-used cache algorithm (v0.0.1)
;;;
;;; Copyright (c) 2009 Takuya Mannami <mtakuya@users.sourceforge.jp>
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;;
;;; 3. Neither the name of the authors nor the names of its contributors
;;; may be used to endorse or promote products derived from this
;;; software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module memolru
  (use srfi-1)
  (export define-memolru))

(select-module memolru)

(define (make-set) '())
(define (set-limit? s n) (= (length s) n))

(define-syntax in!
  (syntax-rules () ((_ s o) (set! s (append s (list o))))))

(define-syntax out!
  (syntax-rules () ((_ s) (set! s (cdr s)))))

(define-syntax l2r!
  (syntax-rules () ((_ s o) (set! s (append (delete o s) (list o))))))

(define-syntax define-memolru
  (syntax-rules () ((_ setnum (proc v ...) b1 b2 ...)
     (define proc 
             (let ((proc (lambda (v ...) b1 b2 ...)) (cache (make-set)))
               (lambda (v ...)
                 (let* ((key (list v ...)) (obj (assoc key cache)))
                             (if (not (equal? obj #f))
                                 (begin (l2r! cache obj) (cdr obj))
                                 (let1 val (call-with-values (lambda () (values v ...)) proc)
                                       (when (set-limit? cache setnum) (out! cache))
                                       (in! cache (cons key val)) val)))))))))
(provide "memolru")