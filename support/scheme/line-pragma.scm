(define-module (line-pragma)
  #:use-module (ice-9 rdelim))

(eval-when (compile load eval)
 (define eor (call-with-input-string "#;done" (lambda (port) (read port))))
 (read-hash-extend #\l #f)
 (define line-pragma-handler (lambda (char port)
                               (let* ((ine (read port))
                                      (lineno (read port))
                                      (filename (read port)))
                                 (if (not (eq? ine 'ine))
                                     (error (format #f "Expected '#line <line-number> <filename>'; got '#~a~a ~a \"~a\"'." char ine lineno filename)))
                                 (set-port-filename! port filename)
                                 (set-port-line! port lineno)
                                 (set-port-column! port 0)
                                 ;; Return unspecified on purpose.
                                 (if #f #f)
                                 )))
 (set-procedure-property! line-pragma-handler 'read-comment #t)
 (read-hash-extend #\l line-pragma-handler)
 (read-hash-extend #\" (lambda (char port)
                         (let ((accum '()))
                           (let loop ((entry (read-char port)))
                             (if (or (eof-object? entry)
                                     (and (char=? #\" entry)
                                          (char=? #\# (peek-char port))
                                          (begin (read-char port)
                                                 #t)))
                                 ;; We're done
                                 (apply string (reverse accum))
                                 (begin
                                   (if (and (char=? #\# entry)
                                            (char=? #\l (peek-char port)))
                                       ;; Drop this line
                                       (begin (read-line port)
                                              (loop (read-char port)))
                                       (begin
                                         ;; Keep and loop
                                         (set! accum (cons entry accum))
                                         (loop (read-char port)))))))))))

