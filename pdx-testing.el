;;; pdx-testing.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;  Description
;;

(load-file "pdx-parser.el")


;;; testing

;;
;;this got carried away
;;FIXME redo T-E-O-I to only have 1 dolist (probably outer one), call it with mapcar

(defun TEST-EXPRESSIONS-ON-INPUTS (test-exps input-strings)
  ;;master tester
  (setq buf nil pt nil) ;ensure bound
  (setq results (make-vector (length input-strings) nil))
  ;;there is one vector-of-lists ``results'' (as in ALL results)
  ;;of dimension = # of input bufs...
  (let ((i 0)
        (buf-results nil))
    ;;...where list ``buf-results'' (as in "results on this buf")
    ;;fills the vector for each input buf.
    (dolist (str input-strings results)
      (setq buf str) ;this is only  global if bound outside of here already
      (dolist (expr test-exps (progn
                                (aset results i buf-results)
                                (setq buf-results nil)
                                (setq i (1+ i))))
        (push (eval expr) buf-results)))))
(defun TEST-EXPRESSIONS-ON-REGION (test-exps &optional emacs-buffer-name)
  ;;test exps on a region selected in an open emacs buffer of given name
  ;;(default to the example 1 txt file)
  (let ((input-string (with-current-buffer
                          (get-buffer (if emacs-buffer-name emacs-buffer-name "pdx-ex1.txt"))
                        (buffer-substring-no-properties
                                       (region-beginning) (region-end)))))
    (TEST-EXPRESSIONS-ON-INPUTS
     test-exps
     (list input-string))))
(defun TEST-EXPRESSIONS-ON-FILE (test-exps &optional path-to-file)
  ;;tests exps on entire file, does not need to be an open emacs buffer
  ;;(default to the example 1 txt file)
  (let ((input-string (with-temp-buffer
                        (insert-file-contents (if path-to-file path-to-file "pdx-ex1.txt"))
                        (buffer-string))))
    (TEST-EXPRESSIONS-ON-INPUTS test-exps (list input-string))))


(defun TEST (&rest test-exps)
  ;;useful wrapper for T-E-O-F with &rest
  (aref (funcall #'TEST-EXPRESSIONS-ON-FILE test-exps) 0))
(defun TEST-REGION (&rest test-exps)
  ;;useful wrapper for T-E-O-R with &rest
  (aref (funcall #'TEST-EXPRESSIONS-ON-REGION test-exps) 0))
(defun TEST-STR (input-strings &rest test-exps)
  ;;wrapper where you specify list of strings
  (funcall #'TEST-EXPRESSIONS-ON-INPUTS test-exps input-strings))

;;; run tests

(TEST
 ;;just test the pretty printer with a quick types list
 '(mapcar #'type-name (list 's 'n '= '{ '} '\. '- 'sym 'eof))
 ); ->
  ; ("string" "number" "equals" "open brace" "close brace" "decimal pt" "minus" "other symbol" "end of file")

(TEST-STR
 (list "s = { n.n }")
 ;;consumes if next=expected?
 ;;NOTE standalone calls to tok are bad if i on whitespace
 ' (tok* -1 (list 's 'n))
   ;;         => (s . 0)  ;can expect any in list?
 ' (tok* 0 '=)
   ;;         => (= . 2)
 ' (tok* 2 '{)
   ;;         => ({ . 4)
 ' (tok* 8 'n)
  ;;          =>  (n . 8)  ;doesnt consume if unexpected?
)

(TEST-REGION
 ;;can we turn the input into lexemes?
 ' (lexemes-list)
 ;;                => (s = { s = n s = s s = s s = { s = s s = - n \. n } s = { s = s s = { s = { s = n } s = n } } } eof)
 ' (let ((lexemes (lexemes-list)))
     (mapcar #'type-name lexemes)) ;; =>
 ;; ("string" "equals" "open brace" "string" "equals" "number"  ...  "close brace" "end of file")
 )

(TEST-STR
 ;;does special type 'sym work?
 (list "$ = { [] }")
 ' (let ((lexemes (lexemes-list)))
     (mapcar #'type-name lexemes))
   ;;                              => ("other symbol" "equals" "open brace" "other symbol" "other symbol" "close brace" "end of file")
 )

(TEST-STR
 ;;can we parse a number expression?
 (list "n" "n.n" "-n")
 ' (lexemes-list) ;; => (n eof), => (n \. n eof), => (- n eof)
 )

;;; blah
(defun unset-my-shit ()
  ;;non function global symbols
  (progn (makunbound 'buf) (makunbound 'results) (makunbound 'pt)))
