;;; pdx-parser.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; ideally this should be convertable to scheme with find/replace except for i/o.
;; elisp has a lot of built in functionality for working with text that are unused here.
;;  Description
;;
;;; Code:


;this is just for pretty printing.
;eventually a token will be ('type, "val") where "val" is what really appears in text
(defun type-name (type)
  (cl-case type
    ('s  "string")
    ('n  "number")
    ('w  "whitespace") ;remove
    ('=  "equals")
    ('{  "open brace")
    ('}  "close brace")
    ('\.  "decimal pt")
    ('-  "minus")
    ('sym  "other symbol")
    ('eof "end of file")))


;first try skipping all whitespace. parsing ``n = { s = n s = -n }'' is just as hard as ``n = { s = ns = -n }'' or other variation
;  (being impossible to lex out ss or nn if you actually had real text input is besides the point)
;then there'd be an implicit delimiter operator coming in after every s, n (unless = is next) and }
;would this implicit delimiter be a token though?? probably not since it's literally not in the text

;idea: resize global ``buf'' +1 and (aset) new last elt nil, so we have actual nil-terminated C-like strings
;better idea: stop using strings. could have current buf instead of global string buf and rewrite funs with (point) rather than aref.
;  (most functions acting on buffer objects would have to be off limits)

(defun is-whitespace (c)
  (cl-case c ((?\s ?\n ?\t) t)))

;inc ptr to first non whitespace char
;strings are not nil terminated so ptr at eof will be out of bounds
;TODO skip comments
(defun skip (i)
  (if (>= i (length buf))
      (length buf)
    (let ((c (aref buf i)))
      (if (is-whitespace c)
          (skip (1+ i))
        i))))

;for now, tokens will be single symbols.
;this is to be used like those scanner functions in parser.c in main project
(defun scan (i)
  (let
      ;;emulate ``char* c = buf[i]'' on null-terminated C-string buf  (change from 'eof to nil?)
      ((c (if (>= i (length buf)) 'eof (aref buf i))))
    ((lambda (lexeme)
       ;;this lambda may represent a simplified lexeme-to-token procedure. it's here
       ;;because i wanted to use a 'sym type w/out anything like a glbl token-types enum.
       ;;could just replace block w/ ``(unless (eq c 'eof) (intern (char-to-string c))''
       ;;FIXME (should have just kept everything super simple--no 'sym, no 'eof)
       (if (eq lexeme 'eof)
           'eof
         (if (member lexeme (list ?s ?n ?= ?{ ?} ?\. ?-))
             (intern (char-to-string lexeme))
           (if (is-whitespace lexeme) ;shouldnt happen
               nil ;'w failsafe
             'sym))))
     c)))

;this is tokenize-as/consume. returns a parser object, (last-token . idx)
;  (TODO? param P instead of i?)
;can tokenize anything (nil), or expect ``as-type'' (atom or list)
;(again, ``as-type'' here is a token datatype, its "value" being the symbol itself).
;  not keeping a token word length for now but finding next-type would need to use that
;    (and then we wouldnt have to ``(tok -1)'' to initialize)
(defun tok (i &optional as-type)
  (let (;;force a list (``nil'' is a list '() but not a cons cell -elisp docs)
        (as-type (if (listp as-type) as-type (cons as-type nil)))
        ;;``skip'' from end idx of this token (to beg of next) and scan in next type
        (next-type (scan (skip (1+ i)))))
    (if (or (eq as-type nil)
            (member t (mapcar (lambda (type) (eq type next-type)) as-type)))
        (cons next-type (skip (1+ i)))
      (cons (scan i) (skip i))))) ;FIXME eval error out of range when fails to tokenize at -1.

;makes a single pass to convert all tokens read into a list
;for this simplified trial, it just holds symbols. later, it should hold strings
; (what actually appears in input text, only incidentally holds data about tokens' type or location)
;    (e.g. using start ptr and length for a string, or being able to (intern) the string and get token type)
(defun lexemes-list ()
  (if (version< emacs-version "28.0")
      (cl-labels ((lex (i type lexemes)
                       (if (eq type 'eof)
                           lexemes
                         (let ((p (tok i)))
                           (lex (cdr p) (car p) (append lexemes (cons (car p) nil)))))))
        (lex -1 nil nil))
    ;;named-let is available in scheme and newer elisp
    ;;either way, should still opt for iterative in the parsing stage?
    (named-let lex ((i -1) (type nil) (lexemes nil))
               (if (eq type 'eof)
                   lexemes
                 (let ((p (tok i)))
                   (lex (cdr p) (car p) (append lexemes (cons (car p) nil))))))))

(defun parse ()
  (let* ()))

;;; pdx-parser.el ends here


;;; testing
;;
;;this got carried away
;;FIXME redo T-E-O-I to only have 1 dolist (probably outer one), call it with mapcar

(defun TEST-EXPRESSIONS-ON-INPUTS (test-exps input-strings)
  ;;master tester
  (setq buf nil) ;ensure bound
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
 ' (tok -1 (list 's 'n))
   ;;         => (s . 0)  ;can expect any in list?
 ' (tok 0 '=)
   ;;         => (= . 2)
 ' (tok 2 '{)
   ;;         => ({ . 4)
 ' (tok 8 'n)
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
  (progn (makunbound 'buf) (makunbound 'results)))
