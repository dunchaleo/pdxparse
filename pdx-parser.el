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
    ('eof "end of file"))) ;eof really not needed now either


;first try skipping all whitespace. parsing ``n = { s = n s = -n }'' is just as hard as ``n = { s = ns = -n }'' or other variation
;  (being impossible to lex out ss or nn if you actually had real text input is besides the point)
;then there'd be an implicit delimiter operator coming in after every s, n (unless = is next) and }
;would this implicit delimiter be a token though?? probably not since it's literally not in the text

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

;this is tokenize-as/consume.
;returns a parser object, (last-token . idx)
;  TODO what should this parser object actually be?
;  TODO?? param P instead of i?
;again ``type'' here is a token datatype, its "value" being the symbol itself.
;  not keeping a token word length for now but finding next-i would need to use that
;    (and then we wouldnt have to (tok -1) to initialize)
(defun tok (i &optional type)
  (let ((n-i (skip (1+ i)))) ;next-i:
    (if (or (eq type nil) (eq type (scan n-i))) ;tokenize anything or expect only
        (cons (scan n-i) n-i) ;(cons type n-i) bad when type nil/any
      (cons (scan i) i))))

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
    ;k;either way, should still opt for iterative in the parsing stage?
    (named-let lex ((i -1) (type nil) (lexemes nil))
               (if (eq type 'eof)
                   lexemes
                 (let ((p (tok i)))
                   (lex (cdr p) (car p) (append lexemes (cons (car p) nil))))))))

(defun parse ()
  (let* ()))

;;; pdx-parser.el ends here

;;;testing:

(defun TEST (test-exp &optional emacs-buffer-name) ;no &rest, to test multi exp, just do progn
  ;;test region selected in open emacs buffer in the example txt file
  (with-current-buffer (get-buffer (if emacs-buffer-name emacs-buffer-name "pdx-ex1.txt"))
    ;;with lexical scoping, cant pass buf into anything above, it has to be a global..
    (setq buf (buffer-substring-no-properties (region-beginning) (region-end)))
    (eval test-exp)))
(defun TEST-MULTI (test-exp bufs-to-use)
  ;;test a list of strings as seperate input buffers
 (dolist (str bufs-to-use)
   (setq buf str)
   (eval test-exp)))
(defun TEST-MULTI (test-exp bufs-to-use)
  ;;test a list of strings as seperate input buffers
  (setq results nil)
  (dolist (str bufs-to-use results)
    (setq buf str)
    (setq results (append
                   results (eval test-exp)))))

(TEST ' ;just test the pretty printer with a quick types list
   (mapcar #'type-name (list 's 'n '= '{ '} '\. '- 'sym 'eof))
 ); ->
  ; ("string" "number" "equals" "open brace" "close brace" "decimal pt" "minus" "other symbol" "end of file")


(TEST ' ;can we turn the input into lexemes?
 (lexemes-list)
 ); ->
  ; (s = { s = n s = s s = s s = { s = s s = - n \. n } s = { s = s s = { s = { s = n } s = n } } } eof)

(TEST '
 (let ((lexemes (lexemes-list)))
   (mapcar #'type-name lexemes))
 ); ->
  ; ("string" "equals" "open brace" "string" "equals" "number"  ...  "close brace" "end of file")

(TEST-MULTI ' ;does special type 'sym work?
 (let ((lexemes (lexemes-list)))
   (mapcar #'type-name lexemes))
 (list "$ = { [] }")
 ); ->
  ; ("other symbol" "equals" "open brace" "other symbol" "other symbol" "close brace" "end of file")

(TEST-MULTI ' ;can we parse a number expression?
 (lexemes-list)
 (list "n" "n.n" "-n")
 )
