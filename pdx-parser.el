;;; pdx-parser.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; ideally this should be convertable to scheme with find/replace except for i/o.
;; elisp has a lot of built in functionality for working with text that are unused here.
;;  Description
;;
;;; Code:

;i think these will just be the cars of the tokens. a token is ('type . "val")
;(eventually)
(setq types (list 's 'n 'w '= '{ '} '\. '- 'sym)) ;remove 'w

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
    ('sym  "other symbol")))
(defun print-types ()
  (mapcar (lambda (type)
            (print (type-name type)))
          types))


;first try skipping all whitespace. parsing ``n = { s = n s = -n }'' is just as hard as ``n = { s = ns = -n }'' or other variation
;  (being impossible to lex out ss or nn if you actually had real text input is besides the point)
;then there'd be an implicit delimiter operator coming in after every s, n (unless = is next) and }
;would this implicit delimiter be a token though?? probably not since it's literally not in the text

(defun is-whitespace (c)
  (cl-case c ((?\s ?\n ?\t) t)))

(defun skip (i)
  (let ((c (aref buf i)))
    (if (is-whitespace c)
        (skip (1+ i))
      i)))

;for now, tokens will be single symbols.
;this is to be used like those scanner functions in parser.c in main project
(defun scan (i)
  (let* ((c (aref buf i))
         (token (intern (char-to-string c))))
    (if (memq token types)
        token
      (if (is-whitespace c)
          nil    ;'w
        'sym))))

;this is tokenize-as/consume.
;returns a parser object, (last-token . idx)
;  TODO what should this parser object actually be?
;again ``type'' here is a token datatype, its "value" being the symbol itself
(defun tok (i &optional type)
  (let ((n-i (skip (1+ i)))) ;next-i
    (if (or (eq type nil) (eq type (scan n-i))) ;tokenize anything or expect only
        (cons (scan n-i) n-i) ;(cons type n-i) bad when type nil
      (cons (scan i) i))))

;makes a test pass
(defun print-lexemes (buf)
  (dolist (c buf)
    )

(defun parse (buf)
  (let* ()))
;;; pdx-parser.el ends here


;testing: eval inlne with example selected in editor

(with-current-buffer (get-buffer "pdx-ex1.txt")
  (let ((buf (buffer-substring-no-properties (region-beginning) (region-end))))
       buf))
