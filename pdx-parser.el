;;; pdx-parser.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; ideally this should be convertable to scheme with find/replace
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

;for now, tokens will be single symbols
;this is to be used like those scanner functions in parser.c in main project
; TODO if not ('= '{ '} '\.) then 'sym
(defun get-token (i)
  (let ((c (aref buf i)))
    (if (is-whitespace c)
      nil
    (intern (char-to-string (aref buf i))))))

;this is tokenize-as/consume. call it like (tok 'type (skip i)) ;unless this fun calls skip
;returns a parser object, (last-token . current-idx)
;  TODO what should this parser object actually be? problem finding last-token.
;again ``type'' here is a token datatype, its "value" being the symbol itself
(defun tok (type i)
  (let ((ni (skip i)))
    (if (eq type (get-token i))
        (cons type (skip i))
      (cons (get-token i) i))))

(defun parse ()
  (let* ()))
;;; pdx-parser.el ends here
