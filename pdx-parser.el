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

;this is tokenize-as/consume. returns a parser object, ((idx . expectedp) . token)
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
        (cons
         (cons (skip (1+ i)) t)
         next-type)
      ;;DONE eval error out of range when fails to tokenize at -1.
      (if (>= i 0)
          ;harmlessly return i and token at i. (``(skip i)'' for > bounds)
          (cons (cons (skip i) nil) (scan i))
        ;weird but i dont want a "bof"
        (cons (cons -1 nil) 'sym)))))

(defun tok* (i &optional as-type)
  ;;return a (token . idx) object--sometimes the extra expectedp bit is annoying
  (let ((p (tok i as-type)))
    (cons (cdr p) (caar p))))

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
                           (lex (caar p) (cdr p) (append lexemes (cons (cdr p) nil)))))))
        (lex -1 nil nil))
    ;;named-let is available in scheme and newer elisp
    ;;either way, should still opt for iterative in the parsing stage?
    (named-let lex ((i -1) (type nil) (lexemes nil))
               (if (eq type 'eof)
                   lexemes
                 (let ((p (tok i)))
                   (lex (caar p) (cdr p) (append lexemes (cons (cdr p) nil))))))))


;;; parsing

;; EBNF description
;;
;;  file =
;;  {ident "=" block} "eof" .
;;
;;  block =
;;  "{" {[ident | ident "=" (ident | block)]} "}" .
;;  ident =
;;  string | ["-"] number [ "." number ] .

;;https://emacsdocs.org/docs/elisp/Errors TODO read

(defun parser-container ()
    (let ((pt -1)
          (token-stack nil))
      (cl-labels
          (
                                        ;naive, appending rets to data like this feels wrong
                                        ; (token-push (p)
                                        ;   (if (cdar p)
                                        ;       (push (cdr (tok pt) token-stack))))
           (peek () (cdr (tok pt)))
           ;;accept,expect? try-consume,force-consume? permit,assert?
           ;;we use the word "expect" in ``tok'' already...
           (allow (&optional as-type)
             (let ((p (tok pt as-type)))
               (setq pt (caar p))
               (if (cdar p) p))) ;(if (cdar p) (token-push p))
           (force (as-type)
             (let ((p (allow as-type)))
               (if (not (cdar p))
                   (error "at %s: unexpected token `%s' (%s)"
                          (skip (1+ pt)) (peek) as-type)
                 p)))
           ;;these are written very procedurally i.e. not functionally
           (parse-identifier ()
             (if (eq (peek) 's)
                 (force 's)
               (progn
                 (allow '-)
                 (force 'n)
                 (if (eq (peek) '\.)
                     (progn
                       (force '\.)
                       (force 'n))
                   ))))
           (parse-block ()
             (force '{)
             (cl-loop do
                      (parse-identifier)
                      (if (eq (peek) '=)
                          (progn
                            (force '=)
                            (if (or (eq (peek) 's) (eq (peek) 'n))
                                (parse-identifier)
                              (parse-block))))
                      until (eq (peek) '})))
           (parse-identifier-idea ()
             ;;(list 's '- 'n '\.n 'n) <- "walk-list"
             ;;(list nil nil nil nil nil) <- init "force/allow list" (known length)
             ;;the condititionals in p-i build the f-a list
             ;;then finally once all conditionals are eval'd, walk list seq gets mapcar'd
             ;;on smth like
             ;;(lambda (f-a-lst walk-val)
             ;;  (if (nth f-a-lst-idx f-a-lst) (force w-v) (allow w-v)))
             ;;where f-a-lst-idx is +1'd each map somehow
             ))
        (parse-identifier)))
)
;(defun parse-file ()
;  (cl-loop do
;           (parse-identifier)
;           (force '=)
;           (parse-block)
;           until (eq (tok (1- i)) (tok i))(tok i 'eof))
;  (force 'eof))

;;; pdx-parser.el ends here


