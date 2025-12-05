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
      ;;DONE(todo) eval error out of range when fails to tokenize at -1.
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

;; EBNF-like description
;; () grouping
;; | alternative
;; * repeat 0 or more times   (also common {})
;; ? optional                 (also common [])
;; 'x terminal x              (also common "")
;; =. define production rule (for nonterminal)
;;
;;
;;  file =
;;  (ident '= block)* 'eof .
;;
;;  block =
;;  '{ (ident | ident '= (ident | block))* '} .
;;  ident =
;;  's | '-? 'n ('. 'n)? .
;;

;;https://emacsdocs.org/docs/elisp/Errors TODO read
;NOTE tok's "any-in-list" feature is still unused with this...
;organized so there can be a "global" pt
(defun parser-container ()
    (let ((pt -1)
          (state nil)
          (acc 0))
      (cl-labels
          ;;helpers: allow,force are used for their side-effects.
          ;;procedures for non-terminals: parse-*
          ((peek () (cdr (tok pt)))
           ;;accept,expect? try-consume,force-consume? permit,assert?
           ;;we use the word "expect" in ``tok'' already...
           (allow (&optional as-type)
             (let ((p (tok pt as-type)))
               ;;redundant now since keeping state stack: pt is (caar (car state))
               (setq pt (caar p))
               (if (cdar p) (progn ;;accumulator++, to track how many times to pop
                              (setq acc (1+ acc)) (push p state)))))
           (force (as-type)
             (let ((p (allow as-type)))
               (if (not (cdar p))
                   (error "at %s: unexpected token `%s' (%s)"
                          (skip (1+ pt)) (peek) as-type)
                 p)))
           (compile-ident-expr (acc-init)
             (let ((ident-exp nil))
             ;;TODO for real multichar tokens, this will need to be updated.
             ;;here, it works like the atof(ident-exp) option in comment in ``p-i''. it's just concatenating instead of making a float.
               (cl-loop repeat (- acc acc-init) do (push (cdr (pop state)) ident-exp))
               ;;push the data object to the state IN PLACE OF all the read tokens or other objects there already
               (push ident-exp state))
             ;;compile rebuilds result into state stack, so the accumulator only has 1 more object
             (setq acc (1+ acc-init)))
           (compile-block-expr (acc-init)
             (let ((block-exp nil) (val nil))
               (pop state) ;  '}
               (cl-loop repeat (- acc acc-init) do
                        ;;there's no place to do this but here--filtering the '= tokens from the state stack in the case of ident=ident in a block
                        ;;(and forming the cons pair appropriately)
                        ;;TODO then we can also make sure to push single symbols in for standalone idents. (or in compile ident?)
                        ;;this is so bad maybe because the grammar doesnt go well with the desired data struct?
                        ;;  (imagine an "assignment" nonterminal)
                        (let ((obj (pop state)))
                          (setq val
                                (if (eq (cdr obj) '=) ;this sucks
                                    ;;cons cell (ident . ident2) in ``ident = ident2''
                                    (cons (pop state) val)
                                  obj))
                          (push val block-exp)))
               (pop state) ;  '=
               (pop state) ;  '{
               ;;ident in ``ident = {}'' as fist elt:
               (push (pop state) block-exp)
               ;;push the data object to the state IN PLACE OF all the read tokens or other objects there already
               (push block-exp state))
             (setq acc (1+ acc-init)))
           ;; new for nonterm procedures:
           ;; instead of building -exp return list by (push (force 'x) [-exp]),
           ;; ``allow'' now pushes to a state stack and nonterm expression is built
           ;; by pushing (pop state) to it. means theres no need to reverse before return.
           ;; (and it could be extended for precedence. although building a list in reverse
           ;; and calling reverse is apparently pretty normal in lisp?)
           ;;NOTE these are written very procedurally i.e. not functionally
           (parse-identifier ()
             (let ((acc-init acc))
               (if (eq (peek) 's)
                   (force 's)
                 ;;how to handle number exps? you could just have the parser check for syntax validity and throw it into atof().
                 ;;then you could have a number type as the thing in the built tree. question is what you want the data structure to be.
                 (progn
                   (allow '-)
                   (force 'n)
                   (if (eq (peek) '\.)
                       (progn
                         (force '\.)
                         (force 'n)))))
               (compile-ident-expr acc-init)))
           (parse-block ()
            ;;block data structure: atomics (standalone idents), cons pairs (for ident=ident) and lists (inner blocks)
            (let ((acc-init (+ acc 2)))
              (force '{)
              (cl-loop until (eq (peek) '}) do
                       ;push ident
                       ;NOTE this is ok for empty block thanks to until clause evaling up front
                       (parse-identifier)
                       (if (eq (peek) '=)
                           (progn
                             (force '=)
                             (setq acc (1+ acc)) ;;;;;;;;;see line 178
                             (if (eq (peek) '{)
                                 (parse-block)
                               (parse-identifier)))))
              (force '})
              (compile-block-expr acc-init)))
          ; (parse-file () ;TODO
          ;   (let ((file-exp nil))
          ;     (cl-loop do
          ;              (push (expr-to-str (parse-identifier)) file-exp)
          ;              (force '=)
          ;              (push (parse-block (pop file-exp)) file-exp)
          ;              until (eq (peek) 'eof))
          ;     (force 'eof)
          ;     (push 'eof file-exp)
          ;     (reverse file-exp))))
           )
        (parse-block))
      state))

;;; pdx-parser.el ends here


;OLD temp from testing (parse-block "test")
;ELISP> (setq buf "{n=n n={n}}")
;"{n=n n={n}}"
;ELISP> (parser-container)
;(("some-identifier" "some-identifier")
; ("some-identifier" . "some-identifier")
; "test")


;temp from testing
;
;ELISP> (parser-container)
;((nil (n) (n \. n) (- n) (s)))
;
;ELISP> buf
;"{n n.n -ns}"
;
;ELISP> (setq buf "{s n = {-n.n s} s = {s s=s s={s s}}}")
;"{s n = {-n.n s} s = {s s=s s={s s}}}"
;
;ELISP> (parser-container)
;((nil nil nil nil nil nil nil nil nil nil nil nil nil
;      (nil nil nil ((0 . t) . {) (s) ((n) (- n \. n) (s)) ((s) (20 . t) . {)
;           ((20 . t) . {) (s) ((s) s) (s) ((s) (s) (s)))))
