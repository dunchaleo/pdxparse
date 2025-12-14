;;; pdx-parser.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; branch elisp-features: use emacs' core buffer mechanics instead to cheat tokenizing stage.
;; aim to emit like a token stream.
;;
;; Description
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




;inc ptr to first non whitespace char
(defun skip (&optional i)
  ;(if (eobp)
  ;    (point-max)
  (goto-char (if i i (point)))
  (if (eq (char-after) ?#)
      (skip (forward-line))
    (goto-char (cdr (bounds-of-thing-at-point 'whitespace)))))


;this is to be used like those scanner functions in parser.c in main project.
;relies a lot on thing-at-point, TODO wrap in with-syntax-table to gimp number and word recognition ('-, '. and '., respectively)
;return token type and bounds: (type . (start . end))
;(still bad if called on whitespace pt)
;can scan at any point, does not change buffer ptr (resets it)
(defun scan (i)
  (let* ((pt (point))
         (c (progn (goto-char i) (char-after)))
         (token (cond ((is-number c)
                       (cons 'n (bounds-of-thing-at-point 'number)))
                      ((is-alpha c)
                       (cons 's (bounds-of-thing-at-point 'word)))
                      (t ;treat anything not a number/letter as a 1-char lexeme
                       (cons ((lambda (lexeme)
                                (if (member lexeme (list ?= ?{ ?} ?\. ?-))
                                    (intern (char-to-string lexeme))
                                  'sym))
                              c)
                             (cons (point) (1+ point)))))))
    (goto-char pt)
    token))

;this is tokenize-as/consume. returns a parser object, ((bounds . expectedp) . token-type)
;can tokenize anything (nil), or expect ``as-type'' (atom or list).
;param ``as-type'' is a token type i.e just a symbol, param P is a parser object
(defun tok (&optional as-type)
  (let (;;force a list (``nil'' is a list '() but not a cons cell -elisp docs)
        (as-type (if (listp as-type) as-type (cons as-type nil)))
        ;;``skip'' from end idx of this token (to beg of next) and scan in next type
        (next-type (scan (skip (1+ (point))))))
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
  ;;return a (idx . token-type bounds) object--sometimes the extra expectedp bit is annoying
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
;(defun parser-container ()
;    (let ((pt -1)
;          (state nil)
;          (acc 0))
;      (cl-labels
;          ;;helpers: allow,force are used for their side-effects.
;          ;;procedures for non-terminals: parse-*
;          ((peek () (cdr (tok pt)))
;           ;;accept,expect? try-consume,force-consume? permit,assert?
;           ;;we use the word "expect" in ``tok'' already...
;           (allow (&optional as-type)
;             (let ((p (tok pt as-type)))
;               ;;redundant now since keeping state stack: pt is (caar (car state))
;               (setq pt (caar p))
;               (if (cdar p) (progn ;;accumulator++, to track how many times to pop
;                              (setq acc (1+ acc)) (push p state)))))
;           (force (as-type)
;             (let ((p (allow as-type)))
;               (if (not (cdar p))
;                   (error "at %s: unexpected token `%s' (%s)"
;                          (skip (1+ pt)) (peek) as-type)
;                 p)))
;           (compile-ident-expr (acc-init)
;             (let ((ident-expr nil)
;                   (n (- acc acc-init)))
;             ;;TODO for real multichar tokens, this will need to be updated.
;             ;;here, it works like the atof(ident-expr) option in comment in ``p-i''. but concat as list -> to string instead of making a float.
;               (cl-loop repeat n do (push (cdr (pop state)) ident-expr))
;               ;;push the data object to the state IN PLACE OF all the read tokens or other objects there already
;               (push ident-expr state))
;             ;;compile rebuilds result into state stack, so the accumulator only has 1 more object
;             (setq acc (1+ acc-init)))
;           (compile-block-expr (acc-init)
;             (let ((block-expr nil) (obj nil) (key-val nil)
;                   (n ; -2 for '{ and '}
;                    (- acc acc-init 2)))
;               (pop state) ;  '}
;               (cl-loop repeat (ceiling (/ n 3)) do ;NOTE counting trick
;                        ;;this is so bad maybe because the grammar doesnt go well with the desired data struct?
;                        ;;  (imagine an "assignment" nonterminal)
;                        ;;intended to be a less expressive/more compact data format where block name is first elt
;                        ;;NOTE this can only deal with blocks where all elts are assignments, no single idents
;                        (let ((val (pop state)) (key (progn (pop state) (pop state))))
;                          (push (cons key val) block-expr)))
;               (pop state) ;  '{
;               ;;push the data object to the state IN PLACE OF all the read tokens or other objects there already
;               (push block-expr state))
;             (setq acc (1+ acc-init)))
;           ;; new for nonterm procedures:
;           ;; instead of building -exp return list by (push (force 'x) [-exp]),
;           ;; ``allow'' now pushes to a state stack and nonterm expression is built
;           ;; by pushing (pop state) to it. means theres no need to reverse before return.
;           ;; (and it could be extended for precedence. although building a list in reverse
;           ;; and calling reverse is apparently pretty normal in lisp?)
;           ;;NOTE these are written very procedurally i.e. not functionally
;           (parse-identifier ()
;             (let ((acc-init acc))
;               (if (eq (peek) 's)
;                   (force 's)
;                 ;;how to handle number exps? you could just have the parser check for syntax validity and throw it into atof().
;                 ;;then you could have a number type as the thing in the built tree. question is what you want the data structure to be.
;                 (progn
;                   (allow '-)
;                   (force 'n)
;                   (if (eq (peek) '\.)
;                       (progn
;                         (force '\.)
;                         (force 'n)))))
;               (compile-ident-expr acc-init)))
;           (parse-block ()
;            ;;block data structure: atomics (standalone idents), cons pairs (for ident=ident) and lists (inner blocks)
;            ;;UPDATE: not true anymore, see compile-block-expr
;            (let ((acc-init acc))
;              (force '{)
;              (cl-loop until (eq (peek) '}) do
;                       ;push ident
;                       ;NOTE this is ok for empty block thanks to until clause evaling up front
;                       (parse-identifier)
;                       (if (eq (peek) '=)
;                           (progn
;                             (force '=)
;                             (if (eq (peek) '{)
;                                 (parse-block)
;                               (parse-identifier)))))
;              (force '})
;              (compile-block-expr acc-init)))
;          ; (parse-file () ;TODO
;          ;   (let ((file-exp nil))
;          ;     (cl-loop do
;          ;              (push (expr-to-str (parse-identifier)) file-exp)
;          ;              (force '=)
;          ;              (push (parse-block (pop file-exp)) file-exp)
;          ;              until (eq (peek) 'eof))
;          ;     (force 'eof)
;          ;     (push 'eof file-exp)
;          ;     (reverse file-exp))))
;           )
;        (parse-block))
;      state))

;;; pdx-parser.el ends here


