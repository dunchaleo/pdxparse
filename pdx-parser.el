;;; pdx-parser.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 LAPTOP-J7R7QGBM
;;
;; Author: LAPTOP-J7R7QGBM <knair@LAPTOP-J7R7QGBM>
;; Maintainer: LAPTOP-J7R7QGBM <knair@LAPTOP-J7R7QGBM>
;; Created: November 02, 2025
;; Modified: November 02, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/knair/pdx-parser
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;i think these are just the cars of the tokens. a token is ('type . "val")
(list 'num 'eq '{

;hack: tokenize '{ with following whitespace if there is any, just "{" if not.
;accordingly, when scanning whitespace, tokenize as 'w unless the terminating char is "}", then tokenize as '}.
;in both cases the string value of the token can just be the one char. or in C, beg at char and len=1



(provide 'pdx-parser)

;;; pdx-parser.el ends here
