;;; navbar.el --- Navigation bar for Emacs           -*- lexical-binding: t; -*-

;; Copyright (C) 2015       papaeye
;; Copyright (C) 2018-2019  Naoya Yamashita
;; Copyright (C) 2019       ROCKTAKEY

;; Author: papaeye <papaeye@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience
;; Version: 0.1.0
;; URL: https://github.com/conao3/navbar.el
;; Package-Requires: ((emacs "24.4"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the Affero GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Affero GNU General Public License for more details.

;; You should have received a copy of the Affero GNU General Public License
;; along with this program.  If not, see  <https://www.gnu.org/licenses/>.

;;; Commentary:

;; navbar.el is a navigation bar for Emacs.
;; Just like the navbar component of Bootstrap <http://getbootstrap.com/>,
;; navbar.el can contain various components called navbar items.
;;
;; navbar.el requires Emacs 24.4 or later.

;;; Code:

(defconst navbar-version "0.1.0")

(require 'cl-lib)

;;; Customization

(defgroup navbar nil
  "Navigation bar for Emacs"
  :group 'environment)

(defvar navbar-init-frg nil)

(defun navbar-custom-set-item-list (sym val)
  "Set SYM as VAL and call `navbar-sync'."
  (set-default sym val)
  (when navbar-init-frg
    (navbar-sync)))

(defcustom navbar-item-list nil
  "List of navbar items.
It is necessary to run `navbar-sync' to reflect the change of this."
  :set #'navbar-custom-set-item-list
  :type '(repeat (choice (string :tag "Literal text")
			 (plist :tag "Literal property list")
			 (function :tag "Factory function")))
  :group 'navbar)

(defcustom navbar-item-separator
  (propertize " "
	      'display '(space :width 0.2)
	      'face `(:background ,(face-background 'default)))
  "String to separate navbar items."
  :type 'string
  :group 'navbar)

(defcustom navbar-item-padding
  (propertize " " 'display '(space :width 0.5))
  "String to usable to pad a navbar item."
  :type 'string
  :group 'navbar)

(defcustom navbar-display-function #'navbar-display
  "Function to display serialized a list of navbar items in a buffer.
The function is called with two arguments, a list of navbar items and
a buffer."
  :type 'function
  :group 'navbar)

(defcustom navbar-lighter " nav"
  "The `navbar-mode' lighter."
  :type 'string
  :group 'navbar)

(defface navbar
  '((t :inherit fringe))
  "Face of the navbar buffer."
  :group 'navbar)

(defface navbar-item
  '((t :foreground "#eee8d5" :background "#b58900"))
  "Default face of the navbar item."
  :group 'navbar)

(defconst navbar-font-lock-keywords
  '(("(\\(navbar-define-item\\)\\>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))))

;;; Polyfills

(defun navbar-delete-side-window (window)
  "Delete side window WINDOW.  Polyfill of `delete-side-window'."
  (let ((window-combination-resize
         (window-parameter (window-parent window) 'window-side))
        (ignore-window-parameters t))
    (delete-window window)))

(defun navbar-property-at (point prop window)
  (with-selected-window window
    (get-text-property point prop)))

;;; Features

(defvar navbar-item-alist nil)

(defmacro navbar-define-item (item doc &rest args)
  "Define a navbar item ITEM.
A navbar item is a plain property list.
This macro defines a function ITEM which returns a property list.
If :get property described below is supplied in ARGS, this macro also
defines a function ITEM-update which updates the navbar buffer cleverly.

DOC is a doc string for function ITEM.

:enable
	VALUE should be a symbol of a variable.
	If the symbol value is nil, the navbar item is not displayed.
:get	VALUE should be a function which returns the current value of
	the navbar item.
	It should return symbol `unchanged' if the value is not updated.
:initialize
	VALUE should be a function which is run by `navbar-initialize'
	if ENABLE is non-nil at that time.
:deinitialize
	VALUE should be a function which is run by `navbar-deinitialize'.
:hooks	VALUE should be a list of cons:
	CAR is a symbol of a hook and CDR is a function.
	Each function is added to the corresponding hook by
	`navbar-initialize' and removed from it by `navbar-deinitialize'.
:mode	VALUE should be a symbol of a mode.
:mode-on
	VALUE should be a function added to MODE-on-hook.
	It is removed from the hook by `navbar-deinitialize'.
	This is also run by `navbar-initialize'
	if the mode is enabled at that time.
:mode-off
	VALUE should be a function added to MODE-off-hook.
	It is removed from the hook by `navbar-deinitialize'.
	This is also run by `navbar-deinitialize'."
  (declare (indent defun) (doc-string 2))
  (let ((key `(quote ,item))
	(item-update (intern (concat (symbol-name item) "-update")))
	(enable t)
	getter
	get
	initialize
	deinitialize
	hooks
	;; for mode
	mode
	mode-on-hook
	mode-on-func
	mode-off-hook
	mode-off-func
	mode-hooks
	;; for extra keywords
	extra-keywords
	keyword)

    (while (keywordp (setq keyword (car args)))
      (setq args (cdr args))
      (pcase keyword
	(`:enable (setq enable `(quote ,(pop args))))
	(`:get (setq getter (pop args)))
	(`:initialize (setq initialize (list :initialize (pop args))))
	(`:deinitialize (setq deinitialize (list :deinitialize (pop args))))
	(`:hooks (setq hooks (list :hooks (pop args))))
	(`:mode (setq mode (pop args)))
	(`:mode-on (setq mode-on-func (pop args)))
	(`:mode-off (setq mode-off-func (pop args)))
	(_ (push keyword extra-keywords)
	   (push (pop args) extra-keywords))))

    (when getter
      (setq get (list :get getter)))

    (when mode
      (setq enable `(quote ,mode))
      (setq mode-on-hook (intern (concat (symbol-name mode) "-on-hook")))
      (setq mode-off-hook (intern (concat (symbol-name mode) "-off-hook")))
      (when mode-on-func
	(setq initialize (list :initialize mode-on-func))
	(push (list 'cons `(quote ,mode-on-hook) mode-on-func) mode-hooks))
      (when mode-off-func
	(setq deinitialize (list :deinitialize mode-off-func))
	(push (list 'cons `(quote ,mode-off-hook) mode-off-func) mode-hooks))
      (when mode-hooks
	(setq hooks (list :hooks `(list ,@(nreverse mode-hooks))))))

    `(progn
       ,(when getter
	  `(defun ,item-update (&optional force)
	     (when (navbar-item-update ,key force)
	       (navbar-update))))
       (defun ,item ()
	 ,doc
	 (list :key ,key :enable ,enable
	       ,@get
	       ,@initialize
	       ,@deinitialize
	       ,@hooks
	       ,@(nreverse extra-keywords))))))

(defun navbar-item-value-get (key)
  "Return KEY's :value property value."
  (plist-get (cdr (assq key navbar-item-alist))
	     :value))

(defun navbar-item-enabled-p (key)
  "Return non-nil if KEY's item is enabled."
  (navbar--item-enabled-p
   (cdr (assq key navbar-item-alist))))

(defun navbar--item-enabled-p (item)
  (or (not (plist-member item :enable))
      (symbol-value (plist-get item :enable))))

(defun navbar-item-update (key &optional force)
  "Update KEY's value by running the value of :get property if available.
If optional FORCE argument is non-nil, it is passed to :get function.

Return non-nil if the item has :get property and the return value of
the :get function is neither symbol `unchanged' nor existing value."
  (let* ((item (cdr (assq key navbar-item-alist)))
	 (getter (plist-get item :get))
	 old-value new-value)
    (when getter
      (setq old-value (plist-get item :value))
      (setq new-value (and (navbar--item-enabled-p item)
			   (if force
			       (funcall getter force)
			     (funcall getter))))
      (unless (or (eq new-value 'unchanged)
		  (equal-including-properties new-value old-value))
	(plist-put item :value new-value)))))

(defun navbar--item-propertize (value &rest properties)
  (let ((plist properties)
	p v)
    (while plist
      (setq p (pop plist))
      (setq v (pop plist))
      (pcase p
	(`:truncate (setq value (truncate-string-to-width value v nil nil t)))
	(`:propertize (setq value (apply #'propertize value v)))
	(`:padding (setq value (concat v value v)))
	(`:padding-left (setq value (concat v value)))
	(`:padding-right (setq value (concat value v))))))
  value)

(defun navbar--item-value-serialize (value)
  "Convert item VALUE to a string.
VALUE can be a string, a cons of a string and a property list
or a list of them.  If it is a list, this uses `concat'
to concatenate the elements of the list."
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((keywordp (cadr value))
    (apply #'navbar--item-propertize
	   (navbar--item-value-serialize (car value))
	   (cdr value)))
   (t
    (concat (navbar--item-value-serialize (car value))
	    (navbar--item-value-serialize (cdr value))))))

(defun navbar--item-serialize (item)
  "Convert ITEM to a string if possible.
If an item value is a symbol, the symbol is returned as is.
If ITEM has multiple string values,
they are concatenated with `navbar-item-separator'."
  (let ((value (plist-get item :value)))
    (if (and (symbolp value) value)
	value
      (apply #'navbar--item-propertize
	     (mapconcat #'navbar--item-value-serialize
                        (if (or (stringp value) (keywordp (cadr value)))
                            (list value)
                          value)
			navbar-item-separator)
	     item))))

(defun navbar--serialize (item-list)
  "Convert ITEM-LIST to a list of item values.
An item value is converted to a string if possible.

The continuous elements of strings are concatenated with
`navbar-item-separator'.
Disabled items are ignored."
  (let (result value)
    (dolist (item item-list (nreverse result))
      (when (navbar--item-enabled-p item)
	(setq value (navbar--item-serialize item))
	(if (and (stringp value) (stringp (car result)))
	    (cl-callf concat (car result) navbar-item-separator value)
	  (push value result))))))

(defun navbar--expand-glues (values strings window)
  (let* ((max-width (window-body-width window t))
	 (line-width (with-temp-buffer
		       (let (deactivate-mark)
			 (insert (apply #'concat strings)))
		       (save-window-excursion
			 (set-window-buffer window (current-buffer))
			 ;; Default X-LIMIT seems to return (1- max-width)
			 ;; if the fringe width is 0 and the text width is
			 ;; larger than max-width.
			 (car (window-text-pixel-size window nil nil
						      (1+ max-width)))))))
    (if (>= line-width max-width)
	strings
      (let* ((space (- max-width line-width))
	     (num-glues (cl-loop for value in values
				 count (eq value 'glue)))
	     (q (/ space num-glues))
	     (r (% space num-glues))
	     glues)
	(setq glues (make-list num-glues q))
	(cl-incf (car glues) r)
	(mapcar (lambda (value)
		  (if (eq value 'glue)
		      (propertize " " 'display `(space :width (,(pop glues))))
		    value))
		values)))))

(defun navbar-display (item-list buffer)
  "Display serialized ITEM-LIST in BUFFER."
  (let* ((values (navbar--serialize item-list))
	 (strings (cl-loop for value in values
			   when (stringp value)
			   collect value)))
    (unless (equal values strings)
      (setq strings (navbar--expand-glues
		     values strings (get-buffer-window buffer))))
    (with-current-buffer buffer
      (let ((deactivate-mark))
	(erase-buffer)
	(insert (apply #'concat strings))))))

(defun navbar-update (&optional frame)
  "Update navbar of FRAME."
  (with-selected-frame (or frame (selected-frame))
    (funcall navbar-display-function
	     (mapcar #'cdr navbar-item-alist)
	     (navbar-get-buffer frame))))

(defun navbar--funcall-with-no-display (function &rest arguments)
  (let ((navbar-display-function #'ignore))
    (apply #'funcall function arguments)))

(defun navbar-initialize ()
  "Initialize `navbar-item-alist' and add functions to hooks,
Also, this runs :initialize functions without updating the navbar buffer."
  (navbar-deinitialize)
  (let ((item-alist))
    (dolist (item navbar-item-list)
      (when (functionp item)
	(setq item (funcall item)))
      (when (stringp item)
	(setq item (list :key t :value item)))
      (let ((key (plist-get item :key))
	    (hooks (plist-get item :hooks)))
	(push (cons key item) item-alist)
        (mapc (lambda (hook) (add-hook (car hook) (cdr hook))) hooks)))
    (mapc (lambda (item)
            (let ((fn (plist-get item :initialize)))
              (when (and fn (navbar--item-enabled-p item))
                (navbar--funcall-with-no-display fn))))
          (mapcar #'cdr navbar-item-alist))
    (setq navbar-item-alist (nreverse item-alist))))

(defun navbar-deinitialize ()
  "Remove functions from hooks and clean up `navbar-item-alist'.
Also, this runs :deinitialize functions without updating the navbar buffer."
  (dolist (item (mapcar 'cdr navbar-item-alist))
    (let ((hooks (plist-get item :hooks))
	  (fn    (plist-get item :deinitialize)))
      (mapc (lambda (hook) (remove-hook (car hook) (cdr hook))) hooks)
      (when fn
	(navbar--funcall-with-no-display fn))))
  (setq navbar-item-alist nil))

(defun navbar-sync ()
  "Reinitialize navbar items and refresh the navbar buffer."
  (interactive)
  (navbar-initialize)
  (navbar-update))

;;; GUI

(defvar navbar-base-map
  (let ((map (make-sparse-keymap)))
    (dolist (mouse '("mouse-1" "mouse-2" "mouse-3"))
      (dolist (kind '("" "drag-" "down-"))
	(dolist (repeat '("" "double-" "triple-"))
	  (dolist (modifier '("" "A-" "C-" "H-" "M-" "S-" "s-"))
	    (define-key map
	      (kbd (concat modifier "<" repeat kind mouse ">"))
	      'ignore)))))
    map)
  "Keymap ignoring all mouse events.")

(defun navbar-get-buffer-name (&optional frame)
  (let ((string (prin1-to-string (or frame (selected-frame)))))
    ;; Match the frame address in core
    (string-match " \\([^ ]+\\)>\\'" string)
    (concat " *navbar " (match-string 1 string) "*")))

(defun navbar-get-buffer (&optional frame)
  (let* ((name (navbar-get-buffer-name frame))
	 (buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
	(setq mode-line-format nil)
	(setq cursor-type nil)
	(setq truncate-lines t)
	(setq window-size-fixed 'height)
	(use-local-map navbar-base-map)
	(buffer-face-set 'navbar)))
    buffer))

(defvar navbar-display-table
  (let ((table (make-display-table)))
    ;; Don't display the truncation indicator `$'
    (set-display-table-slot table 'truncation ?\n)
    table)
  "Display table for navbar.el.")

(defun navbar-get-window (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (let* ((buffer (navbar-get-buffer frame))
	   (window (display-buffer-in-side-window
		    buffer '((side . top) (window-height . 1)))))
      (set-window-fringes window 0 0)
      (set-window-display-table window navbar-display-table)
      (set-window-parameter window 'delete-window 'ignore)
      (set-window-parameter window 'no-other-window t)
      (set-window-parameter window 'no-delete-other-windows t)
      (set-window-parameter window 'navbar-get-window t)
      window))
  (get-buffer-window (navbar-get-buffer-name frame) frame))

(defun navbar-kill-buffer-and-window (&optional frame)
  (unless frame
    (setq frame (selected-frame)))
  (let ((window (navbar-get-window frame))
	(buffer (navbar-get-buffer frame)))
    (when window
      (navbar-delete-side-window window))
    (when buffer
      (kill-buffer buffer))))

(defun navbar-frame-list ()
  "Return `frame-list', except child-frame.

Ref: 29.14 Child Frames
  To create a new child frame or to convert a normal frame
  into a child frame, set that frame's parent-frame parameter"
  (delq nil
        (mapcar (lambda (frame)
                  (unless (frame-parameter frame 'parent-frame)
                    frame))
                (frame-list))))

;;; Advices

(defvar navbar-advice-list '((next-window . navbar-advice-next-window)
                             (window-list . navbar-advice-window-list)))

(defun navbar-advice-next-window (fn &rest args)
  (let* ((ret (apply fn args)))
    (while (string-match " \\*navbar " (buffer-name (window-buffer ret)))
      (setq ret (apply fn `(,ret ,(nth 1 args) ,(nth 2 args)))))
    ret))

(defun navbar-advice-window-list (fn &rest args)
  (delq nil
        (mapcar
         (lambda (elm)
           (unless (string-match " \\*navbar " (buffer-name (window-buffer elm)))
             elm))
         (apply fn args))))

(defun navbar-advice-add ()
  (mapc (lambda (pair)
          (eval `(advice-add ',(car pair) :around #',(cdr pair))))
        navbar-advice-list))

(defun navbar-advices-remove ()
  (mapc (lambda (pair)
          (eval `(advice-remove ',(car pair) ',(cdr pair))))
        navbar-advice-list))

;;; Minor mode

(defun navbar-setup ()
  (navbar-advice-add)
  (add-hook 'after-make-frame-functions #'navbar-update)
  (add-hook 'after-make-frame-functions #'navbar-get-window)
  (add-hook 'window-size-change-functions #'navbar-update)
  (mapc #'navbar-get-window (navbar-frame-list))
  (navbar-initialize)
  (mapc #'navbar-update (navbar-frame-list))
  (font-lock-add-keywords 'emacs-lisp-mode navbar-font-lock-keywords))

(defun navbar-teardown ()
  (navbar-deinitialize)
  (navbar-advices-remove)
  (remove-hook 'after-make-frame-functions #'navbar-update)
  (remove-hook 'after-make-frame-functions #'navbar-get-window)
  (remove-hook 'winDow-size-change-functions #'navbar-update)
  (mapc 'navbar-kill-buffer-and-window (navbar-frame-list))
  (font-lock-remove-keywords 'emacs-lisp-mode navbar-font-lock-keywords))

;;;###autoload
(define-minor-mode navbar-mode nil
  :global t
  :group 'navbar
  :lighter navbar-lighter
  (setq navbar-init-frg t)
  (if navbar-mode
      (navbar-setup)
    (navbar-teardown)))

;;; Workarounds

;; (defun navbar-revive-workaround ()
;;   (with-eval-after-load 'revive
;;     (defun revive:window-list ()
;;       (window-list nil 'no-minibuf (navbar-get-window)))
;;     (defun construct-window-configuration (edgelist)
;;       (delete-other-windows)
;;       (revive:restore-winconf
;;        0 1 (revive:screen-width) (1- (revive:screen-height))
;;        edgelist))))

(provide 'navbar)
;;; navbar.el ends here
