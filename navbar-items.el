;;; navbar-items.el --- items for navbar.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; navbar items

;;; Code:

(require 'navbar)
(require 'time)
(require 'format-spec)
(require 'mew)
(require 'elscreen)


;;; version

(defface navbarx-version
  '((t :inherit navbar-item))
  "Face of `navbarx-version'."
  :group 'navbar)

(navbar-define-item navbarx-version
  "Navbar item for `navbar-version'."
  :value (propertize
	  "\u00bb\u00bb"
	  'face 'navbarx-version
	  'help-echo (concat "navbar " navbar-version)))


;;; time

;; This navbar item displays `display-time-string' in the navbar buffer
;; instead of the mode line.

(defface navbarx-time
  '((t :inherit navbar-item))
  "Face of `navbarx-time'."
  :group 'navbar)

(defun navbarx-time-get ()
  (and display-time-string
       (list (substring-no-properties display-time-string)
	     :padding navbar-item-padding
	     :propertize '(face navbarx-time))))

(defun navbarx-time-on ()
  (setq global-mode-string
	(delq 'display-time-string global-mode-string))
  (add-hook 'display-time-hook #'navbarx-time-update)
  (display-time-update))

(defun navbarx-time-off ()
  (navbarx-time-update)
  (remove-hook 'display-time-hook #'navbarx-time-update))

;;;###autoload (autoload 'navbarx-time "navbarx-time")
(navbar-define-item navbarx-time
  "Navbar item for `display-time-mode' support."
  :get #'navbarx-time-get
  :mode display-time-mode
  :mode-on #'navbarx-time-on
  :mode-off #'navbarx-time-off)


;;; mew

(defface navbarx-mew
  '((t :inherit navbar-item))
  "Face of `navbarx-mew'."
  :group 'navbar)

(defvar navbarx-mew-biff-function-original nil)

(defun navbarx-mew-biff-function (n)
  (funcall navbarx-mew-biff-function-original n)
  (navbarx-mew-update))

(defun navbarx-mew-get ()
  (and mew-biff-string
       (list mew-biff-string
	     :padding-left navbar-item-padding
	     :propertize '(face navbarx-mew))))

(defun navbarx-mew-delete-mode-string ()
  (setq global-mode-string
	(assq-delete-all 'mew-biff-string global-mode-string)))

(defun navbarx-mew-on ()
  (setq navbarx-mew-biff-function-original mew-biff-function)
  (setq mew-biff-function #'navbarx-mew-biff-function)
  (add-hook 'mew-status-update-hook #'navbarx-mew-delete-mode-string)
  (add-hook 'mew-pop-sentinel-hook #'navbarx-mew-update)
  (add-hook 'mew-imap-sentinel-hook #'navbarx-mew-update))

(defun navbarx-mew-off ()
  (setq mew-biff-function navbarx-mew-biff-function-original)
  (remove-hook 'mew-status-update-hook #'navbarx-mew-delete-mode-string)
  (remove-hook 'mew-pop-sentinel-hook #'navbarx-mew-update)
  (remove-hook 'mew-imap-sentinel-hook #'navbarx-mew-update))

;;;###autoload (autoload 'navbarx-mew "navbarx-mew")
(navbar-define-item navbarx-mew
  "Navbar item for Mew support."
  :enable mew-init-p
  :get #'navbarx-mew-get
  :initialize #'navbarx-mew-on
  :deinitialize #'navbarx-mew-off
  :hooks (list (cons 'mew-init-hook #'navbarx-mew-on)
	       (cons 'mew-quit-hook #'navbarx-mew-off)))


;;; elscreen

;; This navbar item displays tabs of ElScreen in the navbar buffer
;; instead of the header line.
;;
;; This uses the following faces and variable of ElScreen for the look
;; and feel of tabs:
;;
;; * `elscreen-tab-current-screen-face'
;; * `elscreen-tab-other-screen-face'
;; * `elscreen-tab-display-kill-screen'

(defcustom navbarx-elscreen-tab-truncate 16
  "Width to truncate the tab body."
  :type 'integer
  :group 'navbar)

(defcustom navbarx-elscreen-tab-body-format
  (concat "%s\u00bb" navbar-item-padding "%n")
  "String to be formatted using `format-spec' and shown in the tab body.

The following characters are replaced:
%s:	screen number
%n:	screen name"
  :type 'string
  :group 'elscreen)

(defface navbarx-elscreen-tab-previous-screen
  '((t :inherit elscreen-tab-other-screen-face))
  "Face for the previous screen."
  :group 'navbar)

(defun navbarx-elscreen-screen-command (command)
  (lambda (event)
    (interactive "e")
    (let* ((position (event-start event))
	   (point (posn-point position))
	   (window (posn-window position))
	   (screen (navbar-property-at point 'navbarx-elscreen-screen window)))
      (funcall command screen))))

(defun navbarx-elscreen-kill-screen-help (window _object pos)
  (let ((screen (navbar-property-at pos 'navbarx-elscreen-screen window)))
    (format
     "mouse-1: kill screen %d, M-mouse-1: kill screen %d and buffers on it"
     screen screen)))

(defvar navbarx-elscreen-kill-screen-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") (navbarx-elscreen-screen-command
				       'elscreen-kill))
    (define-key map (kbd "M-<mouse-1>") (navbarx-elscreen-screen-command
					 'elscreen-kill-screen-and-buffers))
    map))

(defvar navbarx-elscreen-tab-body-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") (navbarx-elscreen-screen-command
				       'elscreen-goto))
    map))

(defvar navbarx-elscreen-kill-screen
  (concat (propertize "[\u00d7]"
		      'keymap navbarx-elscreen-kill-screen-map
		      'help-echo 'navbarx-elscreen-kill-screen-help)
	  ;; Reset the keymap for the right item
	  (propertize " " 'display '(space :width 0))))

(defun navbarx-elscreen-get (&optional force)
  (if (and (not (window-minibuffer-p))
	   ;; The order is significant
	   (or (elscreen-screen-modified-p 'navbarx-elscreen-get)
	       force))
      (navbarx-elscreen--get)
    'unchanged))

(defun navbarx-elscreen--get ()
  (let ((screen-list (sort (elscreen-get-screen-list) '<))
	(screen-to-name-alist (elscreen-get-screen-to-name-alist))
	(current-screen (elscreen-get-current-screen))
	(previous-screen (elscreen-get-previous-screen))
	deactivate-mark)
    (mapcar
     (lambda (screen)
       (let* ((screen-name (cdr (assq screen screen-to-name-alist)))
	      (tab-face (cond
			 ((= screen current-screen)
			  'elscreen-tab-current-screen-face)
			 ((= screen previous-screen)
			  'navbarx-elscreen-tab-previous-screen)
			 (t
			  'elscreen-tab-other-screen-face)))
	      (tab-body (list (format-spec navbarx-elscreen-tab-body-format
					   `((?s . ,screen)
					     (?n . ,screen-name)))
			      :truncate navbarx-elscreen-tab-truncate
			      :propertize
			      (list 'help-echo screen-name
				    'keymap navbarx-elscreen-tab-body-map)
			      :padding navbar-item-padding)))
	 (list (cond
		((not elscreen-tab-display-kill-screen)
		 (list tab-body))
		((eq elscreen-tab-display-kill-screen 'right)
		 (list tab-body navbarx-elscreen-kill-screen))
		(t
		 (list navbarx-elscreen-kill-screen tab-body)))
	       :propertize (list 'face tab-face
				 'pointer 'hand
				 'navbarx-elscreen-screen screen))))
     screen-list)))

(defun navbarx-elscreen-on ()
  (if elscreen-frame-confs
      (navbarx-elscreen--on)
    (defadvice elscreen-start (after navbarx-elscreen-start activate)
      (navbarx-elscreen--on))))

(defun navbarx-elscreen--on ()
  (remove-hook 'elscreen-screen-update-hook 'elscreen-tab-update)
  (add-hook 'elscreen-screen-update-hook #'navbarx-elscreen-update)
  ;; When `elscreen-start' is run after `navbar-mode' is enabled,
  ;; hook functions of `elscreen-screen-update-hook' have already run
  ;; by `elscreen-make-frame-confs' before adding `navbarx-elscreen-update'
  ;; to `elscreen-screen-update-hook' by `navbarx-elscreen-on',
  ;; thus it is necessary to run `navbarx-elscreen-update' here.
  ;;
  ;; Moreover, successive call of `navbarx-elscreen-get' returns `unchanged'
  ;; because of `elscreen-screen-modified-p', thus it is necessary to
  ;; get ElScreen tabs forcibly.
  (navbarx-elscreen-update 'force))

(defun navbarx-elscreen-off ()
  (ignore-errors
    (ad-remove-advice 'elscreen-start 'after 'navbarx-elscreen-start)
    (ad-update 'elscreen-start))

  (add-hook 'elscreen-screen-update-hook 'elscreen-tab-update)
  (remove-hook 'elscreen-screen-update-hook #'navbarx-elscreen-update))

;;;###autoload (autoload 'navbarx-elscreen "navbarx-elscreen")
(navbar-define-item navbarx-elscreen
  "Navbar item for ElScreen support."
  :enable t
  :get #'navbarx-elscreen-get
  :initialize #'navbarx-elscreen-on
  :deinitialize #'navbarx-elscreen-off)

(provide 'navbar-items)
;;; navbar-items.el ends here
