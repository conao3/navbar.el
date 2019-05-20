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

(defface navbar-item-version
  '((t :inherit navbar-item))
  "Face of `navbar-item-version'."
  :group 'navbar)

(navbar-define-item navbar-item-version
  "Navbar item for `navbar-version'."
  :value (propertize
	  "\u00bb\u00bb"
	  'face 'navbar-item-version
	  'help-echo (concat "navbar " navbar-version)))


;;; time

;; This navbar item displays `display-time-string' in the navbar buffer
;; instead of the mode line.

(defface navbar-item-time
  '((t :inherit navbar-item))
  "Face of `navbar-item-time'."
  :group 'navbar)

(defun navbar-item-time-get ()
  (and display-time-string
       (list (substring-no-properties display-time-string)
	     :padding navbar-item-padding
	     :propertize '(face navbar-item-time))))

(defun navbar-item-time-on ()
  (setq global-mode-string
	(delq 'display-time-string global-mode-string))
  (add-hook 'display-time-hook #'navbar-item-time-update)
  (display-time-update))

(defun navbar-item-time-off ()
  (navbar-item-time-update)
  (remove-hook 'display-time-hook #'navbar-item-time-update))

;;;###autoload (autoload 'navbar-item-time "navbar-item-time")
(navbar-define-item navbar-item-time
  "Navbar item for `display-time-mode' support."
  :get #'navbar-item-time-get
  :mode display-time-mode
  :mode-on #'navbar-item-time-on
  :mode-off #'navbar-item-time-off)


;;; mew

(defface navbar-item-mew
  '((t :inherit navbar-item))
  "Face of `navbar-item-mew'."
  :group 'navbar)

(defvar navbar-item-mew-biff-function-original nil)

(defun navbar-item-mew-biff-function (n)
  (funcall navbar-item-mew-biff-function-original n)
  (navbar-item-mew-update))

(defun navbar-item-mew-get ()
  (and mew-biff-string
       (list mew-biff-string
	     :padding-left navbar-item-padding
	     :propertize '(face navbar-item-mew))))

(defun navbar-item-mew-delete-mode-string ()
  (setq global-mode-string
	(assq-delete-all 'mew-biff-string global-mode-string)))

(defun navbar-item-mew-on ()
  (setq navbar-item-mew-biff-function-original mew-biff-function)
  (setq mew-biff-function #'navbar-item-mew-biff-function)
  (add-hook 'mew-status-update-hook #'navbar-item-mew-delete-mode-string)
  (add-hook 'mew-pop-sentinel-hook #'navbar-item-mew-update)
  (add-hook 'mew-imap-sentinel-hook #'navbar-item-mew-update))

(defun navbar-item-mew-off ()
  (setq mew-biff-function navbar-item-mew-biff-function-original)
  (remove-hook 'mew-status-update-hook #'navbar-item-mew-delete-mode-string)
  (remove-hook 'mew-pop-sentinel-hook #'navbar-item-mew-update)
  (remove-hook 'mew-imap-sentinel-hook #'navbar-item-mew-update))

;;;###autoload (autoload 'navbar-item-mew "navbar-item-mew")
(navbar-define-item navbar-item-mew
  "Navbar item for Mew support."
  :enable mew-init-p
  :get #'navbar-item-mew-get
  :initialize #'navbar-item-mew-on
  :deinitialize #'navbar-item-mew-off
  :hooks (list (cons 'mew-init-hook #'navbar-item-mew-on)
	       (cons 'mew-quit-hook #'navbar-item-mew-off)))


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

(defcustom navbar-item-elscreen-tab-truncate 16
  "Width to truncate the tab body."
  :type 'integer
  :group 'navbar)

(defcustom navbar-item-elscreen-tab-body-format
  (concat "%s\u00bb" navbar-item-padding "%n")
  "String to be formatted using `format-spec' and shown in the tab body.

The following characters are replaced:
%s:	screen number
%n:	screen name"
  :type 'string
  :group 'elscreen)

(defface navbar-item-elscreen-tab-previous-screen
  '((t :inherit elscreen-tab-other-screen-face))
  "Face for the previous screen."
  :group 'navbar)

(defun navbar-item-elscreen-screen-command (command)
  (lambda (event)
    (interactive "e")
    (let* ((position (event-start event))
	   (point (posn-point position))
	   (window (posn-window position))
	   (screen (navbar-property-at point 'navbar-item-elscreen-screen window)))
      (funcall command screen))))

(defun navbar-item-elscreen-kill-screen-help (window _object pos)
  (let ((screen (navbar-property-at pos 'navbar-item-elscreen-screen window)))
    (format
     "mouse-1: kill screen %d, M-mouse-1: kill screen %d and buffers on it"
     screen screen)))

(defvar navbar-item-elscreen-kill-screen-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") (navbar-item-elscreen-screen-command
				       'elscreen-kill))
    (define-key map (kbd "M-<mouse-1>") (navbar-item-elscreen-screen-command
					 'elscreen-kill-screen-and-buffers))
    map))

(defvar navbar-item-elscreen-tab-body-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") (navbar-item-elscreen-screen-command
				       'elscreen-goto))
    map))

(defvar navbar-item-elscreen-kill-screen
  (concat (propertize "[\u00d7]"
		      'keymap navbar-item-elscreen-kill-screen-map
		      'help-echo 'navbar-item-elscreen-kill-screen-help)
	  ;; Reset the keymap for the right item
	  (propertize " " 'display '(space :width 0))))

(defun navbar-item-elscreen-get (&optional force)
  (if (and (not (window-minibuffer-p))
	   ;; The order is significant
	   (or (elscreen-screen-modified-p 'navbar-item-elscreen-get)
	       force))
      (navbar-item-elscreen--get)
    'unchanged))

(defun navbar-item-elscreen--get ()
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
			  'navbar-item-elscreen-tab-previous-screen)
			 (t
			  'elscreen-tab-other-screen-face)))
	      (tab-body (list (format-spec navbar-item-elscreen-tab-body-format
					   `((?s . ,screen)
					     (?n . ,screen-name)))
			      :truncate navbar-item-elscreen-tab-truncate
			      :propertize
			      (list 'help-echo screen-name
				    'keymap navbar-item-elscreen-tab-body-map)
			      :padding navbar-item-padding)))
	 (list (cond
		((not elscreen-tab-display-kill-screen)
		 (list tab-body))
		((eq elscreen-tab-display-kill-screen 'right)
		 (list tab-body navbar-item-elscreen-kill-screen))
		(t
		 (list navbar-item-elscreen-kill-screen tab-body)))
	       :propertize (list 'face tab-face
				 'pointer 'hand
				 'navbar-item-elscreen-screen screen))))
     screen-list)))

(defun navbar-item-elscreen-on ()
  (if elscreen-frame-confs
      (navbar-item-elscreen--on)
    (defadvice elscreen-start (after navbar-item-elscreen-start activate)
      (navbar-item-elscreen--on))))

(defun navbar-item-elscreen--on ()
  (remove-hook 'elscreen-screen-update-hook 'elscreen-tab-update)
  (add-hook 'elscreen-screen-update-hook #'navbar-item-elscreen-update)
  ;; When `elscreen-start' is run after `navbar-mode' is enabled,
  ;; hook functions of `elscreen-screen-update-hook' have already run
  ;; by `elscreen-make-frame-confs' before adding `navbar-item-elscreen-update'
  ;; to `elscreen-screen-update-hook' by `navbar-item-elscreen-on',
  ;; thus it is necessary to run `navbar-item-elscreen-update' here.
  ;;
  ;; Moreover, successive call of `navbar-item-elscreen-get' returns `unchanged'
  ;; because of `elscreen-screen-modified-p', thus it is necessary to
  ;; get ElScreen tabs forcibly.
  (navbar-item-elscreen-update 'force))

(defun navbar-item-elscreen-off ()
  (ignore-errors
    (ad-remove-advice 'elscreen-start 'after 'navbar-item-elscreen-start)
    (ad-update 'elscreen-start))

  (add-hook 'elscreen-screen-update-hook 'elscreen-tab-update)
  (remove-hook 'elscreen-screen-update-hook #'navbar-item-elscreen-update))

;;;###autoload (autoload 'navbar-item-elscreen "navbar-item-elscreen")
(navbar-define-item navbar-item-elscreen
  "Navbar item for ElScreen support."
  :enable t
  :get #'navbar-item-elscreen-get
  :initialize #'navbar-item-elscreen-on
  :deinitialize #'navbar-item-elscreen-off)

(provide 'navbar-items)
;;; navbar-items.el ends here
