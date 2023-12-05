;;; current-window-only.el --- Open things only in the current window   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Jakub Kadlčík

;; Author: Jakub Kadlčík <frostyx@email.cz>
;; URL: https://github.com/FrostyX/current-window-only
;; Version: 1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: frames

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Open things only in the current window. No other windows, no splits.


;;; Code:

;;;; Customization

(defgroup current-window-only nil
  "Open things only in the current window. No other windows, no splits."
  :prefix "current-window-only-"
  :group 'frames)

;;;; Modes

;;;###autoload
(define-minor-mode current-window-only-mode
  "Open things only in the current window. No other windows, no splits."
  :global t
  (if current-window-only-mode
      (current-window-only--on)
    (current-window-only--off)))

;;;; Variables

;; We want to rember the variable values that user had before activating the
;; `current-window-only-mode' in case he decides to disable it. In that case
;; we want to set the variables to their previous values instead of the
;; default ones.
(defvar current-window-only--old-config '())

;;;; Functions

;;;;; Private

(defun current-window-only--on ()
  ;; Remember the user configuration in case we need to restore it
  (dolist (var '(display-buffer-overriding-action))
    (when (boundp var)
      (setf (alist-get var current-window-only--old-config)
            (symbol-value var))))

  (setq display-buffer-overriding-action
        '((display-buffer-reuse-window
           display-buffer-same-window)))

  ;; The `org-agenda', `org-capture', and probably all commands with the
  ;; similar input field that expects one character and blocks all other input,
  ;; don't respect the `display-buffer-alist' variable and appear in some other
  ;; window. Overriding `switch-to-buffer-other-window' helps.
  (advice-add
   'switch-to-buffer-other-window
   :override #'current-window-only--switch-to-buffer-other-window)

  ;; Commands like `org-agenda' and `org-capture' temporarily maximize some
  ;; window, change layout, and/or close all other windows when they are
  ;; finished. This prevents them from doing so.
  (advice-add
   'delete-other-windows
   :override #'current-window-only--delete-other-windows))

(defun current-window-only--off ()
  (dolist (item current-window-only--old-config)
    (set (car item) (cdr item)))

  (advice-remove
   'switch-to-buffer-other-window
   #'current-window-only--switch-to-buffer-other-window)

  (advice-remove
   'delete-other-windows
   #'current-window-only--delete-other-windows))

(defun current-window-only--switch-to-buffer-other-window
    (buffer-or-name &optional norecord)
  (switch-to-buffer buffer-or-name norecord t))

(defun current-window-only--delete-other-windows
    (&optional window interactive)
  (ignore window)
  (ignore interactive))

;;;; Footer

(provide 'current-window-only)

;;; current-window-only.el ends here
