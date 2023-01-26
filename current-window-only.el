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

;;;; Functions

;;;;; Private

(defun current-window-only--on ()
  (setq Man-notify-method 'pushy)
  (setq org-src-window-setup 'current-window)
  (setq org-agenda-window-setup 'current-window)

  (setq display-buffer-alist
        '((".*" (display-buffer-reuse-window
                 display-buffer-same-window)
           (reusable-frames . t))))

  (advice-add
   'switch-to-buffer-other-window
   :override #'current-window-only--switch-to-buffer-other-window)

  (advice-add
   'delete-other-windows
   :override #'current-window-only--delete-other-windows))

(defun current-window-only--off ()
  (setq Man-notify-method 'friendly)
  (setq org-src-window-setup 'reorganize-frame)
  (setq org-agenda-window-setup 'reorganize-frame)
  (setq display-buffer-alist nil)

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
