;;; vimot.el --- VI-style motion when called up, but only then
;;-------------------------------------------------------------------
;;
;; Copyright (C) 2017 Steven Rich
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------

;; Author: Steven Rich <srichy@mac.com>
;; Created: 01 February 2017
;; Version: 0.1 (2017-02-01)
;; Keywords: keyboard motion

;;; Commentary:
;; (I used key-chord.el as a template for this file)

;; ########   Compatibility   ########################################
;;
;; Developed on Emacs-24.3
;; Others unknown

;; ########   Quick start   ########################################
;;
;; Add to your ~/.emacs
;;
;;	(require 'vimotion)
;;      (vimotion-enable t)
;;      (global-set-key (kbd "C-^") 'vimotion-activate)
;;

;; ########   Description   ########################################
;;
;; vimotion is a essentially a way to transiently "take over" key mapping
;; (by invoking `vimotion-activate') to provide vi-style motion key
;; mappings until any key which is not handled by vimotion is pressed.
;; Once that is done, the previous key mappings are restored and normal
;; Emacs behavior ensues.
;;
;; For example, normal vi 'h', 'j', 'k', and 'l' will allow for character-
;; by-character motion and will remain in vimotion mode.  Any prefix keys
;; (like C-x or C-c) will exit vimotion and pass that key into normal key
;; handling (i.e., it won't be lost).

;;; Code:

;; Internal vars
(defvar vimotion-is-active nil)
(defvar vimotion-cancel-mapping-fn nil)

(defun vimotion-cancel-mapping ()
  "Turn off vi motion.  Needed for 'i', 'a', etc."
  (interactive)
  (funcall vimotion-cancel-mapping-fn)
  )

(defvar vimotion-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'vimotion-cancel-mapping)
    (define-key map (kbd "a") (lambda ()
                                (interactive)
                                (forward-char)
                                (vimotion-cancel-mapping)
                                ))
    (define-key map (kbd "o") (lambda ()
                                (interactive)
                                (forward-line)
                                (open-line 1)
                                (vimotion-cancel-mapping)
                                ))
    (define-key map (kbd "h") 'backward-char)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "l") 'forward-char)
    (define-key map (kbd "e") (lambda()
                                (interactive)
                                (forward-char)
                                (forward-word)
                                (backward-char)
                                ))
    (define-key map (kbd "w") (lambda ()
                                 (interactive)
                                 (backward-char)
                                 (forward-word)
                                 (forward-word)
                                 (backward-word)
                                 ))
    (define-key map (kbd "b") 'backward-word)
    (define-key map (kbd "$") 'move-end-of-line)
    (define-key map (kbd "^") 'back-to-indentation)
    (define-key map (kbd "0") 'move-beginning-of-line)
    (define-key map (kbd "G") 'end-of-buffer)
    (define-key map (kbd "gg") 'beginning-of-buffer)
    (define-key map (kbd "C-f") 'scroll-up-command)
    (define-key map (kbd "C-b") 'scroll-down-command)
    (define-key map (kbd "C-u") 'scroll-down-command)
    (define-key map (kbd "C-d") 'scroll-up-command)
    (define-key map (kbd "C-e") 'scroll-up-line)
    (define-key map (kbd "C-y") 'scroll-down-line)
    (define-key map (kbd ">") 'scroll-up-line)
    (define-key map (kbd "<") 'scroll-down-line)
    (define-key map (kbd "{") 'backward-paragraph)
    (define-key map (kbd "}") 'forward-paragraph)
    (define-key map (kbd "(") 'backward-sentence)
    (define-key map (kbd ")") 'forward-sentence)
    (define-key map (kbd "H") '(lambda () (interactive)(move-to-window-line 0)))
    (define-key map (kbd "M") 'move-to-window-line)
    (define-key map (kbd "L") '(lambda () (interactive)(move-to-window-line -1)))
    (define-key map (kbd "[[") 'backward-list) ;; not exactly right, but close
    (define-key map (kbd "[]") 'backward-list) ;; not exactly right, but close
    (define-key map (kbd "]]") 'forward-list)  ;; not exactly right, but close
    (define-key map (kbd "][") 'forward-list)  ;; not exactly right, but close
    (define-key map (kbd "/") (lambda () (interactive) (isearch-forward)(vimotion-activate)))
    (define-key map (kbd "?") (lambda () (interactive) (isearch-backward)(vimotion-activate)))
    (define-key map (kbd "n") 'isearch-repeat-forward)
    (define-key map (kbd "N") 'isearch-repeat-backward)
    (define-key map (kbd "*") (lambda () (interactive) (isearch-forward-symbol-at-point)(vimotion-activate)))
    (define-key map (kbd "v") (lambda () (interactive) (if mark-active (deactivate-mark) (set-mark-command nil))))
    (define-key map (kbd "d") (lambda () (interactive) (if mark-active (kill-region (mark) (point)) (message "Motion commands not supported"))))
    (define-key map (kbd "p") 'yank)
    map))

;;;###autoload
(defun vimotion-enable (arg)
  "Enable toggling vimotion"
  (interactive "P")
  (add-to-list 'minor-mode-alist
               '(vimotion-is-active " !VM!"))
  )

(defun vimotion-end ()
  "Exit vimotion mapping"
  (setq vimotion-is-active nil)
  (force-mode-line-update)
  )

;;;###autoload
(defun vimotion-activate ()
  "Turn on vimotion."
  (interactive)
  (setq vimotion-is-active t)
  (force-mode-line-update)
  (setq vimotion-cancel-mapping-fn
        (set-transient-map vimotion-keymap t 'vimotion-end))
  )

(provide 'vimotion)

;;; vimotion.el ends here
