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
;; Functional on Emacs-29.0.50

;; ########   Quick start   ########################################
;;
;; 1. Put vimot.el in a suitable for Emacs (e.g., ~/.emacs.d)
;; 2. Add to your ~/.emacs.d/init.el (or ~/.emacs, etc.):
;;
;; ;; vimot
;; (load-file "~/.emacs.d/vimot.el")
;; (vimotion-enable t)
;; (global-set-key (kbd "C-^") 'vimotion-activate)
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

;; Count prefix string
(defvar vimotion-accumulator '(nil . 0))

(defun vimotion-reset-accum ()
  (setq vimotion-accumulator '(nil . 0))
  )

(defun vimotion-accum (n)
  "Add number to accumulator"
  (let ((curval (cdr vimotion-accumulator)))
    (setq vimotion-accumulator `(t . ,(+ (* curval 10) n)))
    )
  )

(defun vimotion-have-count ()
  "Return t if there is a definite count"
  (car vimotion-accumulator)
  )

(defun vimotion-count ()
  "Return t if there is a definite count"
  (cdr vimotion-accumulator)
  )

(defun vimotion-cancel-mapping ()
  "Turn off vi motion.  Needed for 'i', 'a', etc."
  (interactive)
  (funcall vimotion-cancel-mapping-fn)
  )

(defmacro vimotion-action (map key actions)
  (list 'define-key map key `(lambda ()
                               (interactive)
                               (setq rval ,actions)
                               (vimotion-reset-accum)
                               rval))
  )

(defvar vimotion-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "1") (lambda () (interactive) (vimotion-accum 1)))
    (define-key map (kbd "2") (lambda () (interactive) (vimotion-accum 2)))
    (define-key map (kbd "3") (lambda () (interactive) (vimotion-accum 3)))
    (define-key map (kbd "4") (lambda () (interactive) (vimotion-accum 4)))
    (define-key map (kbd "5") (lambda () (interactive) (vimotion-accum 5)))
    (define-key map (kbd "6") (lambda () (interactive) (vimotion-accum 6)))
    (define-key map (kbd "7") (lambda () (interactive) (vimotion-accum 7)))
    (define-key map (kbd "8") (lambda () (interactive) (vimotion-accum 8)))
    (define-key map (kbd "9") (lambda () (interactive) (vimotion-accum 9)))

    (vimotion-action map (kbd "i") (vimotion-cancel-mapping))
    (vimotion-action map (kbd "a") (progn
                                     (forward-char)
                                     (vimotion-cancel-mapping)
                                     ))
    (vimotion-action map (kbd "o") (progn
                                     (forward-line)
                                     (open-line 1)
                                     (vimotion-cancel-mapping)
                                     ))
    (vimotion-action map (kbd "h") (backward-char))
    (vimotion-action map (kbd "j") (next-line))
    (vimotion-action map (kbd "k") (previous-line))
    (vimotion-action map (kbd "l") (forward-char))
    (vimotion-action map (kbd "e") (progn
                                     (forward-char)
                                     (forward-word)
                                     (backward-char)
                                     ))
    (vimotion-action map (kbd "w") (progn
                                     (backward-char)
                                     (forward-word)
                                     (forward-word)
                                     (backward-word)
                                     ))
    (vimotion-action map (kbd "b") (backward-word))
    (vimotion-action map (kbd "$") (move-end-of-line 1))
    (vimotion-action map (kbd "^") (back-to-indentation))
    (define-key map (kbd "0") (lambda ()
                                (interactive)
                                (if (vimotion-have-count)
                                    (vimotion-accum 0)
                                  (move-beginning-of-line 1)
                                  (vimotion-reset-accum)
                                  )
                                )
      )
    (vimotion-action map (kbd "G") (if (vimotion-have-count)
                                       (let ((line-num (vimotion-count)))
                                         (goto-line line-num)
                                         )
                                     (end-of-buffer)
                                     ))
    (vimotion-action map (kbd "gg") (beginning-of-buffer))
    (vimotion-action map (kbd "C-f") (scroll-up-command))
    (vimotion-action map (kbd "C-b") (scroll-down-command))
    (vimotion-action map (kbd "C-u") (scroll-down-command))
    (vimotion-action map (kbd "C-d") (scroll-up-command))
    (vimotion-action map (kbd "C-e") (scroll-up-line))
    (vimotion-action map (kbd "C-y") (scroll-down-line))
    (vimotion-action map (kbd ">") (scroll-up-line))
    (vimotion-action map (kbd "<") (scroll-down-line))
    (vimotion-action map (kbd "{") (backward-paragraph))
    (vimotion-action map (kbd "}") (forward-paragraph))
    (vimotion-action map (kbd "(") (backward-sentence))
    (vimotion-action map (kbd ")") (forward-sentence))
    (vimotion-action map (kbd "H") (move-to-window-line 0))
    (vimotion-action map (kbd "M") (move-to-window-line nil))
    (vimotion-action map (kbd "L") (move-to-window-line -1))
    (vimotion-action map (kbd "[[") (backward-list)) ;; not exactly right, but close
    (vimotion-action map (kbd "[]") (backward-list)) ;; not exactly right, but close
    (vimotion-action map (kbd "]]") (forward-list))  ;; not exactly right, but close
    (vimotion-action map (kbd "][") (forward-list))  ;; not exactly right, but close
    (vimotion-action map (kbd "/") (progn
                                     (isearch-forward)
                                     (vimotion-activate)))
    (vimotion-action map (kbd "?") (progn
                                     (isearch-backward)
                                     (vimotion-activate)))
    (vimotion-action map (kbd "n") (isearch-repeat-forward))
    (vimotion-action map (kbd "N") (isearch-repeat-backward))
    (vimotion-action map (kbd "*") (progn
                                     (isearch-forward-symbol-at-point)
                                     (vimotion-activate)))
    (vimotion-action map (kbd "v") (if mark-active
                                       (deactivate-mark)
                                     (set-mark-command nil)))
    (vimotion-action map (kbd "d") (if mark-active
                                       (kill-region (mark) (point))
                                     (message "Motion commands not supported")))
    (vimotion-action map (kbd "p") (yank))
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
