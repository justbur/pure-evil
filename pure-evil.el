;;; pure-evil.el --- Emacs mode that purifies evil-mode

;; Copyright (C) 2015-2016 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; Package-Requires: ((evil "1.2.3"))
;; Homepage: https://github.com/justbur/pure-evil
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'evil)

;; Right now we just modify the definition of `evil-state-keymaps'. TODO: Figure
;; out a cleaner way to do this
(defun pure-evil-state-keymaps (state &rest excluded)
  "Return a keymap alist of keymaps activated by STATE.
If STATE references other states in its :enable property,
these states are recursively processed and added to the list.
\(The EXCLUDED argument is an internal safeguard against
infinite recursion, keeping track of processed states.)"
  (let* ((state (or state evil-state))
         (enable (evil-state-property state :enable))
         (map (cons
               (evil-state-property state :mode)
               (evil-state-property state :keymap t)))
         (result '(nil))
         (remove-duplicates (null excluded)))
    (unless (memq state enable)
      (setq enable (cons state enable)))
    ;; process STATE's :enable property
    (dolist (entry enable)
      (cond
       ((memq entry excluded))
       ;; the keymaps for STATE
       ((eq entry state)
        (setq result `(,@result
                       (,map)))
        (push state excluded))
       ;; the keymaps for another state: call `evil-state-keymaps'
       ;; recursively, but keep track of processed states
       ((evil-state-p entry)
        (setq result `(,@result
                       ,(apply #'evil-state-keymaps entry excluded))))
       ;; a single keymap
       ((or (keymapp entry)
            (and (keymapp (symbol-value entry))
                 (setq entry (symbol-value entry)))
            (setq entry (evil-keymap-for-mode entry)))
        (setq result `(,@result
                       ((,(evil-mode-for-keymap entry t) .
                         ,entry)))))))
    ;; postpone the expensive filtering of duplicates to the top level
    (if remove-duplicates
        (apply #'evil-concat-keymap-alists result)
      (apply #'append result))))

;;;###autoload
(define-minor-mode pure-evil-mode
  "Mode that removes any overriding evil maps in any buffer."
  :lighter "pure-evil"
  (if pure-evil-mode
      (progn
        (advice-add #'evil-state-keymaps :override #'pure-evil-state-keymaps)
        (evil-normalize-keymaps))
    (advice-remove #'evil-state-keymaps #'pure-evil-state-keymaps)
    (evil-normalize-keymaps)))

(provide 'pure-evil)
;;; pure-evil.el ends here
