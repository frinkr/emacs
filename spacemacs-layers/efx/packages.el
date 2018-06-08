;;; packages.el --- efx layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Yuqing Jiang <frinkr@outlook.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defconst efx-packages
  '(
    fill-column-indicator
    highlight-symbol
    p4
    recentf
    reveal-in-osx-finder
    undo-tree
    ))

(defun efx/post-init-fill-column-indicator()
  (efx/config-fill-column-indicator))


(defun efx/init-highlight-symbol()
  (use-package highlight-symbol
    :config (efx/config-highlight-thing)
    )
  )
(defun efx/post-init-p4()
  (setenv "P4CONFIG" "p4.config"))

(defun efx/post-init-recentf()
  (setq recentf-max-menu-items 2500)
  )

(defun efx/init-reveal-in-osx-finder()
  (use-package reveal-in-osx-finder
    :defer t
    :config (defalias 'open-in-finder 'reveal-in-osx-finder)
    )
  )

(defun efx/post-init-undo-tree()
  (efx/config-undo-tree))

;;; packages.el ends here

