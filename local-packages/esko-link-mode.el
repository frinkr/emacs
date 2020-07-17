;;; esko-link-mode.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  frink

;; Author: frink
;; Keywords: lisp
;; Version: 0.0.1

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

;; Clickable links to Esko websets.

;;; Code:

(require 'goto-addr)
(require 'browse-url)

(setq esko-project-codes '("DPP" "DPI" "JP"))  ;; Esko project code
(setq esko-project-regexp
      (string-join (mapcar (lambda (x) (concat x "-[0-9]+"))
                           esko-project-codes) "\\|"))

;; trap goto-address-url-regexp
(setq goto-address-url-regexp-old goto-address-url-regexp)
(setq goto-address-url-regexp
      (concat
       goto-address-url-regexp-old
       "\\|" esko-project-regexp))

;; trap browse-url-url-at-point
(defadvice browse-url-url-at-point (after fx/browse-url-url-at-point () activate)
  (if (string-match esko-project-regexp ad-return-value)
      (setq ad-return-value
            (concat "http://jira.esko.com/browse/"
                    (progn (string-match esko-project-regexp ad-return-value)
                           (match-string 0 ad-return-value))))
    )
  )

(provide 'esko-link-mode)

;;; esko-link-mode.el ends here
