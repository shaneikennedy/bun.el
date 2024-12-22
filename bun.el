;;; bun.el --- Run your bun workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shane Kennedy

;; Author: Shane Kennedy
;; Homepage: https://github.com/shaneikennedy/bun.el
;; Package-Requires: ((emacs "26.1") (transient "0.1.0"))
;; Keywords: tools
;; Version: 0

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
;; This package offers a transient interface to the bun cli.

;;; Code:
(require 'bun-common)
(require 'bun-run)
(require 'bun-install)
(require 'bun-update)
(require 'bun-init)
(require 'bun-test)

(defconst bun-mode-map compilation-mode-map)

(define-derived-mode bun-mode compilation-mode "bun"
  "Major mode for the bun compilation buffer."
  (use-local-map bun-mode-map)
  (setq major-mode 'bun-mode)
  (setq mode-name "bun")
  (setq-local truncate-lines t))

;;;###autoload
(defun bun ()
  "Entrypoint function to the package.
This will first check to make sure there is a package.json file and then open the menu."
  (interactive)
  (if (bun-common--get-project-dir)
      (call-interactively #'bun-menu)
    (if (y-or-n-p "You are not in an bun project, would you like to initialize one? ")
        (call-interactively #'bun-init))))

;; Entrypoint menu
(transient-define-prefix bun-menu ()
  "Open bun transient menu pop up."
  [["Command"
    ("u" "Update"       bun-update)
    ("i" "Install"       bun-install-menu)
    ("t" "Test"       bun-test)
    ("r" "Run"       bun-run)]]
  (interactive)
  (transient-setup 'bun-menu))


(provide 'bun)
;;; bun.el ends here
