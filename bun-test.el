;;; bun-test.el --- Run your bun workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shane Kennedy

;; Author: Shane Kennedy
;; Homepage: https://github.com/shaneikennedy/bun.el
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
;; Functions for testing bun packages.

;;; Code:
(require 'bun-common)

(transient-define-prefix bun-test-menu ()
  "Open bun test transient menu pop up."
  []
  [["Command"
    ("t" "Test current project" bun-test)]]
  (interactive)
  (transient-setup 'bun-test-menu))


(defconst bun-test--prefix-command "bun test")

(defun bun-test-menu-arguments nil
  "Arguments function for transient."
  (transient-args 'bun-test-menu))

;;;###autoload
(defun bun-test (&optional args)
  "Invoke the compile mode with the test prefix-command and ARGS if provided."
  (interactive (list (bun-test-menu-arguments)))
  (let* ((arguments (string-join args " ")))
    (bun-common--compile bun-test--prefix-command arguments)))

(provide 'bun-test)
;;; bun-test.el ends here
