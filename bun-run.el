;;; bun-run.el --- Run your bun workflows -*- lexical-binding: t; -*-

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
;; Functions for using bun run.

;;; Code:
(require 'bun-common)

(defconst bun-run--prefix-command "bun run")

(defun bun-run--get-run-command (script-name)
  "Construct the shell command for a given SCRIPT-NAME."
  (concat bun-run--prefix-command " " script-name))

(defun bun-run--get-scripts (project-dir)
  "Function to parse package.json in the PROJECT-DIR to find bun scripts."
  (mapcar 'car (cdr (assoc 'scripts (json-read-file (concat project-dir bun-common--config-file))))))


(defun bun-run--choose-script ()
  "Let user choose which script to run."
  (interactive)
  (completing-read "Select script from list: " (bun-run--get-scripts (bun-common--get-project-dir)) nil t))

;;;###autoload
(defun bun-run (&optional _args)
  "Invoke the compile mode with the run prefix-command and ARGS if provided."
  (interactive (list (bun-common--arguments)))
  (bun-common--compile (bun-run--get-run-command (bun-run--choose-script))))

(provide 'bun-run)
;;; bun-run.el ends here
