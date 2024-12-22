;;; bun-update.el --- Run your bun workflows -*- lexical-binding: t; -*-

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
;;; Functions for running bun update.

;;; Code:
(require 'bun-common)

(defconst bun-update--prefix-command "bun update")

(defun bun-update--get-update-command (package-name)
  "Construct the shell command for a given PACKAGE-NAME."
  (concat bun-update--prefix-command " " package-name))

(defun bun-update--get-packages (project-dir)
  "Function to parse package.json in the PROJECT-DIR to find bun packages."
  (append
   (bun-update--get-dev-dependency-packages project-dir)
   (bun-update--get-optional-dependency-packages project-dir)
   (bun-update--get-dependency-packages project-dir)))

(defun bun-update--get-dev-dependency-packages(project-dir)
  "Function to parse package.json in the PROJECT-DIR to find bun devDependencies."
  (mapcar 'car (cdr (assoc 'devDependencies (json-read-file (concat project-dir bun-common--config-file))))))

(defun bun-update--get-optional-dependency-packages(project-dir)
  "Function to parse package.json in the PROJECT-DIR to find bun optionalDependencies."
  (mapcar 'car (cdr (assoc 'optionalDependencies (json-read-file (concat project-dir bun-common--config-file))))))

(defun bun-update--get-dependency-packages(project-dir)
  "Function to parse package.json in the PROJECT-DIR to find bun dependencies."
  (mapcar 'car (cdr (assoc 'dependencies (json-read-file (concat project-dir bun-common--config-file))))))

(defun bun-update--choose-package ()
  "Let user choose which package to update."
  (interactive)
  (completing-read "Select package from list: " (bun-update--get-packages (bun-common--get-project-dir)) nil t))

;;;###autoload
(defun bun-update (&optional _args)
  "Invoke the compile mode with the update prefix-command and ARGS if provided."
  (interactive (list (bun-common--arguments)))
  (bun-common--compile (bun-update--get-update-command (bun-update--choose-package))))


(provide 'bun-update)
;;; bun-update.el ends here
