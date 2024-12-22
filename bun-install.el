;;; bun-install.el --- Run your bun workflows -*- lexical-binding: t; -*-

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
;; Functions for installing bun packages.

;;; Code:
(require 'bun-common)

(transient-define-prefix bun-install-menu ()
  "Open bun install transient menu pop up."
  ["Arguments"
   ("-f" "Force fetching even if copy exists on disk"        "--force")
   ("-g" "Save as global dependency"        "--global")
   ("-p" "Save as production dependency"        "--production")
   ("-d" "Save as development dependency"        "--dev")
   ("-o" "Save as optional dependency"        "--optional")
   ("-e" "Save exact version"                 "--exact")
   ("-n" "Do not save to package.json"        "--no-save")]
  [["Command"
    ("i" "Install new package"       bun-install)
    ("I" "Install current packages (in package.json)" bun-install-current)]]
  (interactive)
  (transient-setup 'bun-install-menu))


(defconst bun-install--prefix-command "bun install")

(defun bun-install--get-install-command (package-name)
  "Construct the shell command for a given PACKAGE-NAME."
  (concat bun-install--prefix-command " " package-name))

(defun bun-install--choose-package ()
  "Let user choose which package to install."
  (interactive)
  (completing-read "Type the name of the package you want to install: " ()))

(defun bun-install-menu-arguments nil
  "Arguments function for transient."
  (transient-args 'bun-install-menu))

(defun bun-install-current (&optional args)
  "Invoke the compile mode with the install prefix-command and ARGS if provided but no packages."
  (interactive)
  (let ((arguments (string-join args " "))
	(bun-command (bun-install--get-install-command "")))
    (bun-common--compile bun-command arguments)))

;;;###autoload
(defun bun-install (&optional args)
  "Invoke the compile mode with the install prefix-command and ARGS if provided."
  (interactive (list (bun-install-menu-arguments)))
  (let* ((arguments (string-join args " "))
         (bun-command (bun-install--get-install-command (bun-install--choose-package))))
    (bun-common--compile bun-command arguments)))

(provide 'bun-install)
;;; bun-install.el ends here
