;;; bun-common.el --- Run your bun workflows -*- lexical-binding: t; -*-

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
;; The common functions needed by many or all bun commands.

;;; Code:
(require 'compile)
(require 'json)
(require 'subr-x)
(require 'transient)

(defgroup bun ()
  "Group for bun."
  :group 'tools
  :prefix "bun-")

(defconst bun-common--config-file "package.json")

(defcustom bun-common-buffer-name-function "*bun*"
  "Buffer name for `bun' command, or function which return buffer name.
The function takes three arguments, ROOT, BUN-COMMAND, ARGS.
ROOT is project root directory.  BUN-COMMAND is bun command string.
ARGS is list of arguments passed to bun command.

You can use `bun-common-create-unique-buffer-name' to use unique buffer name
among all sesstions."
  :group 'bun
  :type '(choice
          (string :tag "Use same buffer through all sessions")
          (const :tag "Use unique buffer name among all sessions" bun-common-create-unique-buffer-name)
          function))

(defun bun-common-create-unique-buffer-name (root bun-command _args)
  "Create buffer name unique to ROOT and BUN-COMMAND."
  (concat "*" bun-command " in " root "*"))

(defun bun-common--generate-buffer-name-function (root bun-command args)
  "Generate function which return buffer name to pass `compilation-start'.
ROOT is project root directory.  BUN-COMMAND is bun command string.
ARGS is list of arguments passed to bun command.

This function uses `bun-common-buffer-name-function'."
  (lambda (_)
    (if (stringp bun-common-buffer-name-function)
        bun-common-buffer-name-function
      (funcall bun-common-buffer-name-function
               root bun-command args))))

;; Common
(defun bun-common--get-prorect-dir ()
  "Function to determine the file path of the project root directory."
  (message (locate-dominating-file (or (buffer-file-name) default-directory)
                                   bun-common--config-file)))

(defun bun-common--compile (bun-command &optional args)
  "Generic compile command for BUN-COMMAND with ARGS functionality."
  (compilation-start (string-join (list bun-command args) " ")
                     'bun-mode
                     (bun-common--generate-buffer-name-function
                      (bun-common--get-project-dir) bun-command args)))

(defun bun-common--arguments nil
  "Arguments function for transient."
  (transient-args 'bun-menu))

(provide 'bun-common)
;;; bun-common.el ends here
