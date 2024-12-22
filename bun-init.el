;;; bun-init.el --- Run your bun workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shane Kennedy

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
;; Functions for initializing a node project.

;;; Code:
(require 'bun-common)

(defconst bun-init--prefix-command "bun init")
(defconst bun-init--temp-buffer ".buninit")

;;;###autoload
(defun bun-init ()
  "Initialize a project folder as a bun project."
  (interactive)
  (save-excursion
    (let* ((project-root-folder (read-directory-name "Project root :"))
           (command bun-init--prefix-command))
      (generate-new-buffer (concat project-root-folder bun-init--temp-buffer))
      (set-buffer (concat project-root-folder bun-init--temp-buffer))
      (let ((current-prefix-arg '(4)))
        (setq compilation-read-command nil)
        (setq compile-command command)
        (call-interactively #'compile))
      (kill-buffer project-root-folder))))

(provide 'bun-init)
;;; bun-init.el ends here
