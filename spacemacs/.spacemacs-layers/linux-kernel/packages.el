;;; packages.el --- linux-kernel layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Mattijs Korpershoek <mkorpershoek@baylibre.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Spacemacs layer for linux kernel development

;;; Code:

(defconst linux-kernel-packages
  '((magit-b4
     :location (recipe
                :fetcher github
                :repo JulienMasson/magit-b4))
    dtrt-indent
    dts-mode
    kconfig-mode))

(defun linux-kernel/init-magit-b4 ()
  (use-package magit-b4
    :after magit
    :init
    (define-key magit-mode-map "#" 'magit-b4-dispatch)))

(defun linux-kernel/post-init-dtrt-indent ()
  (use-package dtrt-indent
    :hook ((c-mode-common . dtrt-indent-mode)
           (dts-mode . dtrt-indent-mode)
           (makefile-mode . dtrt-indent-mode))))

(defun linux-kernel/init-dts-mode()
  (use-package dts-mode))

(defun linux-kernel/init-kconfig-mode()
  (use-package kconfig-mode))
