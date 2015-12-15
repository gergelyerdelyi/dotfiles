;;; auto-font-size.el - Automatic font sizing based on OS and screen size
;; Copyright (C) 2015 Gergely Erdélyi

;; Author: Gergely Erdélyi <gergely@erdelyi.hu>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defun font-for-mac-screen-size ()
  (cond
   ((>= (x-display-mm-width) 700) "Hack 20")
   ((< (x-display-mm-width) 400) "Hack 14")))

(defun font-for-linux-screen-size ()
  (cond
   ((>= (x-display-mm-width) 600) "Hack 14")
   ((< (x-display-mm-width) 600) "Hack 12")))

(defun font-for-screen ()
  (cond
   ((eq system-type 'darwin)
    (font-for-mac-screen-size))
   ((eq system-type 'gnu/linux)
    (font-for-linux-screen-size))))

(defun set-frame-font (frame)
  (if window-system
      (let ((ideal-font (font-for-screen)))
        (if ideal-font
            (set-frame-parameter frame 'font ideal-font)
          (display-warning :warning
                           (format "Can't set font size for display width %d mm" (x-display-mm-width)))))))

(defun reset-font ()
  (interactive)
  (set-frame-font nil))

(provide 'auto-font-size)
