;; -*- lexical-binding: t; -*-

(require 'color)
(defun buffer-modeline-color-hue-for-string (str)
	(let* ((hue (string-to-number (substring (md5 str) 0 8) 16))
				 (hue (/ (mod hue 256) 255.0)))
		hue))

(defun buffer-modeline-color-color-for-string (str sat val)
	(let* ((hue (buffer-modeline-color-hue-for-string str)))
		(apply #'color-rgb-to-hex (color-hsl-to-rgb hue sat val))))

(defvar buffer-modeline-color-active-saturation '(0.6 . 0.5)
  "Saturation for random color of modeline when buffer is active.

This is a cons cell of '(bright . dark)")
;;(setq buffer-modeline-color-active-saturation '(0.6 . 0.5))

(defvar buffer-modeline-color-active-value '(0.8 . 0.2)
  "Value (brightness) for random color of modeline when buffer is active.

This is a cons cell of '(bright . dark)")
;;(setq buffer-modeline-color-active-value '(0.6 . 0.2))

(defvar buffer-modeline-color-inactive-saturation '(0.2 . 0.25)
  "Saturation for random color of modeline when buffer is inactive.

This is a cons cell of '(bright . dark)")
;;(setq buffer-modeline-color-inactive-saturation '(0.4 . 0.25))

(defvar buffer-modeline-color-inactive-value '(0.8 . 0.2)
  "Value (brightness) for random color of modeline when buffer is inactive.

This is a cons cell of '(bright . dark)")
;;(setq buffer-modeline-color-inactive-value '(0.8 . 0.2))

(defvar-local buffer-modeline-color-active-face-remap-token nil
  "The token returned by `face-remap-add-relative' so that face-remappings can be removed.")

(defvar-local buffer-modeline-color-inactive-face-remap-token nil
  "The token returned by `face-remap-add-relative' so that face-remappings can be removed.")

(defun buffer-modeline-color ()
	(let* ((name (or (buffer-file-name) (buffer-name)))
				 (hue (buffer-modeline-color-hue-for-string name))
         (is-dark (eq (frame-parameter (next-frame) 'background-mode) 'dark))
         (value-getter (if is-dark #'cdr #'car)))
    (when buffer-modeline-color-active-face-remap-token
      (face-remap-remove-relative buffer-modeline-color-active-face-remap-token))
    (setq buffer-modeline-color-active-face-remap-token
          (face-remap-add-relative 'mode-line
                                   (let* ((sat (funcall value-getter buffer-modeline-color-active-saturation))
                                          (val (funcall value-getter buffer-modeline-color-active-value))
                                          (col (apply #'color-rgb-to-hex (color-hsl-to-rgb hue sat val))))
															       `(:background ,col))
															     'mode-line))
    (when buffer-modeline-color-inactive-face-remap-token
      (face-remap-remove-relative buffer-modeline-color-inactive-face-remap-token))
		(setq buffer-modeline-color-inactive-face-remap-token
          (face-remap-add-relative 'mode-line-inactive
                                   (let* ((sat (funcall value-getter buffer-modeline-color-inactive-saturation))
                                          (val (funcall value-getter buffer-modeline-color-inactive-value))
                                          (col (apply #'color-rgb-to-hex (color-hsl-to-rgb hue sat val))))
															       `(:background ,col))
                                   'mode-line-inactive))
		))

(define-minor-mode buffer-modeline-color-mode
  "Toggle buffer-modeline-color-mode."
  :local t
  :init-value nil
  (if buffer-modeline-color-mode
      (progn
        (add-hook 'after-revert-hook #'buffer-modeline-color)
        (add-hook 'window-configuration-change-hook #'buffer-modeline-color)
        (add-hook 'buffer-list-update-hook #'buffer-modeline-color))
    (remove-hook 'after-revert-hook #'buffer-modeline-color)
    (remove-hook 'window-configuration-change-hook #'buffer-modeline-color)
    (remove-hook 'buffer-list-update-hook #'buffer-modeline-color)))

(provide 'buffer-modeline-color)
