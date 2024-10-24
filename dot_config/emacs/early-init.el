(setq package-enable-at-startup nil)

(setq inhibit-startup-echo-area-message (user-login-name)
      inhibit-startup-screen t)
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq initial-scratch-message nil)

(mapc (lambda (mode) (funcall mode -1))
  '(menu-bar-mode scroll-bar-mode tool-bar-mode))
(setq-default mode-line-format nil)
