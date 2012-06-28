;;; serve-rails.el - rails server from inside emacs

;; Copyright (C) 2012 Matt Briggs

;; Author: Matt Briggs <matt@mattbriggs.net>
;; Keywords: navigation rails
;; Version: 1

;;; Dependancies: popwin.el, eproject, github.com/mbriggs/buffer-tail.el

;;; Commentary:

;; Start a rails server in a comint buffer

(defvar serve-rails/starting-spork-hook nil)
(defvar serve-rails/starting-guard-hook nil)
(defvar serve-rails/launch-complete-hook nil)

(defvar serve-rails/default-server 'thin)

(defvar serve-rails/servers
  '((thin      . ("rails" "server" "thin"))
    (passenger . ("passenger" "start"))
    (puma      . ("rails" "server" "puma"))
    (unicorn   . ("unicorn start"))))

(defun serve-rails:start-project-server ()
  (interactive)
  (let ((proj-server (eproject-attribute :rails-server)))
    (serve-rails:start-server proj-server)))

(defun serve-rails:start-server (&optional server)
  (interactive)
  (let ((current-dir default-directory))
    (cd (eproject-root))
    (rvm-autodetect-ruby)
    (let* ((server-name (or server serve-rails/default-server))
           (config (cdr (assoc server-name serve-rails/servers)))
           (command (car config))
           (args (cdr config))
           (out (apply 'make-comint "rails-server" command nil args)))

      (with-current-buffer out
        (make-local-variable 'comint-buffer-maximum-size)
        (setq comint-buffer-maximum-size 3000)
        (add-hook 'comint-output-filter-functions 'comint-truncate-buffer t t))

      (popwin:popup-buffer-tail out :noselect t)
      (toggle-buffer-tail "*rails-server*" "on"))
    (cd current-dir))
  (run-hooks 'serve-rails/launch-complete-hook))

(defun serve-rails:start-jasmine (&optional server)
  (interactive)
  (let ((current-dir default-directory))
    (cd (eproject-root))
    (rvm-autodetect-ruby)
    (let* ((out (apply 'make-comint "jasmine-server" "rake" nil '("jasmine"))))

      (with-current-buffer out
        (make-local-variable 'comint-buffer-maximum-size)
        (setq comint-buffer-maximum-size 3000)
        (add-hook 'comint-output-filter-functions 'comint-truncate-buffer t t))

      (popwin:popup-buffer-tail out :noselect t)
      (toggle-buffer-tail "*jasmine-server*" "on"))
    (cd current-dir))
  (run-hooks 'serve-rails/launch-complete-hook))

(defun serve-rails:start-spork (&optional server)
  (interactive)
  (let ((current-dir default-directory))
    (cd (eproject-root))
    (rvm-autodetect-ruby)
    (run-hooks 'serve-rails/starting-spork-hook)
    (let* ((out (apply 'make-comint "spork-server" "bundle" nil '("exec" "spork" "TestUnit"))))

      (with-current-buffer out
        (make-local-variable 'comint-buffer-maximum-size)
        (setq comint-buffer-maximum-size 3000)
        (add-hook 'comint-output-filter-functions 'comint-truncate-buffer t t))

      (popwin:popup-buffer-tail out
                                :position 'right
                                :width 70
                                :noselect t
                                :stick t)
      (toggle-buffer-tail "*spork-server*" "on"))
    (cd current-dir))
  (run-hooks 'serve-rails/launch-complete-hook))

(defun serve-rails:start-guard (&optional server)
  (interactive)
  (let ((current-dir default-directory))
    (cd (eproject-root))
    (rvm-autodetect-ruby)
    (run-hooks 'serve-rails/starting-guard-hook)
    (let* ((out (apply 'make-comint "guard" "bundle" nil '("exec" "guard"))))

      (with-current-buffer out
        (make-local-variable 'comint-buffer-maximum-size)
        (setq comint-buffer-maximum-size 3000)
        (add-hook 'comint-output-filter-functions 'comint-truncate-buffer t t))

      (popwin:popup-buffer-tail out
                                :position 'right
                                :width 70
                                :noselect t
                                :stick t)
      (toggle-buffer-tail "*guard*" "on"))
    (cd current-dir))
  (run-hooks 'serve-rails/launch-complete-hook))

(provide 'serve-rails)
