;;; serve-rails.el - rails server from inside emacs

;; Copyright (C) 2012 Matt Briggs

;; Author: Matt Briggs <matt@mattbriggs.net>
;; Keywords: navigation rails
;; Version: 1

;;; Dependancies: popwin.el, eproject

;;; Commentary:

;; Start a rails server in a comint buffer

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

      (popwin:popup-buffer out :noselect t))
    (cd current-dir)))

(defun serve-rails:start-jasmine (&optional server)
  (interactive)
  (let ((current-dir default-directory))
    (cd (eproject-root))
    (rvm-autodetect-ruby)
    (let* ((out (apply 'make-comint "jasmine-server" "bash" nil '( "-c" (concat "(cd " (eproject-root) " && bundle exec rake jasmine)")))))

      (with-current-buffer out
        (make-local-variable 'comint-buffer-maximum-size)
        (setq comint-buffer-maximum-size 3000)
        (add-hook 'comint-output-filter-functions 'comint-truncate-buffer t t))

      (popwin:popup-buffer out :noselect t))
    (cd current-dir)))

(provide 'serve-rails)
