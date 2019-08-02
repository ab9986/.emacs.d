(setq emacs-config-el-path (expand-file-name "lisp/emacs-config.el" user-emacs-directory)) 
(setq emacs-config-org-path (expand-file-name "lisp/emacs-config.org"
                                              user-emacs-directory))

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path (expand-file-name "lisp" user-emacs-directory))

;;(add-to-list 'load-path
;;             (expand-file-name "lisp" user-emacs-directory))
             

(if (file-exists-p emacs-config-el-path)
    (require 'emacs-config)
  (progn
    ;;设置所有的.org 打开默认为 utf-8
    (modify-coding-system-alist 'file "\\.org\\'"
                                'utf-8)
    (require 'org-install)
    (require 'ob-tangle)
    ;;(org-babel-load-file "~/.emacs.d/emacs-config.org")
    (org-babel-load-file emacs-config-org-path)
    (byte-compile-file emacs-config-el-path)))
(provide 'init)
