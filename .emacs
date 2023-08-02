;; disable tool bar!
(tool-bar-mode -1)
;; disable menu bar!
(menu-bar-mode -1)
;; disable scroll bar!
(scroll-bar-mode -1)


;(provide 'init-ui)

;(tool-bar-mode)
;(menu-bar-mode)

;; never show startup screen
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

;; open full-screen in gui by default
(if (display-graphic-p) (toggle-frame-fullscreen))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;(straight-use-package 'el-patch)
(straight-use-package 'use-package)

(use-package ujelly-theme
  :straight t
  :config
  (load-theme 'ujelly t))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

(add-hook 'prog-mode-hook 'copilot-mode)

(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "<tab>") #'my/copilot-tab)
  (define-key copilot-mode-map (kbd "TAB") #'my/copilot-tab)
  (define-key copilot-mode-map (kbd "C-<tab>") #'copilot-complete)
  (define-key copilot-mode-map (kbd "C-c y") #'my/copilot-tab))

;;(setenv "OPENAI_API_KEY" (string-trim (shell-command-to-string "pass show openai.com/api-key")))
;; install chatgpt.el from https://github.com/barryridge/ChatGPT.el/tree/joshcho-set-backend-model
;; looks cool but can't make it work :(

;; Codex-Completion Package
;;(use-package codex-completion
;;  :straight (codex-completion :type git :host github :repo "debanjum/codex-completion")
;;  :bind ("C-c x" . 'codex-completion)
;;  :bind ("C-c i" . 'codex-completion)
;;  :config (setq codex-completion-openai-api-token (string-trim (shell-command-to-string "pass show openai.com/api-key"))))

;;(use-package gptel
;;  :straight t
;;  :config
;;  (setq gptel-api-key (string-trim (shell-command-to-string "pass show openai.com/api-key")))
;;  (setq gptel-model "gpt-3.5-turbo-16k"))

;; install gptel from github using straight https://github.com/karthink/gptel
(use-package gptel
  :straight (gptel :type git :host github :repo "karthink/gptel" :files ("*.el"))
  :bind ("C-c x" . 'gptel-send)
  :config
  (setq gptel-api-key (string-trim (shell-command-to-string "pass show openai.com/api-key")))
  (setq-default gptel-model "gpt-4"))
;;(setq gptel-model "gpt-3.5-turbo-16k"))

;; install company using straight from melpa
(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (global-company-mode t))

;; install rust-mode using straight from melpa
(use-package rust-mode
  :straight t
  :config
  (setq rust-format-on-save t))

;; install LSP for rust using straight from melpa
;(use-package lsp-mode
;  :straight t
;  :hook (rust-mode . lsp)
;  :commands lsp)

;; install LSP for rust using straight from melpa and configure it
(use-package lsp-mode
  :straight t
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  ;;(lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; install rustic
(use-package rustic
  :straight t
  :config
  (setq rustic-lsp-server 'rust-analyzer))

(use-package lsp-ui
  :straight t
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; install flycheck using straight from melpa
(use-package flycheck
  :straight t
  :config
  ;; use flycheck in cpp-mode
  (add-hook 'c++-mode-hook 'flycheck-mode)
  ;; use flycheck in rust-mode
  (add-hook 'rust-mode-hook 'flycheck-mode))

;; install cpputils-cmake using straight from melpa
;(use-package cpputils-cmake
;  :straight t)

;; install cmake-mode from melpa
;(use-package cmake-mode
;  :straight t)

;; install code folding with origami
;(use-package origami
;  :straight t
;  :config
;  (global-origami-mode)
;  (add-hook 'prog-mode-hook 'origami-mode))

;; install company-box, which plays nice with copilot
;(use-package company-box
;  :straight t
;  :hook (company-mode . company-box-mode))

;(use-package humanoid-themes
;  :straight t
;  :config
;  (load-theme 'humanoid-dark t))

;(use-package dracula-theme
;  :straight t
;  :config
;  (load-theme 'dracula t))

;; include asciidoc mode
(use-package adoc-mode
  :straight t
  :mode "\\.adoc\\'"
  ;; use regular sized text
  :hook (adoc-mode . (lambda () (text-scale-set 0))))

;; flyspell for text mode only
(use-package flyspell
  :straight t
  :hook (text-mode . flyspell-mode))
  ;:hook (prog-mode . flyspell-prog-mode))

;; setup ess with straight.el
;;(use-package ess
;;  :straight t
;;  :mode ("\\.R\\'" . R-mode)
;;  :config
;;  (setq ess-ask-for-ess-directory nil)
;;  (setq ess-eval-visibly-p nil)
;;  (setq ess-use-flymake nil)
;;  (setq ess-use-company nil)
;;  (setq ess-use-tracebug nil)
;;  (setq ess-use-auto-complete nil)
;;  (setq ess-use-eldoc nil)
;;  (setq ess-use-ido nil)
;;  (setq ess-use-underscore nil)
;;  (setq ess-use-inferior-program-in-buffer-name nil)
;;  (setq ess-use-inferior-program-in-buffer-name nil))
;;
;; setup tidal cycles with straight.el from github
(use-package tidal :straight t)

;; set up whisper.el with straight.el
(use-package whisper
  :straight (:host github :repo "natrys/whisper.el" :files ("*.el"))
  :ensure t
  ;; models could be tiny, tiny.en, base, base.en, small, small.en, medium, medium.en, large
  :config (setq whisper-model "small.en"
  ;;            whisper-language "en"
                ;;            whisper-enable-speed-up t
                whisper-use-threads 16)
  :bind ([f8] . 'whisper-run)
  :bind ("C-c w" . 'whisper-run))

;; turn off tool bar
(tool-bar-mode 0)
;;(menu-bar-mode -1)
(menu-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(cond
 ((member "Noto Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Noto Mono-11"))
 ((member "Monaco" (font-family-list))
  (set-face-attribute 'default nil :font "Monaco-11"))
 ((member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata-11"))
 ((member "Consolas" (font-family-list))
  (set-face-attribute 'default nil :font "Consolas-11"))
 ((member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-11")))

(setq c-default-style "linux"
      c-basic-offset 4)

(global-set-key [f9] 'compile)

(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (setq tab-width 4)))

;; install solarized and use light mode
;;(use-package solarized-theme
;;  :straight t
;;  :config
;;  (load-theme 'solarized-light t))

(savehist-mode 1)

;(require 'rust-mode)
;(load-file (expand-file-name "init.el" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



