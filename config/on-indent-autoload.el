;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-indent-autoload.el
;;;;


(feature-allowed-p aggressive-indent
  ;; enable automatically adjust the identation of code
	;; https://github.com/Malabarba/aggressive-indent-mode

	(with-eval-after-load 'aggressive-indent

		;; disable `electric-indent-mode'
		(setq% aggressive-indent-dont-electric-modes t aggressive-indent)

		) ;; end of `with-eval-after-load'
	
	) ;; end of `feature-allowed-p'


(when (self-spec->*env-spec :edit :allowed)
	(with-eval-after-load 'sh-script
		(setq% sh-basic-offset (self-spec->*env-spec :edit :tab-width) sh-script)))

