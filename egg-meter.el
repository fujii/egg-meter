;;; egg-meter.el --- Benchmark of Japanese kana-kanji conversion engine using Egg

;; Copyright (C) 2009 Fujii Hironori

;; Author: Fujii Hironori <fujii.hironori@gmail.com>
;; Keywords: languages

(require 'egg)

(defvar egg-meter-backend-alist nil)

(defvar egg-meter-buffer-name "*Egg Meter*")

(defun egg-meter-register-backend (name file symbol)
  (load file t)
  (when (plist-get (symbol-plist symbol) 'egg-start-conversion)
    (add-to-list 'egg-meter-backend-alist (cons name symbol))))

(egg-meter-register-backend "Anthy" "egg/anthy" 'anthy-egg-conversion-backend)
(egg-meter-register-backend "Canna" "egg/canna" 'canna-backend-Japanese)
(egg-meter-register-backend "Sj3" "egg/sj3" 'sj3-conversion-backend)
(egg-meter-register-backend "FreeWnn" "egg/wnn" 'wnn-backend-Japanese)

(defun egg-meter-convert (backend hiragana)
  (let* ((blist (egg-start-conversion backend hiragana nil))
	 (clist (mapcar 'egg-get-bunsetsu-converted blist)))
    (egg-end-conversion blist t)
    clist))

(defun egg-meter-convert-string (backend hiragana)
  (mapconcat 'identity (egg-meter-convert backend hiragana) " "))

(defun egg-meter-test (sentences)
  (set-buffer (get-buffer-create egg-meter-buffer-name))
  (display-buffer egg-meter-buffer-name)
  (erase-buffer)
  (mapcar (lambda (sentence)
	    (mapcar
	     (lambda (pair)
	       (insert (car pair) "\t")
	       (insert (egg-meter-convert-string (cdr pair) sentence) "\n"))
	     egg-meter-backend-alist))
	  sentences))

(defun egg-meter-test-buffer ()
  (interactive)
  (egg-meter-test (split-string (buffer-substring (point-min) (point-max)) "\n" t)))

(provide 'egg-meter)
