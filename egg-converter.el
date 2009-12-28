;;; egg-converter.el --- Japanese kana-kanji converter using Egg

;; Copyright (C) 2009 Fujii Hironori

;; Author: Fujii Hironori <fujii.hironori@gmail.com>
;; Keywords: languages

;; emacs -batch -l egg-converter.el -f egg-converter-batch anthy input.txt output.txt

(require 'egg)

(defvar egg-converter-backend-alist nil)

(defun egg-converter-register-backend (name file symbol)
  (load file t)
  (when (plist-get (symbol-plist symbol) 'egg-start-conversion)
    (add-to-list 'egg-converter-backend-alist (cons name symbol))))

(egg-converter-register-backend "anthy" "egg/anthy" 'anthy-egg-conversion-backend)
(egg-converter-register-backend "canna" "egg/canna" 'canna-backend-Japanese)
(egg-converter-register-backend "sj3" "egg/sj3" 'sj3-conversion-backend)
(egg-converter-register-backend "wnn" "egg/wnn" 'wnn-backend-Japanese)
;(egg-converter-register-backend "Mana" "egg/anthy" 'egg-mana-conversion-backend)

(defun egg-converter-convert-string (backend hiragana)
  (let* ((blist (egg-start-conversion backend hiragana nil))
	 (clist (mapcar 'egg-get-bunsetsu-converted blist)))
    (egg-end-conversion blist t)
    (apply 'concat clist)))

(defun egg-converter-convert-buffer (backend input output)
  (with-current-buffer input
    (mapcar (lambda (sentence)
	      (message "%s" sentence)
	      (with-current-buffer output
		(insert (egg-converter-convert-string backend sentence) "\n")))
	    (split-string (buffer-substring (point-min) (point-max)) "\n" t))))

(defun egg-converter-convert-file (backend-name input-file output-file)
  (let ((backend (or (assoc backend-name egg-converter-backend-alist) (error "unknown backend: %s" backend-name)))
	(input (find-file-existing input-file)))
    (with-temp-buffer
      (egg-converter-convert-buffer (cdr backend) input (current-buffer))
      (write-file output-file))))

(defun egg-converter-batch ()
  (unless (= 3 (length command-line-args-left))
    (error "invalid arugment"))
  (apply 'egg-converter-convert-file command-line-args-left)
  (setq command-line-args-left nil))

(provide 'egg-converter)
