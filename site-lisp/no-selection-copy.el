;;; no-selection-copy --- Copy without selection

;;; Commentary:

;; This package provides copy without selection functionality and is
;; based on:
;;    http://www.emacswiki.org/emacs/CopyWithoutSelection

;;; Code:

(defun get-point (symbol &optional arg)
  "Get the `point' after calling SYMBOL with ARG."
  (funcall symbol arg)
  (point))


(defun copy-thing (move-forward move-backwards &optional arg)
  "Copy thing between one MOVE-FORWARD and MOVE-BACKWARDS ARG times into `kill-ring'."
  (save-excursion
    (let ((beg (get-point move-forward 1))
          (end (get-point move-backwards arg)))
      (copy-region-as-kill beg end))))

(defun copy-word (&optional arg)
  "Copy ARG words at point into `kill-ring'."
  (interactive "P")
  (copy-thing 'forward-word 'backward-word arg))


(defun copy-line (&optional arg)
  "Save current line (or ARG lines) into `kill-ring' without marking the line."
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg))


(defun copy-paragraph (&optional arg)
  "Copy ARG paragraphs at point."
  (interactive "P")
  (copy-thing 'backward-paragraph 'forward-paragraph arg))

(provide 'no-selection-copy)

;;; no-selection-copy ends here
