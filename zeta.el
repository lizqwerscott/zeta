;;; zeta.el --- Use zed zeta model for edit predict in Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>

;; Homepage: https://github.com/lizqwerscott/zeta.el.git
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (llm "0.20.0"))
;; Keywords: AI LLM Emacs Coding

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;; Require
(require 'cl-lib)
(require 'llm)
(require 'llm-ollama)

(require 'zeta-dyn)

;;; Var
(defconst cursor-marker "<|user_cursor_is_here|>"
  "Marker indicating the user cursor position.")

(defconst start-of-file-marker "<|start_of_file|>"
  "Marker indicating the start of file.")

(defconst editable-region-start-marker "<|editable_region_start|>"
  "Marker indicating the start of editable region.")

(defconst editable-region-end-marker "<|editable_region_end|>"
  "Marker indicating the end of editable region.")

(defgroup zeta nil
  "Zeta model."
  :group 'zeta)

(defcustom zeta-llm-provide (make-llm-ollama :host "localhost"
                                             :port 11434
                                             :chat-model "zeta-q8-0"
                                             :default-chat-non-standard-params '(("num_ctx" . 512)))
  "Zeta llm provide"
  :group 'zeta)

(defcustom zeta-diff-idle 0.6
  "Zeta diff stay idle."
  :group 'zeta
  :type 'number)

(defcustom zeta-up-content-line 10
  "Zeta get content line number"
  :group 'zeta
  :type 'number)

(defcustom zeta-down-content-line 10
  "Zeta get content line number"
  :group 'zeta
  :type 'number)

(defface zeta-diff-show
  '((t :inherit shadow))
  "Face for completion candidates in the zeta diff overlay."
  :version "30.1"
  :group 'zeta)

(defface zeta-diff-delete-text
  '((t :background "red"))
  "Face for completion candidates in the zeta diff overlay."
  :version "30.1"
  :group 'zeta)

(defface zeta-diff-add-text
  '((t :background "green"))
  "Face for completion candidates in the zeta diff overlay."
  :version "30.1"
  :group 'zeta)

(define-minor-mode zeta-mode
  "Zeta mode"
  :init-value nil
  :group 'zeta
  (if zeta-mode
      (progn
        (add-hook 'before-change-functions #'zeta-monitor-before-change nil t)
        (add-hook 'after-change-functions #'zeta-monitor-after-change nil t)
        (setf zeta-edit-buffer (buffer-name)))
    (remove-hook 'before-change-functions #'zeta-monitor-before-change t)
    (remove-hook 'after-change-functions #'zeta-monitor-after-change t)
    (setf zeta-edit-buffer nil)))

(defvar-local zeta-status 'monitor
  "Storage zeta buffer handle status.
'monitor : monitor user input
'ai : ai model handle
'handle : model return handle
'show : show model overlay")

(defvar-local zeta-edit-buffer nil
  "Storage for original buffer.")

;; monitor user input
(defvar-local zeta-diff-recorder--original-content nil
  "Storage for original buffer content.")

(defvar-local zeta-monitor-timer nil
  "Monitor change timer")

(defvar-local zeta-monitor-status nil
  "Monitor user input status.
'idle
'change")

;; ai
(defvar-local zeta-llm-request nil
  "LLM chat request")

;; ai handle
(defvar-local zeta-diff-content nil
  "Storage for diff content.")

;; show diff
(defvar-local zeta--last-point nil
  "Last known cursor position for suggestion overlay.")

(defvar-local zeta-diff-contents nil
  "Storage for all handle diff content.")

(defvar-local zeta-diff-add-overlay nil
  "Storage add new diff content overlay which show.")

(defvar-local zeta-diff-modify-overlay nil
  "Storage diff content overlay which show.")

(defvar-local zeta-diff-modify-check-overlay nil
  "Storage diff content overlay which show.")

;;; Monitor
(defun zeta-monitor-before-change (begin end)
  (pcase zeta-status
    ('monitor
     (zeta-diff-recorder-start)
     (when (equal zeta-monitor-status 'idle)
       (cancel-timer zeta-monitor-timer)
       (setf zeta-monitor-timer nil))
     (setf zeta-monitor-status 'change))
    ('ai
     ;; 取消 ai 模型请求
     (llm-cancel-request zeta-llm-request)
     (setf zeta-status 'monitor))
    ('handle
     (setf zeta-status 'monitor))
    ('show
     ;; 清除展示的内容，停止展示内容
     (when zeta-active-add-mode
       (zeta-diff-clear-add-diff))
     (when zeta-active-modify-mode
       (when zeta-diff-modify-overlay
         (delete-overlay zeta-diff-modify-overlay)
         (setf zeta-diff-modify-overlay nil))
       (zeta-diff-ignore-modify-diff))
     (setf zeta-status 'monitor))))

(defun zeta-monitor-after-change (begin end length)
  (when (and (equal zeta-status 'monitor)
           (not zeta-monitor-timer))
    (setf zeta-monitor-timer
          (run-with-idle-timer zeta-diff-idle
                               nil
                               #'(lambda ()
                                   (setf zeta-monitor-timer nil)
                                   (zeta-diff-recorder-stop))))))


;;; Diff
;;;###autoload
(defun zeta-diff-recorder-start ()
  "Start recording buffer changes for diff generation."
  (interactive)
  (setq zeta-diff-recorder--original-content
        (buffer-string)))

;;;###autoload
(defun zeta-diff-recorder-stop ()
  "Stop recording and show diff between original and current content."
  (interactive)
  (unless zeta-diff-recorder--original-content
    (user-error "Recording not started. Call `zeta-diff-recorder-start' first"))

  (let ((old-content zeta-diff-recorder--original-content)
        (new-content (buffer-string))
        (old-file (make-temp-file "diff-old-"))
        (new-file (make-temp-file "diff-new-"))
        (name "*zeta-diff*")
        (process nil))
    (with-temp-file old-file
      (insert old-content))
    (with-temp-file new-file
      (insert new-content))
    (setq process
          (apply #'start-process
                 `("zeta-diff"
                   ,name
                   "diff"
                   ,old-file
                   ,new-file
                   "-u")))
    (set-process-sentinel process
                          #'(lambda (proc event)
                              ;; (message "%s" event)
                              (let* ((content-data (zeta-get-content-data)))
                                (with-current-buffer name
                                  (beginning-of-buffer)
                                  (kill-line 2)
                                  (let ((diff-data (buffer-string)))
                                    ;; (message "diff content: %s" diff-data)
                                    ;; (message "content data: %s" content-data)
                                    (setf zeta-status 'ai)
                                    (zeta-get-model-respone diff-data content-data)
                                    (erase-buffer))))))))

(defun zeta-get-content-data ()
  "Get content around cursor position using zeta-content-line."
  (interactive)
  (let* ((start-line (line-number-at-pos))
         (start-pos (point))
         (up-content nil)
         (down-content nil)
         (content-start nil)
         (content-end nil))
    (save-excursion
      (forward-line (- zeta-up-content-line))
      (move-beginning-of-line nil)
      (setf content-start (point))
      (setf up-content
            (buffer-substring-no-properties (point)
                                            start-pos))
      (forward-line (+ zeta-up-content-line zeta-down-content-line))
      (move-beginning-of-line nil)
      (setf content-end (point))
      (setf down-content
            (buffer-substring-no-properties start-pos
                                            (point))))
    (list
     :cursor-pos (point)
     :start content-start
     :end content-end
     :buffer (buffer-name)
     :content (concat editable-region-start-marker
                      up-content
                      cursor-marker
                      down-content
                      editable-region-end-marker))))

(defun zeta-diff-res (cursor-pos start-pos end-pos name res-data)
  (when-let* ((res-edit-data (with-temp-buffer
                               (insert res-data)
                               (beginning-of-buffer)
                               (let ((start (search-forward editable-region-start-marker))
                                     (end (search-forward editable-region-end-marker)))
                                 (buffer-substring-no-properties start
                                                                 (- end
                                                                    (length editable-region-end-marker)))))))
    (with-current-buffer name
      (save-excursion
        (let* ((source-data (buffer-substring start-pos
                                              end-pos))
               (res (zeta-diff-words source-data res-edit-data start-pos)))
          (setf zeta-diff-content nil)
          (dotimes (i (zeta-diff-data-len res))
            (let ((start (zeta-diff-data-start res i))
                  (end (zeta-diff-data-end res i))
                  (data (zeta-diff-data-data res i)))
              (push (list :start start :end end :data data)
                    zeta-diff-content)))
          (setf zeta-diff-content (reverse zeta-diff-content))
          (setq zeta-status 'show)
          (zeta-diff-handle-diff))))))

;;; Show overlay
(defun zeta-diff-show-add-overlay (start end data cursor-pos)
  (unless zeta-diff-add-overlay
    (let ((ov (make-overlay start end)))
      (add-hook 'post-command-hook #'zeta--on-cursor-moved nil t)
      (setf zeta-diff-add-overlay ov
            zeta--last-point cursor-pos)
      (overlay-put ov 'diff-text data)
      (overlay-put ov 'diff-start start)
      (overlay-put ov 'diff-end end)
      (overlay-put ov 'after-string
                   (propertize data 'face 'zeta-diff-show))
      (zeta-active-add-mode 1))))

(defun zeta-diff-show-modify-overlay (start end data cursor-pos)
  (unless zeta-diff-modify-overlay
    (let ((ov (make-overlay start end)))
      (setf zeta-diff-modify-overlay ov)
      (overlay-put ov 'diff-text data)
      (overlay-put ov 'diff-start start)
      (overlay-put ov 'diff-end end)
      (overlay-put ov 'before-string (propertize "<TAB>" 'face 'zeta-diff-add-text))
      (zeta-active-modify-mode 1))))

(defun zeta-diff-show-overlay ()
  (with-current-buffer zeta-edit-buffer
    (when-let* ((content (car zeta-diff-contents))
                (type (plist-get content :type))
                (start (plist-get content :start))
                (end (plist-get content :end))
                (data (plist-get content :data))
                (now-pos (point)))
      (pcase type
        ('in
         (zeta-diff-show-add-overlay start end data now-pos))
        ('before
         (zeta-diff-show-modify-overlay start end data now-pos))
        ('after
         (zeta-diff-show-modify-overlay start end data now-pos))))))

(defun zeta-diff-handle-diff ()
  (when zeta-diff-content
    (with-current-buffer zeta-edit-buffer
      (let ((now-pos (point))
            (diff-before-cursor)
            (diff-in-cursor)
            (diff-after-cursor))
        (dolist (content zeta-diff-content)
          (let* ((start (plist-get content :start))
                 (end (plist-get content :end))
                 (data (plist-get content :data))
                 (source-data (buffer-substring start end)))
            ;; (message "%d..%d: %s" start end data)
            (unless (string= (string-trim data) "")
              ;; (message "start handle")
              (cond
               ((and (>= now-pos start)
                   (<= now-pos end))
                ;; handle add diff
                (if (= start end)
                    (push (list :type 'in :start start :end end :data data)
                          diff-in-cursor)
                  (let* ((point-prev-source (buffer-substring start now-pos))
                         (point-next-source (buffer-substring now-pos end))
                         (point-prev-source-in-data-pos (string-match point-prev-source
                                                                      data)))
                    (when (string= (string-trim point-next-source)
                                   "")
                      (if (string= (string-trim point-prev-source)
                                   "")
                          (push (list :type 'in :start start :end end :data data)
                                diff-in-cursor)
                        (when (= point-prev-source-in-data-pos 0)
                          (push (list :type 'in :start now-pos :end end :data (substring data
                                                                                         (length point-prev-source)
                                                                                         (length data)))
                                diff-in-cursor)))))))
               ;; handle not in cursor replace, before cursor
               ((< end now-pos)
                (push (list :type 'before :start start :end end :data data)
                      diff-before-cursor))
               ;; handle not in cursor replace, after cursor
               ((> start now-pos)
                (push (list :type 'after :start start :end end :data data)
                      diff-after-cursor))))))
        (setf zeta-diff-contents
              (append diff-in-cursor
                      diff-after-cursor
                      diff-before-cursor))
        (zeta-diff-show-overlay)))))

;; handle add diff
(defun zeta-diff-accpet-add-diff ()
  (interactive)
  (let* ((start (overlay-get zeta-diff-add-overlay 'diff-start))
         (data (overlay-get zeta-diff-add-overlay 'diff-text)))
    (zeta-diff-clear-add-diff)
    ;; (remove-hook 'before-change-functions #'zeta-monitor-before-change t)
    ;; (remove-hook 'after-change-functions #'zeta-monitor-after-change t)
    (goto-char start)
    (insert data)
    ;; (add-hook 'before-change-functions #'zeta-monitor-before-change nil t)
    ;; (add-hook 'after-change-functions #'zeta-monitor-after-change nil t)
    ))

(defun zeta-diff-clear-add-diff ()
  (interactive)
  (when zeta-diff-add-overlay
    (delete-overlay zeta-diff-add-overlay)
    (setf zeta-diff-add-overlay nil)
    (zeta-active-add-mode -1))
  (remove-hook 'post-command-hook #'zeta--on-cursor-moved t)
  (setq zeta--last-point nil)
  ;; (zeta-diff-show-overlay)
  )

(defun zeta--cursor-moved-p ()
  "Check if cursor moved from last suggestion position."
  (and zeta--last-point
     (not (eq zeta--last-point (point)))))

(defun zeta--on-cursor-moved ()
  "Zeta event on cursor moved."
  (when (zeta--cursor-moved-p)
    (zeta-diff-clear-add-diff)))

(defvar zeta-active-add-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "TAB" #'zeta-diff-accpet-add-diff)
    map)
  "Keymap used when `zeta-active-add-mode' is enabled.")

(define-minor-mode zeta-active-add-mode
  "Activated when there is an active suggestion in zeta."
  :init-value nil
  :keymap zeta-active-add-mode-map)

;; handle modify diff
(defun zeta-diff-check-modify-diff ()
  (interactive)
  (when-let* ((ov zeta-diff-modify-overlay)
              (data (overlay-get ov 'diff-text))
              (start (overlay-get ov 'diff-start))
              (end (overlay-get ov 'diff-end)))
    (goto-char start)
    (delete-overlay ov)
    (setf zeta-diff-modify-overlay nil)
    (let ((check-ov (make-overlay start end)))
      (setf zeta-diff-modify-check-overlay check-ov)
      (overlay-put check-ov 'diff-text data)
      (overlay-put check-ov 'diff-start start)
      (overlay-put check-ov 'diff-end end)
      (overlay-put check-ov 'face 'zeta-diff-delete-text)
      (overlay-put check-ov 'after-string (propertize data 'face 'zeta-diff-add-text)))))

(defun zeta-diff-accpet-modify-diff ()
  (interactive)
  (when-let* ((ov zeta-diff-modify-check-overlay)
              (data (overlay-get ov 'diff-text))
              (start (overlay-get ov 'diff-start))
              (end (overlay-get ov 'diff-end)))
    (delete-overlay ov)
    (setf zeta-diff-check-modify-diff nil)
    (delete-region start end)
    (insert data)
    (zeta-active-modify-mode -1)
    ;; (zeta-diff-show-overlay)
    ))

(defun zeta-diff-ignore-modify-diff ()
  (interactive)
  (when-let* ((ov zeta-diff-modify-check-overlay)
              (data (overlay-get ov 'diff-text))
              (start (overlay-get ov 'diff-start))
              (end (overlay-get ov 'diff-end)))
    (delete-overlay ov)
    (setf zeta-diff-check-modify-diff nil)
    (zeta-active-modify-mode -1)
    ;; (zeta-diff-show-overlay)
    ))

(defun zeta-diff-action-diff ()
  (interactive)
  (if zeta-diff-modify-overlay
      (zeta-diff-check-modify-diff)
    (zeta-diff-accpet-modify-diff)))

(defvar zeta-active-modify-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "TAB" #'zeta-diff-action-diff)
    (keymap-set map "C-g" #'zeta-diff-ignore-modify-diff)
    map)
  "Keymap used when `zeta-active-modify-mode' is enabled.")

(define-minor-mode zeta-active-modify-mode
  "Activated when there is an active suggestion in zeta."
  :init-value nil
  :keymap zeta-active-modify-mode-map)

;;; AI Model
(defun zeta-generate-prompt (diff-data content-data)
  (format "### UserEdits:

User edited \"%s\": ```%s```

### User Excerpt: ```%s```"
          (buffer-file-name)
          diff-data
          content-data))

(cl-defun zeta-get-model-respone (diff-data content-data &optional (tempature 0))
  ;; (message "zeta start get model")
  (setq zeta-llm-request
        (llm-chat-async zeta-llm-provide
                        (llm-make-chat-prompt (zeta-generate-prompt diff-data
                                                                    (plist-get content-data :content))
                                              :context "You are a code completion assistant and your task is to analyze user edits and then rewrite an excerpt that the user provides, suggesting the appropriate edits within the excerpt, taking into account the cursor location."
                                              :temperature tempature)
                        #'(lambda (res)
                            ;; (message "Zeta(Resource): %s" res)
                            (when (equal zeta-status 'ai)
                              (setq zeta-status 'handle)
                              (unless (string= (string-trim res)
                                               "")
                                (zeta-diff-res (plist-get content-data :cursor-pos)
                                               (plist-get content-data :start)
                                               (plist-get content-data :end)
                                               (plist-get content-data :buffer)
                                               res))))
                        #'(lambda (type err)
                            (message "Error: %s %s" type err)))))


;;; Test

(defun zeta-test-diff ()
  (interactive)
  (let* ((source-data "
    let pivot_index = partition(arrl);
    let

    res
}
"         )
         (diff-data "
    let pivot_index = partition(arrl);
    let left = quick_sort(&arr[..pivot_index]);
    let right = quick_sort(&arr[pivot_index + 1..]);

    res
}

"         )
         (res (zeta-diff-words source-data diff-data 0)))
    (dotimes (i (zeta-diff-data-len res))
      (message "%d..%d: %s"
               (zeta-diff-data-start res i)
               (zeta-diff-data-end res i)
               (zeta-diff-data-data res i)))))

(provide 'zeta)
;;; zeta.el ends here
