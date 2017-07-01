;;; helm-kythe.el --- Google Kythe helm interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Google Inc.

;; Author: Fangrui Song <i@maskray.me>
;; Package-Version: 20170629.1
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (dash "2.13.0") (helm "2.0") (s "1.10.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `helm-kythe.el' is a `helm' interface of Google Kythe.

;; This package is enlightened by `'helm-gtags.el'.

;; Usage
;; For C++:
;; (add-hook 'c++-mode-hook 'helm-kythe-mode)
;;
;; For Haskell:
;; (add-hook 'haskell-mode-hook 'helm-kythe-mode)
;;
;; % haskell-indexer/build-stach.sh /tmp/logs mtl
;; % haskell-indexer/serve.sh /tmp/logs
;; % emacs /tmp/mtl-2.2.1/Control/Monad/Cont/Class.hs
;;
;; Suppose mtl-2.2.1 is the version that is indexed.
;; helm-kythe-find-definitions/references can jump to xrefs that are outside
;; of mtl if the packages have been unpacked at /tmp/ or some
;; directory listed in helm-kythe-file-search-paths.
;;
;; If eldoc-mode is enabled, when the point is at a reference, the `snippet' of its definition will be displayed in minibuffer.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'easymenu)
(require 'helm)
(require 's)

(defcustom helm-kythe-highlight-candidate t
  "Highlight candidate or not"
  :group 'helm-kythe
  :type 'boolean)

;; TODO 0.0.26 /opt/kythe/tools/http_server --listen localhost:8080 does not listen on ::1
(defcustom helm-kythe-http-server-url "http://127.0.0.1:8080"
  "kythe/tools/http_server --listen"
  :group 'helm-kythe
  :type 'string)

(defcustom helm-kythe-prefix-key (kbd "C-c k")
  "If non-nil, it is used for the prefix key of helm-kythe-xxx command."
  :group 'helm-kythe
  :type 'string)

(defcustom helm-kythe-suggested-key-mapping t
  "If non-nil, suggested key mapping is enabled."
  :group 'helm-kythe
  :type 'boolean)

(defcustom helm-kythe-file-search-paths nil
  "A list of search paths for \"kythe://?path=$path\"."
  :group 'helm-kythe
  :type '(list string))

(defface helm-kythe-active
  '()
  "Face for mode line when Kythe is active.")

(defface helm-kythe-file
  '((t :inherit font-lock-keyword-face))
  "Face for filenames in the error list.")

(defface helm-kythe-inactive
  '((t (:strike-through t)))
  "Face for mode line when Kythe is inactive.")

(defface helm-kythe-lineno
  '((t :inherit font-lock-doc-face))
  "Face for line numbers in the error list.")

(defface helm-kythe-match
  '((t :inherit helm-match))
  "Face for word matched against tagname")

(defvar helm-kythe--find-file-action
  (helm-make-actions
   "Open file" #'helm-kythe--action-openfile
   "Open file other window" #'helm-kythe--action-openfile-other-window))

(defvar helm-kythe--use-otherwin nil)

(defvar helm-source-helm-kythe-definitions
  (helm-build-in-buffer-source "Find definitions"
    :init #'helm-kythe--source-definitions
    :real-to-display #'helm-kythe--candidate-transformer
    :action #'helm-kythe--find-file-action))

(defvar helm-source-helm-kythe-definitions-prompt
  (helm-build-in-buffer-source "Find definitions with prompt"
    :init #'helm-kythe--source-definitions-prompt
    :real-to-display #'helm-kythe--candidate-transformer
    :action #'helm-kythe--find-file-action))

(defvar helm-source-helm-kythe-imenu
  (helm-build-in-buffer-source "imenu"
    :init #'helm-kythe--source-imenu
    :real-to-display #'helm-kythe--candidate-transformer
    :action #'helm-kythe--find-file-action))

(defvar helm-source-helm-kythe-references
  (helm-build-in-buffer-source "Find references"
    :init #'helm-kythe--source-references
    :real-to-display #'helm-kythe--candidate-transformer
    :action #'helm-kythe--find-file-action))

(defvar helm-source-helm-kythe-references-prompt
  (helm-build-in-buffer-source "Find references with prompt"
    :init #'helm-kythe--source-references-prompt
    :real-to-display #'helm-kythe--candidate-transformer
    :action #'helm-kythe--find-file-action))

(defvar-local helm-kythe-applied-decorations-p nil
  "Non-nil if decorations have been applied.")

(defconst helm-kythe--buffer "*helm helm-kythe*")
;; (defconst helm-kythe--anchor-regex "\\`\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):\\(.*\\)")
(defconst helm-kythe--ticket-anchor-regex "\\`\\([^\0]+\\)\0\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):\\(.*\\)")

(define-error 'helm-kythe-error "helm-kythe error")

(defun helm-kythe--action-openfile (candidate)
  (when (string-match helm-kythe--ticket-anchor-regex candidate)
    (let ((ticket (match-string-no-properties 1 candidate))
          (filename (match-string-no-properties 2 candidate))
          (line (string-to-number (match-string-no-properties 3 candidate)))
          (column (string-to-number (match-string-no-properties 4 candidate))))
      (when (helm-kythe--find-file
             (if helm-kythe--use-otherwin #'find-file-other-window #'find-file) filename)
        (goto-line line)
        (forward-char column)
        ;; If the target has been modified, enumerate all decorations and find the ticket.
        (if (helm-kythe--ticket-equal? ticket (or (helm-kythe--definition-at-point) (helm-kythe--reference-at-point)))
            (recenter)
          (let ((p (point)))
            (unless (catch 'loop
                      (cl-loop for (prop . get-prop) in
                            '((helm-kythe-definition . helm-kythe--definition-at-point)
                              (helm-kythe-reference . helm-kythe--reference-at-point)) do
                              (beginning-of-buffer)
                              (while (not (eobp))
                                (let ((ticket1 (funcall get-prop))
                                      (next-change
                                       (or (next-single-property-change (point) prop)
                                           (point-max))))
                                  (when (helm-kythe--ticket-equal? ticket ticket1)
                                    (recenter)
                                    (throw 'loop t))
                                  (goto-char next-change)))))
              (goto-char p))))))))

(defun helm-kythe--action-openfile-other-window (candidate)
  (let ((helm-kythe--use-otherwin t))
    (helm-kythe--action-openfile candidate)))

(defun helm-kythe--definition-at-point ()
  (-some->> (get-text-property (point) 'helm-kythe-definition) (alist-get 'ticket)))

(defun helm-kythe--reference-at-point ()
  (-some->> (get-text-property (point) 'helm-kythe-reference) (alist-get 'source_ticket)))

(defun helm-kythe--reference-target-at-point ()
  (-some->> (get-text-property (point) 'helm-kythe-reference) (alist-get 'target_ticket)))

(defun helm-kythe--anchor-to-candidate (anchor)
  (format "%s\0%s:%d:%d:%s"
          (alist-get 'ticket anchor)
          (helm-kythe--path-from-ticket (alist-get 'parent anchor))
          (alist-get 'line_number (alist-get 'start anchor))
          (or (alist-get 'column_offset (alist-get 'start anchor)) 0)  ;; TODO Cabal cpu: 'start' of definition_locations of getSystemArch = X86_64 does not have column_offset
          (alist-get 'snippet anchor)))

(defun helm-kythe--candidate-transformer (candidate)
  (if (and helm-kythe-highlight-candidate
           (string-match helm-kythe--ticket-anchor-regex candidate))
      (format "%s:%s:%s:%s"
              (propertize (match-string-no-properties 2 candidate) 'face 'helm-kythe-file)
              (propertize (match-string-no-properties 3 candidate) 'face 'helm-kythe-lineno)
              (propertize (match-string-no-properties 4 candidate) 'face 'helm-kythe-lineno)
              (match-string-no-properties 5 candidate)
              )
      candidate))

(defun helm-kythe--char-offsets (object start-key end-key)
  (cons (byte-to-position (1+ (alist-get 'byte_offset (alist-get start-key object))))
        (byte-to-position (1+ (alist-get 'byte_offset (alist-get end-key object))))))

(defun helm-kythe--common (srcs)
  (let ((helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t))
    (helm :sources srcs :buffer helm-kythe--buffer)))

(defun helm-kythe--find-file (open-func path)
  (cond ((file-name-absolute-p path) (funcall open-func path))
        ((s-ends-with? path (buffer-file-name)) t)
        (t
         (-if-let* ((_ (eq major-mode 'haskell-mode))
                    (project-root (helm-kythe--haskell-find-project-root))
                    (f (concat (file-name-directory project-root) path))
                    (_ (file-exists-p f)))
             (funcall open-func f)
           (cl-loop for search-path in helm-kythe-file-search-paths do
                 (-if-let* ((f (concat search-path path))
                            (_ (file-exists-p f)))
                     (funcall open-func f)))))))

(defun helm-kythe--fontify-haskell (line)
  (with-temp-buffer
    (when (fboundp 'haskell-mode)
      (let ((flycheck-checkers nil)
            (haskell-mode-hook nil))
        (haskell-mode))
      (insert line)
      (font-lock-ensure)
      (buffer-string))))

(defun helm-kythe--haskell-find-project-root ()
  (require 'inf-haskell)  ;; for inferior-haskell-find-project-root
  (let ((p (inferior-haskell-find-project-root (current-buffer))))
    (while (not (string-match-p ".-[0-9]" (file-name-nondirectory p)))
      (setq p (directory-file-name (file-name-directory p))))
    p))

(defun helm-kythe--path-from-ticket (ticket)
  (when-let (i (string-match "path=\\([^#]+\\)" ticket)) (match-string 1 ticket)))

(defun helm-kythe--ticket-equal? (ticket0 ticket1)
  (and ticket1 (equal (s-replace "%3A" "/" ticket0) (s-replace "%3A" "/" ticket1))))

(defun helm-kythe-post (path data)
  (let ((url-request-method "post")
        (url-request-extra-headers '(("content-type" . "application/json")))
        (url-request-data (json-encode data)))
    (with-current-buffer (url-retrieve-synchronously (concat helm-kythe-http-server-url path) t)
      (beginning-of-buffer)
      (re-search-forward "\n\n")
      (buffer-string)
      (condition-case nil
          (save-excursion (json-read))
        ('json-error
         (signal 'helm-kythe-error (concat "Kythe http_server error: " (string-trim (buffer-substring-no-properties (point) (point-max))))))))))

(defun helm-kythe--propertize-mode-line (face)
  (propertize " Kythe" 'face face))

(defun helm-kythe--set-mode-line (face)
  (setq-local helm-kythe-mode-line (helm-kythe--propertize-mode-line face)))

(defun helm-kythe--source-definitions ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer))
  (-if-let* ((ticket (helm-kythe--reference-target-at-point))
               (defs (helm-kythe-get-definitions ticket)))
      (helm-init-candidates-in-buffer 'global (mapcar #'helm-kythe--anchor-to-candidate defs))
    (message "No definitions")))

;; TODO No results. Why?
(defun helm-kythe--source-definitions-prompt ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer))
  (-if-let* ((ticket (helm-comp-read "Ticket: " '()))
               (defs (helm-kythe-get-definitions ticket)))
      (helm-init-candidates-in-buffer 'global (mapcar #'helm-kythe--anchor-to-candidate defs))
    (message "No definitions")))

(defun helm-kythe--source-imenu ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer))
  (let ((xs))
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
        (let ((def (get-text-property (point) 'helm-kythe-definition))
              (next-change
               (or (next-single-property-change (point) 'helm-kythe-definition)
                   (point-max))))
          (when def (push (helm-kythe--anchor-to-candidate def) xs))
          (goto-char next-change))))
    (helm-init-candidates-in-buffer 'global (nreverse xs))))

(defun helm-kythe--source-references ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer))
  (-if-let* ((ticket (helm-kythe--reference-target-at-point))
               (refs (helm-kythe-get-references ticket)))
      (helm-init-candidates-in-buffer 'global (mapcar #'helm-kythe--anchor-to-candidate refs))
    (message "No references")))

;; TODO No results. Why?
(defun helm-kythe--source-references-prompt ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer))
  (-if-let* ((ticket (helm-comp-read "Ticket: " '()))
             (refs (helm-kythe-get-references ticket)))
      (helm-init-candidates-in-buffer 'global (mapcar #'helm-kythe--anchor-to-candidate refs))
    (message "No references")))

(defun helm-kythe-post-decorations (ticket)
  (helm-kythe-post "/decorations"
              `((location . ((ticket . ,ticket)))
                (references . t)
                (target_definitions . t))))

(defun helm-kythe-post-dir (path)
  (condition-case ex
      (helm-kythe-post "/dir" `((path . ,path)))
    ('helm-kythe-error (error "%s: %s" (nth 1 (backtrace-frame 4)) (cdr ex)))))

(defun helm-kythe-post-xrefs (ticket data)
  (condition-case ex
      (helm-kythe-post "/xrefs"
                    `((anchor_text . t)
                      (ticket . [,ticket])
                      ,@data
                      ))
    ('helm-kythe-error (error "%s: %s" (nth 1 (backtrace-frame 4)) (cdr ex)))))

(defun helm-kythe-apply-decorations ()
  (interactive)
  (with-silent-modifications
    (cl-loop for prop in '(helm-kythe-definition helm-kythe-reference) do
          (put-text-property (point-min) (point-max) prop nil))
    (helm-kythe--set-mode-line 'helm-kythe-inactive)
    (when-let (filename (buffer-file-name))
      (if (eq major-mode 'haskell-mode)
          (-when-let* [(project-root (helm-kythe--haskell-find-project-root))
                       (filename (buffer-file-name))]
            (condition-case ex
                (helm-kythe-decorations (concat (file-name-nondirectory project-root) "/" (file-relative-name filename project-root)))
              ('helm-kythe-error (error "helm-kythe-apply-decorations: %s" (cdr ex)))))
        (cl-loop for search-path in helm-kythe-file-search-paths do
              (when-let (path (file-relative-name filename search-path))
                (helm-kythe-decorations path)
                (return)))))))

;; .cross_references | values[].definition[].anchor
(defun helm-kythe-get-definitions (ticket)
  (when-let (xrefs (-some->> (helm-kythe-post-xrefs ticket '((definition_kind . "BINDING_DEFINITIONS")))
                             (assoc 'cross_references) (cdr)))
    (-mapcat (lambda (xref)
               (when-let (defs (alist-get 'definition (cdr xref)))
                 (mapcar (lambda (x) (alist-get 'anchor x)) defs)
                 )) xrefs)))

;; .cross_references | values[].reference[].anchor
(defun helm-kythe-get-references (ticket)
  (when-let (xrefs (-some->> (helm-kythe-post-xrefs ticket '((reference_kind . "ALL_REFERENCES")))
                             (assoc 'cross_references) (cdr)))
    (-mapcat (lambda (xref)
                   (when-let (refs (alist-get 'reference (cdr xref)))
                     ;; 'start' of /kythe/edge/ref/doc do not have column_offset
                     ;; (-filter (lambda (x) (not (equal (alist-get 'kind x) "/kythe/edge/ref/doc"))) (mapcar (lambda (x) (alist-get 'anchor x)) refs))
                     (mapcar (lambda (x) (alist-get 'anchor x)) refs)
                     )) xrefs)))

(defun helm-kythe-get-xrefs (ticket)
  (-some->> (helm-kythe-post-xrefs ticket '((declaration_kind . "ALL_DECLARATIONS")
                              (definition_kind . "BINDING_DEFINITIONS")
                              (reference_kind . "ALL_REFERENCES")
                              ))
           (assoc 'cross_references) (cdr)))

(defun helm-kythe-decorations (filepath)
  (when-let (refs (helm-kythe-post-decorations (concat "kythe://?path=" filepath)))
    (mapc (lambda (ref)
            (-let [(start . end) (helm-kythe--char-offsets ref 'anchor_start 'anchor_end)]
              (when end
                (put-text-property start end 'helm-kythe-reference ref)))) (alist-get 'reference refs))
    (mapc (lambda (ticket-def)
            (-let [def (cdr ticket-def)]
              ;; cxx_extractor or cxx_indexer, "definition_locations" of a.cc may include tickets of "stdio.h"
              (when (equal filepath (helm-kythe--path-from-ticket (alist-get 'ticket def)))
                (-let [(start . end) (helm-kythe--char-offsets def 'start 'end)]
                  (when end
                    (put-text-property start end 'helm-kythe-definition def)))))) (alist-get 'definition_locations refs))
    (helm-kythe--set-mode-line 'helm-kythe-active)
    nil))

(defun helm-kythe-eldoc-function ()
  (-when-let* [(ticket (helm-kythe--reference-target-at-point))
               (defs (helm-kythe-get-definitions ticket))]
    (-let [doc (alist-get 'snippet (car defs))]
      (if (eq major-mode 'haskell-mode)
          (helm-kythe--fontify-haskell doc)
        doc))))

(defun helm-kythe-find-definitions ()
  (interactive)
  (helm-kythe--common '(helm-source-helm-kythe-definitions)))

(defun helm-kythe-find-definitions-prompt ()
  (interactive)
  (helm-kythe--common '(helm-source-helm-kythe-definitions-prompt)))

(defun helm-kythe-find-references ()
  (interactive)
  (helm-kythe--common '(helm-source-helm-kythe-references)))

(defun helm-kythe-find-references-prompt ()
  (interactive)
  (helm-kythe--common '(helm-source-helm-kythe-references-prompt)))

(defun helm-kythe-dwim ()
  (interactive)
  (if (helm-kythe--definition-at-point)
      (helm-kythe-find-references)
    (helm-kythe-find-definitions)))

(defun helm-kythe-imenu ()
  (interactive)
  (helm-kythe--common '(helm-source-helm-kythe-imenu)))

(defun helm-kythe-resume ()
  (interactive)
  (unless (get-buffer helm-kythe--buffer)
    (error "Error: helm-kythe buffer does not exist."))
  (helm-resume helm-kythe--buffer))

(defvar helm-kythe-mode-map (make-sparse-keymap))
(defvar-local helm-kythe-mode-line (helm-kythe--propertize-mode-line 'helm-kythe-inactive))
(put 'helm-kythe-mode-line 'risky-local-variable t)

;;;###autoload
(define-minor-mode helm-kythe-mode ()
  "Enable helm-kythe-mode"
  :init-value nil
  :lighter helm-kythe-mode-line
  :global nil
  :keymap helm-kythe-mode-map
  (cond
   (helm-kythe-mode
    (add-function :before-until (local 'eldoc-documentation-function) #'helm-kythe-eldoc-function)
    (helm-kythe-apply-decorations))
   (t
    (remove-function (local 'eldoc-documentation-function)
                     #'helm-kythe-eldoc-function))))

(when helm-kythe-suggested-key-mapping
  (let ((command-table '(("a" . helm-kythe-apply-decorations)
                         ("d" . helm-kythe-find-definitions)
                         ;; ("D" . helm-kythe-find-definitions-prompt)
                         ("i" . helm-kythe-imenu)
                         ("r" . helm-kythe-find-references)
                         ;; ("R" . helm-kythe-find-references-prompt)
                         ("l" . helm-kythe-resume)))
        (key-func (if (string-prefix-p "\\" helm-kythe-prefix-key)
                      #'concat
                    (lambda (prefix key) (kbd (concat prefix " " key))))))
    (cl-loop for (key . command) in command-table
          do
          (define-key helm-kythe-mode-map (funcall key-func helm-kythe-prefix-key key) command))
    ))

(easy-menu-define helm-kythe-menu helm-kythe-mode-map "helm-kythe menu"
  '("Kythe"
    ["Apply decorations" helm-kythe-apply-decorations t]
    ["Find definitions" helm-kythe-find-definitions t]
    ["Find references" helm-kythe-find-references t]
    ["imenu" helm-kythe-imenu t]
    ["Resume" helm-kythe-resume t]
    ["Customize" (customize-group 'helm-kythe)]
    ))

(provide 'helm-kythe)
