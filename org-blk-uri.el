(require 'org)
(require 'dash)
(require 's)
(require 'org-web-tools)

(defun org-blk-uri-ext-to-babel-id (extension)
  "Find babel src block language from EXTENSION."
  (--> auto-mode-alist
       (--find (s-match (car it) extension) it)
       cdr
       symbol-name
       (s-replace "-mode" "" it)))

(defun org-blk-uri-file-to-org-block-string (filepath)
  "Make a Org src block string from FILEPATH."
  (let* ((extension (file-name-extension filepath))
         (contents (with-temp-buffer
                     (insert-file-contents filepath)
                     (when (string= extension "org") (org-escape-code-in-region (point-min) (point-max)))
                     (buffer-string)))
         (babel-id (org-blk-uri-ext-to-babel-id (concat "." extension))))
    (concat "#+begin_src " (if babel-id babel-id extension) " :tangle " filepath "\n"  (my/s-indent contents 2) "\n" "#+end_src")))

(defun org-blk-uri-org-src-from-filename (filepath)
  "Insert the org src block corresponding to FILEPATH."
  (interactive)
  (let ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (or (bounds-of-thing-at-point 'filename)
                      `(,(point) . ,(point))))))
    (save-excursion
      (when (and bounds filepath)
        (delete-region (car bounds) (cdr bounds))
        (insert (org-blk-uri-file-to-org-block-string filepath))))))

(defun org-blk-uri-org-web-tools--url-as-readable-org (&optional url)
  "Return string containing Org entry of URL's web page content.
Content is processed with `eww-readable' and Pandoc.  Entry will
be a top-level heading, with article contents below a
second-level \"Article\" heading, and a timestamp in the
first-level entry for writing comments."
  ;; By taking an optional URL, and getting it from the clipboard if
  ;; none is given, this becomes suitable for use in an org-capture
  ;; template, like:

  ;; ("wr" "Capture Web site with eww-readable" entry
  ;;  (file "~/org/articles.org")
  ;;  "%(org-web-tools--url-as-readable-org)")
  (-let* ((url (or url (org-web-tools--get-first-url)))
          (html (org-web-tools--get-url url))
          ((title . readable) (org-web-tools--eww-readable html))
          (title (org-web-tools--cleanup-title (or title "")))
          (converted (org-web-tools--html-to-org-with-pandoc readable))
          (link (org-make-link-string url title))
          (timestamp (format-time-string (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]"))))
    (with-temp-buffer
      (org-mode)
      ;; Insert article text
      (insert converted)
      ;; Demote in-article headings
      (org-web-tools--demote-headings-below 2)
      ;; Insert headings at top
      (goto-char (point-min))
      (insert "* " link " :website:" "\n\n")
      (buffer-string))))

(defun org-blk-uri-org-src-from-url (url)
  "Inserts a src block downloading and formatting as org the URL contents"
  (let (
        (bounds (if (use-region-p) (cons (region-beginning) (region-end)) (bounds-of-thing-at-point 'url)))
        (org-contents
         (concat
          "#+NAME: from "
          url
          "\n"
          "#+BEGIN_SRC org\n"
          (org-escape-code-in-string (org-blk-uri-org-web-tools--url-as-readable-org url))
          "#+END_SRC"
          )))
    (save-excursion
      (when (and bounds url)
        (delete-region (car bounds) (cdr bounds))
        (insert org-contents)))))

(defun org-blk-uri-org-src-from-filename-at-point ()
  "Inserts src block from filename at point."
  (interactive)
  (let* ((filename (if (use-region-p) (buffer-substring (region-beginning) (region-end)) (thing-at-point 'filename)))
         (url (thing-at-point-url-at-point)))
    (if url
        (org-blk-uri-org-src-from-url url)
      (org-blk-uri-org-src-from-filename filename))))

(provide 'org-blk-uri)
