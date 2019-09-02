;;; asx.el --- Ask Stack Exchange -*- lexical-binding: t; -*-

;; Author: Alex Ragone <ragonedk@gmail.com>
;; Created: 24 August 2019
;; Homepage: https://github.com/ragone/asx
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((request "0.3.0") (org "9.2.5") (seq "2") (emacs "25"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows you to search any StackExchange site specified in
;; `asx-sites' and insert the top post in an Org-mode buffer. For better searching,
;; this uses Google and DuckDuckGo as it is superior to StackExchange's searching
;; capabilities. More search engines can be defined using `asx-search-engine-alist'.
;;
;; # Usage
;; Run ~M-x asx~ and enter query when prompted.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'dom)
(require 'org)
(require 'request)
(require 'seq)
(require 'shr)
(require 'subr-x)

;;;; Customization

(defgroup asx nil
  "Ask StackExchange."
  :group 'convenience
  :version "25.1"
  :link '(emacs-commentary-link "asx.el"))

(defcustom asx-number-of-answers 3
  "Answers to include."
  :type 'number
  :group 'asx)

(defcustom asx-prompt-post-p nil
  "If non-nil, prompt for post to show.
Otherwise show the first post."
  :type 'boolean
  :group 'asx)

(defcustom asx-sites '("stackoverflow.com"
                       "stackexchange.com"
                       "superuser.com"
                       "serverfault.com"
                       "askubuntu.com")
  "Sites to search."
  :type 'list
  :group 'asx)

(defcustom asx-search-engine 'google
  "Search engine to use."
  :type 'symbol
  :group 'asx)

(defcustom asx-search-engine-alist '((google
                                      :format "https://www.google.com/search?q=%s"
                                      :extract-fn #'asx--extract-links-google)
                                     (duckduckgo
                                      :format "https://www.duckduckgo.com/?q=%s"
                                      :extract-fn #'asx--extract-links-duckduckgo))
  "Alist of search engine configurations."
  :type '(alist :key-type symbol :value-type plist)
  :group 'asx)

(defcustom asx-buffer-name "*AskStackExchange*"
  "Name of buffer to insert post."
  :type 'string
  :group 'asx)

;;;; Variables

(defvar asx--current-post-index 0
  "Current post index.")

(defvar asx--posts nil
  "List of posts.")

(defvar asx--user-agents
  '("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:11.0) Gecko/20100101 Firefox/11.0"
    "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:22.0) Gecko/20100 101 Firefox/22.0"
    "Mozilla/5.0 (Windows NT 6.1; rv:11.0) Gecko/20100101 Firefox/11.0"
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_4) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.46 Safari/536.5"
    "Mozilla/5.0 (Windows; Windows NT 6.1) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.46 Safari/536.5")
  "List of user agent.")

;;;; Functions

;;;;; Commands

;;;###autoload
(defun asx (query)
  "Search for QUERY."
  (interactive "sQuery: ")
  (asx--request (asx--query-construct query)
                #'asx--handle-search))

(defun asx-next-post ()
  "Go to next post."
  (interactive)
  (asx-n-post 1))

(defun asx-previous-post ()
  "Go to previous post."
  (interactive)
  (asx-n-post -1))

(defun asx-reload-post ()
  "Reload current post."
  (interactive)
  (asx-n-post 0))

(defun asx-go-back-to-first-post ()
  "Go to first post."
  (interactive)
  (asx-n-post (- asx--current-post-index)))

;;;;; Support

(defun asx--request (url callback)
  "Request URL with CALLBACK."
  (message "Loading %s" url)
  (let ((request-curl-options (list (format "-A %s" (asx--get-user-agent)))))
    (request url
             :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
             :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                   (error "%s" error-thrown)))
             :success (cl-function (lambda (&key data &allow-other-keys)
                                     (funcall callback data))))))

(defun asx--get-user-agent ()
  "Return random user-agent from `asx--user-agents'."
  (seq-random-elt asx--user-agents))

(defun asx--get-buffer ()
  "Get or create `asx-buffer-name' buffer."
  (get-buffer-create asx-buffer-name))

(defun asx--handle-search (dom)
  "Handle search for DOM."
  (setq asx--posts (asx--filter-posts
                    (asx--extract-links dom)))
  (setq asx--current-post-index
        (or (and asx-prompt-post-p
                 (cl-position (completing-read "Post: " asx--posts)
                              asx--posts
                              :test #'equal
                              :key #'car))
            0))
  (asx--request (cdr (asx--get-current-post))
                #'asx--insert-post-dom))

(defun asx--extract-links (dom)
  "Extract links from DOM."
  (funcall (cadr (plist-get (asx--get-search-engine) :extract-fn))
           dom))

(defun asx--extract-links-google (dom)
  "Extract links from Google DOM."
  (mapcar (lambda (node)
            (cons
             (dom-text (car (dom-by-class node "^ellip$")))
             (dom-attr (dom-child-by-tag node 'a) 'href)))
          (dom-by-class dom "^r$")))

(defun asx--extract-links-duckduckgo (dom)
  "Extract links from DuckDuckGo DOM."
  (mapcar (lambda (node)
            (cons
             (dom-texts (car (dom-by-class node "result__a")))
             (string-trim (dom-text (car (dom-by-class node "result__url"))))))
          (dom-by-class dom "result ")))

(defun asx--filter-posts (links)
  "Filter LINKS for questions."
  (seq-filter (lambda (link)
                (string-match "questions/[0-9]+" (cdr link)))
              links))


(defun asx--get-current-post ()
  "Return current post."
  (nth asx--current-post-index asx--posts))

(defun asx--insert-post-dom (dom)
  "Insert post DOM."
  (asx--insert-post (asx--normalize-post dom)))

(defun asx--query-construct (query)
  "Return URI for QUERY."
  (concat (format (plist-get (asx--get-search-engine) :format)
                  (url-hexify-string (asx--query-string query)))))

(defun asx--get-search-engine ()
  "Return the search engine from `asx-search-engine-list'."
  (alist-get asx-search-engine asx-search-engine-alist))

(defun asx--query-string (query)
  "Return the query string for QUERY."
  (concat query " " (asx--query-string-sites)))

(defun asx--query-string-sites ()
  "Return query string for sites to search."
  (mapconcat (lambda (site)
               (concat "site:" site))
             asx-sites
             " OR "))

(defun asx--normalize-post (dom)
  "Return plist of DOM representing the post."
  (list :url (cdr (asx--get-current-post))
        :title (dom-text (car (dom-by-class dom "question-hyperlink")))
        :body (dom-by-class (car (dom-by-id dom "^question$")) "post-text")
        :score (dom-text (dom-by-class (car (dom-by-id dom "^question$")) "js-vote-count"))
        :answers (asx--get-answers dom)
        :tags (asx--get-tags dom)))

(defun asx--get-tags (dom)
  "Return tags from DOM node."
  (let ((tag-doms (dom-by-class (dom-by-class dom "^post-taglist")
                                "^post-tag$")))
    (mapcar #'dom-text tag-doms)))

(defun asx--get-answers (dom)
  "Return answers from DOM node."
  (mapcar (lambda (node)
            (let ((parent (dom-parent dom node)))
              (list :body (dom-by-class parent "post-text")
                    :score (dom-text (dom-by-class parent "js-vote-count")))))
          (dom-by-class dom "answercell")))

(defun asx--prepare-buffer ()
  "Prepare buffer to insert content."
  (let ((asx-buffer (asx--get-buffer)))
    (unless (equal (current-buffer) asx-buffer)
      (switch-to-buffer-other-window asx-buffer))
    (read-only-mode -1)
    (erase-buffer)
    (insert "#+STARTUP: overview indent\n")))

(defun asx--finalize-buffer ()
  "Finalize buffer."
  (delete-trailing-whitespace)
  (org-mode)
  (visual-line-mode)
  (goto-char (point-min)))

(defun asx--insert-post (post)
  "Insert POST into `asx-buffer-name'."
  (asx--prepare-buffer)
  (asx--insert-question post)
  (asx--insert-answers
   (seq-take (plist-get post :answers) asx-number-of-answers))
  (asx--finalize-buffer))

(defun asx--insert-question (question)
  "Insert QUESTION."
  (insert
   (concat "#+TITLE: " (plist-get question :title)
           "\n"
           (plist-get question :url)
           "\n"
           (format "* Question (%s)" (plist-get question :score))))
  (asx--insert-node (plist-get question :body))
  (asx--insert-tags (plist-get question :tags)))

(defun asx--insert-tags (tags)
  "Insert TAGS."
  (insert "\nTags: ")
  (mapc (lambda (tag)
          (insert tag)
          (insert " "))
        tags))

(defun asx--insert-answers (answers)
  "Insert ANSWERS."
  (let ((first-answer t))
    (mapcar (lambda (answer)
              (insert (format "\n* Answer (%s)" (plist-get answer :score)))
              (when first-answer
                (insert "\n:PROPERTIES:\n:VISIBILITY: all\n:END:\n")
                (setq first-answer nil))
              (asx--insert-node (plist-get answer :body)))
            answers)))

(defun asx--map-node (node)
  "Map NODE to Org compatible DOM node."
  (if (and (listp node)
           (listp (cdr node)))
      (cond
       ((and (equal (car node) 'a)
             (not (dom-by-tag node 'img)))
        (asx--map-node-to-link node))
       ((equal (car node) 'pre)
        (dom-node 'pre
                  '()
                  "#+BEGIN_EXAMPLE "
                  (or (asx--get-language-maybe node) "prog")
                  "\n"
                  (nthcdr 2 node)
                  (if (dom-attr node 'class)
                      "\n")
                  "#+END_EXAMPLE"))
       (t (mapcar #'asx--map-node node)))
    node))

(defun asx--get-language-maybe (node)
  "Return language from NODE, if any."
  (if-let ((class (dom-attr node 'class)))
      (asx--get-language-string class)))

(defun asx--get-language-string (class)
  "Return language string from CLASS."
  (when (string-match "lang-\\b\\(.+?\\)\\b" class)
    (match-string 1 class)))

(defun asx--map-node-to-link (node)
  "Return Org link from NODE."
  (concat "[[" (dom-attr node 'href) "][" (dom-texts node) "]]"))

(defun asx--insert-node (node)
  "Map NODE and insert."
  (let ((shr-bullet "- ")
        (shr-width 0)
        (shr-use-fonts nil))
    (shr-insert-document (mapcar #'asx--map-node node))))

(defun asx-n-post (n)
  "Jump N steps in `asx--posts' and insert the post."
  (setq asx--current-post-index
        (mod (+ n asx--current-post-index) (length asx--posts)))
  (asx--request (cdr (asx--get-current-post))
                #'asx--insert-post-dom))

;;;; Footer

(provide 'asx)

;;; asx.el ends here
