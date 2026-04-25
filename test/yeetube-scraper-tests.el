;;; yeetube-scraper-tests.el --- Tests for yeetube-scraper  -*- lexical-binding: t; -*-

;;; Code:

;; Workaround: Emacs 31.1 (and Guix grafts of find-func.el) defines a
;; defcustom :set for `find-function-mode-lower-precedence' that
;; references `find-function-mode' before it exists.
(unless (boundp 'find-function-mode)
  (defvar find-function-mode nil))

(require 'ert)
(require 'json)

(defvar yeetube-scraper-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file, captured at load time.")

(require 'yeetube-scraper)

(defun yeetube-scraper-test--load-fixture (name)
  "Load JSON fixture NAME from test/fixtures/ directory."
  (let ((path (expand-file-name (concat "fixtures/" name)
               yeetube-scraper-test--dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (json-parse-buffer :object-type 'alist :array-type 'list))))

;;; ---- Group 1: videoRenderer extraction ----

(ert-deftest yeetube-scraper-test-extract-video-renderer ()
  "Extract plist from a videoRenderer alist."
  (let* ((fixture (yeetube-scraper-test--load-fixture "search-videorenderer.json"))
         (renderer (alist-get 'videoRenderer fixture))
         (result (yeetube-scraper--extract-video renderer)))
    (should (equal "48JlgiBpw_I" (plist-get result :id)))
    (should (equal "The Absolute Beginner's Guide to Emacs" (plist-get result :title)))
    (should (equal "454,448 views" (plist-get result :views)))
    (should (equal "1:11:56" (plist-get result :duration)))
    (should (equal "5 years ago" (plist-get result :date)))
    (should (equal "System Crafters" (plist-get result :channel)))
    (should (equal "/@SystemCrafters" (plist-get result :channel-id)))
    (should (eq 'video (plist-get result :type)))))

(ert-deftest yeetube-scraper-test-extract-video-thumbnail-url ()
  "Thumbnail URL uses the default size variant."
  (let* ((fixture (yeetube-scraper-test--load-fixture "search-videorenderer.json"))
         (renderer (alist-get 'videoRenderer fixture))
         (result (yeetube-scraper--extract-video renderer)))
    (should (equal "https://i.ytimg.com/vi/48JlgiBpw_I/default.jpg"
                   (plist-get result :thumbnail-url)))))

(ert-deftest yeetube-scraper-test-thumbnail-url-fallback ()
  "Falls back to predictable URL when thumbnails list is nil."
  (should (equal "https://i.ytimg.com/vi/abc123/default.jpg"
                 (yeetube-scraper--thumbnail-url "abc123" nil))))

(ert-deftest yeetube-scraper-test-extract-video-missing-fields ()
  "Handles missing optional fields gracefully (empty strings, not nil)."
  (let* ((renderer '((videoId . "test123")
                     (title (runs ((text . "Test"))))))
         (result (yeetube-scraper--extract-video renderer)))
    (should (equal "test123" (plist-get result :id)))
    (should (equal "Test" (plist-get result :title)))
    (should (equal "" (plist-get result :views)))
    (should (equal "" (plist-get result :duration)))
    (should (equal "" (plist-get result :date)))
    (should (equal "" (plist-get result :channel)))
    (should (eq 'video (plist-get result :type)))))

(ert-deftest yeetube-scraper-test-extract-video-streamed-date ()
  "Strips 'Streamed ' prefix from dates."
  (let* ((renderer '((videoId . "test123")
                     (title (runs ((text . "Test"))))
                     (publishedTimeText (simpleText . "Streamed 2 days ago"))))
         (result (yeetube-scraper--extract-video renderer)))
    (should (equal "2 days ago" (plist-get result :date)))))

;;; ---- Group 2: lockupViewModel playlist extraction ----

(ert-deftest yeetube-scraper-test-extract-playlist ()
  "Extract plist from a lockupViewModel playlist."
  (let* ((fixture (yeetube-scraper-test--load-fixture "search-lockupviewmodel.json"))
         (renderer (alist-get 'lockupViewModel fixture))
         (result (yeetube-scraper--extract-playlist renderer)))
    (should (equal "PLlONLmJCfHToFfk1WgTOwZMfIr062jIN9" (plist-get result :id)))
    (should (equal "Emacs From Scratch" (plist-get result :title)))
    (should (equal "" (plist-get result :views)))
    (should (equal "15 videos" (plist-get result :duration)))
    (should (equal "" (plist-get result :date)))
    (should (equal "lxsameer" (plist-get result :channel)))
    (should (equal "" (plist-get result :channel-id)))
    (should (eq 'playlist (plist-get result :type)))))

(ert-deftest yeetube-scraper-test-playlist-thumbnail ()
  "Playlist thumbnail URL is extracted from collectionThumbnailViewModel."
  (let* ((fixture (yeetube-scraper-test--load-fixture "search-lockupviewmodel.json"))
         (renderer (alist-get 'lockupViewModel fixture))
         (result (yeetube-scraper--extract-playlist renderer)))
    (should (equal "https://i.ytimg.com/vi/xxx/hqdefault.jpg"
                   (plist-get result :thumbnail-url)))))

(ert-deftest yeetube-scraper-test-playlist-video-count ()
  "Video count is found by searching for the row containing 'video'."
  (let ((rows '(((metadataParts ((text (content . "SomeChannel")))))
                ((metadataParts ((text (content . "42 videos"))))))))
    (should (equal "42 videos"
                   (yeetube-scraper--playlist-video-count rows)))))

(ert-deftest yeetube-scraper-test-playlist-channel ()
  "Channel name comes from the first metadata row."
  (let ((rows '(((metadataParts ((text (content . "ChannelName")))))
                ((metadataParts ((text (content . "10 videos"))))))))
    (should (equal "ChannelName"
                   (yeetube-scraper--playlist-channel rows)))))

;;; ---- Group 3: dispatch-item ----

(ert-deftest yeetube-scraper-test-dispatch-video ()
  "Dispatch routes videoRenderer to extract-video."
  (let* ((item '((videoRenderer
                  (videoId . "abc123")
                  (title (runs ((text . "A Video")))))))
         (result (yeetube-scraper--dispatch-item item)))
    (should (equal "abc123" (plist-get result :id)))
    (should (eq 'video (plist-get result :type)))))

(ert-deftest yeetube-scraper-test-dispatch-playlist ()
  "Dispatch routes lockupViewModel playlist to extract-playlist."
  (let* ((item `((lockupViewModel
                  (contentId . "PLxxx")
                  (contentType . "LOCKUP_CONTENT_TYPE_PLAYLIST")
                  (metadata
                   (lockupMetadataViewModel
                    (title (content . "My Playlist"))
                    (metadata
                     (contentMetadataViewModel
                      (metadataRows
                       ((metadataParts ((text (content . "Chan")))))
                       ((metadataParts ((text (content . "5 videos"))))))))))
                  (contentImage
                   (collectionThumbnailViewModel
                    (primaryThumbnail
                     (thumbnailViewModel
                      (image
                       (sources ((url . "https://example.com/thumb.jpg")))))))))))
         (result (yeetube-scraper--dispatch-item item)))
    (should (equal "PLxxx" (plist-get result :id)))
    (should (eq 'playlist (plist-get result :type)))))

(ert-deftest yeetube-scraper-test-dispatch-unknown ()
  "Dispatch returns nil for unrecognized item types."
  (let ((item '((adSlot (something . "ad data")))))
    (should-not (yeetube-scraper--dispatch-item item))))

;;; ---- Group 4: continuation token extraction ----

(ert-deftest yeetube-scraper-test-extract-continuation ()
  "Extract continuation token and URL from sections."
  (let* ((sections
          `(((itemSectionRenderer (contents)))
            ((continuationItemRenderer
              (continuationEndpoint
               (continuationCommand (token . "abc_token_123"))
               (commandMetadata
                (webCommandMetadata
                 (apiUrl . "/youtubei/v1/search"))))))))
         (result (yeetube-scraper--extract-continuation sections)))
    (should (equal "abc_token_123" (plist-get result :token)))
    (should (equal "/youtubei/v1/search" (plist-get result :url)))))

(ert-deftest yeetube-scraper-test-extract-continuation-absent ()
  "Return nil when no continuationItemRenderer exists."
  (let ((sections '(((itemSectionRenderer (contents))))))
    (should-not (yeetube-scraper--extract-continuation sections))))

;;; ---- Group 5: full buffer parsing ----

(defun yeetube-scraper-test--make-video-renderer (id title)
  "Build a minimal videoRenderer alist with ID and TITLE.
Uses vectors for JSON arrays, alists for objects."
  `((videoRenderer . ((videoId . ,id)
                      (title . ((runs . ,(vector `((text . ,title))))))))))

(defun yeetube-scraper-test--make-continuation (token url)
  "Build a continuationItemRenderer alist with TOKEN and URL."
  `((continuationItemRenderer
    . ((continuationEndpoint
        . ((continuationCommand . ((token . ,token)))
           (commandMetadata
            . ((webCommandMetadata . ((apiUrl . ,url)))))))))))

(defun yeetube-scraper-test--make-playlist-lockup (id title channel video-count thumb-url)
  "Build a lockupViewModel playlist alist.
Uses vectors for JSON arrays, alists for objects."
  (let* ((ch-part `((metadataParts . ,(vector `((text . ((content . ,channel))))))))
         (vc-part `((metadataParts . ,(vector `((text . ((content . ,video-count))))))))
         (rows (vector ch-part vc-part))
         (content-meta `((contentMetadataViewModel . ((metadataRows . ,rows)))))
         (lockup-meta `((lockupMetadataViewModel
                         . ((title . ((content . ,title)))
                            (metadata . ,content-meta)))))
         (thumb-sources (vector `((url . ,thumb-url))))
         (content-image `((collectionThumbnailViewModel
                           . ((primaryThumbnail
                               . ((thumbnailViewModel
                                   . ((image . ((sources . ,thumb-sources)))))))))))
         (vm `((contentId . ,id)
               (contentType . "LOCKUP_CONTENT_TYPE_PLAYLIST")
               (metadata . ,lockup-meta)
               (contentImage . ,content-image))))
    `((lockupViewModel . ,vm))))

(defun yeetube-scraper-test--insert-yt-initial-data (data)
  "Insert ytInitialData JSON into the current buffer from DATA alist."
  (insert "var ytInitialData = ")
  (insert (json-encode data))
  (insert ";"))

(defun yeetube-scraper-test--make-search-data (items continuation)
  "Build ytInitialData for a search page with ITEMS and CONTINUATION.
Uses vectors for JSON arrays, alists for objects."
  (let* ((item-section `((itemSectionRenderer
                          . ((contents . ,(apply #'vector items))))))
         (section-contents (if continuation
                               (vector item-section continuation)
                             (vector item-section)))
         (section-list `(sectionListRenderer
                         . ((contents . ,section-contents)))))
    `((contents . ((twoColumnSearchResultsRenderer
                    . ((primaryContents . (,section-list)))))))))

(defun yeetube-scraper-test--make-channel-data (grid-items continuation)
  "Build ytInitialData for a channel page with GRID-ITEMS and CONTINUATION.
Uses vectors for JSON arrays, alists for objects."
  (let* ((grid-contents (if continuation
                            (apply #'vector (append grid-items (list continuation)))
                          (apply #'vector grid-items)))
         (tab `((tabRenderer . ((selected . t)
                                (content . ((richGridRenderer
                                             . ((contents . ,grid-contents))))))))))
    `((contents . ((twoColumnBrowseResultsRenderer
                    . ((tabs . ,(vector tab)))))))))

(ert-deftest yeetube-scraper-test-parse-search-buffer ()
  "Parse a minimal ytInitialData search page from a buffer."
  (with-temp-buffer
    (let* ((vid1 (yeetube-scraper-test--make-video-renderer "vid1" "First Video"))
           (vid2 (yeetube-scraper-test--make-video-renderer "vid2" "Second Video"))
           (cont (yeetube-scraper-test--make-continuation "cont_tok" "/youtubei/v1/search"))
           (data (yeetube-scraper-test--make-search-data (list vid1 vid2) cont)))
      (yeetube-scraper-test--insert-yt-initial-data data)
      (let ((result (yeetube-scraper-parse)))
        (should (= 2 (length (plist-get result :items))))
        (should (equal "vid1" (plist-get (car (plist-get result :items)) :id)))
        (should (equal "vid2" (plist-get (cadr (plist-get result :items)) :id)))
        (should (equal "cont_tok"
                       (plist-get (plist-get result :continuation) :token)))))))

(ert-deftest yeetube-scraper-test-parse-channel-buffer ()
  "Parse a minimal ytInitialData channel page from a buffer."
  (with-temp-buffer
    (let* ((vid (yeetube-scraper-test--make-video-renderer "ch_vid1" "Channel Video"))
           (rich-item `((richItemRenderer . ((content . ,vid)))))
           (cont (yeetube-scraper-test--make-continuation "ch_cont" "/youtubei/v1/browse"))
           (data (yeetube-scraper-test--make-channel-data (list rich-item) cont)))
      (yeetube-scraper-test--insert-yt-initial-data data)
      (let ((result (yeetube-scraper-parse)))
        (should (= 1 (length (plist-get result :items))))
        (should (equal "ch_vid1"
                       (plist-get (car (plist-get result :items)) :id)))
        (should (equal "ch_cont"
                       (plist-get (plist-get result :continuation) :token)))))))

(ert-deftest yeetube-scraper-test-parse-mixed-search ()
  "Parse search results containing both videos and playlists."
  (with-temp-buffer
    (let* ((vid (yeetube-scraper-test--make-video-renderer "v1" "A Video"))
           (pl (yeetube-scraper-test--make-playlist-lockup
                "PLxxx" "A Playlist" "Chan" "3 videos"
                "https://example.com/t.jpg"))
           (data (yeetube-scraper-test--make-search-data (list vid pl) nil)))
      (yeetube-scraper-test--insert-yt-initial-data data)
      (let* ((result (yeetube-scraper-parse))
             (items (plist-get result :items)))
        (should (= 2 (length items)))
        (should (eq 'video (plist-get (car items) :type)))
        (should (eq 'playlist (plist-get (cadr items) :type)))
        (should (equal "PLxxx" (plist-get (cadr items) :id)))))))

(provide 'yeetube-scraper-tests)
;;; yeetube-scraper-tests.el ends here
