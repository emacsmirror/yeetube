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

;;; Group 1: videoRenderer extraction

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

;;; Group 1b: text-from-object helper

(ert-deftest yeetube-scraper-test-text-from-object-simple-text ()
  "Returns the `simpleText' string when present."
  (should (equal "Hello"
                 (yeetube-scraper--text-from-object
                  '((simpleText . "Hello"))))))

(ert-deftest yeetube-scraper-test-text-from-object-single-run ()
  "Returns the lone run's text when only `runs' is present."
  (should (equal "Hello"
                 (yeetube-scraper--text-from-object
                  '((runs ((text . "Hello"))))))))

(ert-deftest yeetube-scraper-test-text-from-object-multi-runs ()
  "Concatenates every run's text in order."
  (should (equal "Hello, world!"
                 (yeetube-scraper--text-from-object
                  '((runs ((text . "Hello, "))
                          ((text . "world!"))))))))

(ert-deftest yeetube-scraper-test-text-from-object-nil ()
  "Returns nil for a nil object or empty wrapper."
  (should-not (yeetube-scraper--text-from-object nil))
  (should-not (yeetube-scraper--text-from-object '((foo . "bar")))))

(ert-deftest yeetube-scraper-test-extract-video-views-from-runs ()
  "Falls through to `runs' when `viewCountText' lacks `simpleText'."
  (let* ((renderer '((videoId . "rid1")
                     (title (runs ((text . "T"))))
                     (viewCountText (runs ((text . "1.2K "))
                                          ((text . "views"))))))
         (result (yeetube-scraper--extract-video renderer)))
    (should (equal "1.2K views" (plist-get result :views)))))

(ert-deftest yeetube-scraper-test-extract-video-multi-run-title ()
  "Multi-segment titles are concatenated, not truncated to the first run."
  (let* ((renderer '((videoId . "rid2")
                     (title (runs ((text . "Part 1 — "))
                                  ((text . "Part 2"))))))
         (result (yeetube-scraper--extract-video renderer)))
    (should (equal "Part 1 — Part 2" (plist-get result :title)))))

(ert-deftest yeetube-scraper-test-extract-video-channel-from-owner-text ()
  "Falls back to `ownerText' when `longBylineText' is missing."
  (let* ((renderer '((videoId . "rid3")
                     (title (runs ((text . "T"))))
                     (ownerText (runs ((text . "OwnerChannel"))))))
         (result (yeetube-scraper--extract-video renderer)))
    (should (equal "OwnerChannel" (plist-get result :channel)))))

(ert-deftest yeetube-scraper-test-extract-video-channel-from-short-byline ()
  "Falls back to `shortBylineText' when longByline + ownerText are missing."
  (let* ((renderer '((videoId . "rid4")
                     (title (runs ((text . "T"))))
                     (shortBylineText (runs ((text . "ShortName"))))))
         (result (yeetube-scraper--extract-video renderer)))
    (should (equal "ShortName" (plist-get result :channel)))))

;;; Group 2: lockupViewModel playlist extraction

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

;;; Group 3: dispatch-item

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

(defun yeetube-scraper-test--make-video-lockup (id title views date duration)
  "Build a VIDEO lockupViewModel alist with the given fields."
  `((lockupViewModel
     (contentId . ,id)
     (contentType . "LOCKUP_CONTENT_TYPE_VIDEO")
     (metadata
      (lockupMetadataViewModel
       (title (content . ,title))
       (metadata
        (contentMetadataViewModel
         (metadataRows
          ((metadataParts ((text (content . ,views)))
                          ((text (content . ,date))))))))))
     (contentImage
      (thumbnailViewModel
       (image (sources ((url . ,(format "https://i.ytimg.com/vi/%s/hqdefault.jpg?sqp=x" id))
                        (width . 168) (height . 94))))
       (overlays
        ((thumbnailBottomOverlayViewModel
          (badges ((thumbnailBadgeViewModel (text . ,duration))))))))))))

(ert-deftest yeetube-scraper-test-dispatch-video-lockup ()
  "Dispatch routes VIDEO lockupViewModel to extract-video-lockup."
  (let* ((item (yeetube-scraper-test--make-video-lockup
                "vidLockup1" "Lockup Video" "1.2K views" "2 weeks ago" "12:34"))
         (result (yeetube-scraper--dispatch-item item)))
    (should (equal "vidLockup1" (plist-get result :id)))
    (should (equal "Lockup Video" (plist-get result :title)))
    (should (equal "1.2K views" (plist-get result :views)))
    (should (equal "12:34" (plist-get result :duration)))
    (should (equal "2 weeks ago" (plist-get result :date)))
    (should (equal "https://i.ytimg.com/vi/vidLockup1/default.jpg"
                   (plist-get result :thumbnail-url)))
    (should (eq 'video (plist-get result :type)))))

(ert-deftest yeetube-scraper-test-extract-grid-items-handles-lockup ()
  "Grid items are extracted from lockupViewModel-wrapped richItemRenderer."
  (let* ((lockup (yeetube-scraper-test--make-video-lockup
                  "gridLockup" "Grid Video" "500 views" "3 days ago" "5:00"))
         (grid-entry `((richItemRenderer (content . ,lockup))))
         (items (yeetube-scraper--extract-grid-items (list grid-entry))))
    (should (= 1 (length items)))
    (should (equal "gridLockup" (plist-get (car items) :id)))
    (should (equal "Grid Video" (plist-get (car items) :title)))
    (should (equal "5:00" (plist-get (car items) :duration)))))

(ert-deftest yeetube-scraper-test-lockup-views-and-date-handles-swapped-order ()
  "views/date detection works regardless of metadataParts order."
  (let ((parts1 '(((text (content . "999 views"))) ((text (content . "1 month ago")))))
        (parts2 '(((text (content . "1 month ago"))) ((text (content . "999 views"))))))
    (should (equal '("999 views" . "1 month ago")
                   (yeetube-scraper--lockup-views-and-date parts1)))
    (should (equal '("999 views" . "1 month ago")
                   (yeetube-scraper--lockup-views-and-date parts2)))))

;;; Group 4: continuation token extraction

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

;;; Group 5: full buffer parsing

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

;;; Group 6: continuation response shapes

(defun yeetube-scraper-test--cont-json (commands &optional root)
  "Wrap COMMANDS under continuation response ROOT."
  `((,(or root 'onResponseReceivedCommands) . ,commands)))

(defun yeetube-scraper-test--append-action (items)
  "Build appendContinuationItemsAction command holding ITEMS."
  `((appendContinuationItemsAction
     (continuationItems . ,items))))

(defun yeetube-scraper-test--reload-command (items)
  "Build reloadContinuationItemsCommand holding ITEMS."
  `((reloadContinuationItemsCommand
     (continuationItems . ,items))))

(ert-deftest yeetube-scraper-test-cont-search-section-items ()
  "Search-shaped first-append continuation still yields section items."
  (let* ((vid `((videoRenderer
                 (videoId . "s1")
                 (title (runs ((text . "Search Vid")))))))
         (sec `((itemSectionRenderer (contents . ,(list vid)))))
         (json (yeetube-scraper-test--cont-json
                (list (yeetube-scraper-test--append-action (list sec)))))
         (parsed (yeetube-scraper-parse-continuation-response json))
         (items (plist-get parsed :items)))
    (should (= 1 (length items)))
    (should (equal "s1" (plist-get (car items) :id)))))

(ert-deftest yeetube-scraper-test-cont-richitem-lockup ()
  "Channel-style richItem+lockup continuation yields items."
  (let* ((lockup (yeetube-scraper-test--make-video-lockup
                  "vid1" "Grid Cont" "9 views" "1 day ago" "1:11"))
         (entry `((richItemRenderer (content . ,lockup))))
         (json (yeetube-scraper-test--cont-json
                (list (yeetube-scraper-test--append-action (list entry)))))
         (parsed (yeetube-scraper-parse-continuation-response json))
         (items (plist-get parsed :items)))
    (should (= 1 (length items)))
    (should (equal "vid1" (plist-get (car items) :id)))
    (should (equal "Grid Cont" (plist-get (car items) :title)))))

(ert-deftest yeetube-scraper-test-cont-bare-video-and-lockup ()
  "Bare videoRenderer and VIDEO lockup continuation items dispatch."
  (let* ((bare `((videoRenderer
                  (videoId . "bare1")
                  (title (runs ((text . "Bare")))))))
         (lockup (yeetube-scraper-test--make-video-lockup
                  "lock1" "Lock Cont" "1 views" "2 days ago" "0:30"))
         (json (yeetube-scraper-test--cont-json
                (list (yeetube-scraper-test--append-action
                       (list bare lockup)))))
         (parsed (yeetube-scraper-parse-continuation-response json))
         (items (plist-get parsed :items)))
    (should (= 2 (length items)))
    (should (equal "bare1" (plist-get (car items) :id)))
    (should (equal "lock1" (plist-get (cadr items) :id)))))

(ert-deftest yeetube-scraper-test-cont-reload-command ()
  "Reload commands parse under each continuation response root."
  (let* ((vid `((videoRenderer
                 (videoId . "r1")
                 (title (runs ((text . "Reload")))))))
         (sec `((itemSectionRenderer (contents . ,(list vid))))))
    (dolist (root '(onResponseReceivedCommands
                    onResponseReceivedActions
                    onResponseReceivedEndpoints))
      (let* ((json (yeetube-scraper-test--cont-json
                    (list (yeetube-scraper-test--reload-command (list sec)))
                    root))
             (parsed (yeetube-scraper-parse-continuation-response json))
             (items (plist-get parsed :items)))
        (should (= 1 (length items)))
        (should (equal "r1" (plist-get (car items) :id)))))))

(ert-deftest yeetube-scraper-test-cont-non-first-append ()
  "Non-first append actions parse under each continuation response root."
  (let* ((vid `((videoRenderer
                 (videoId . "n2")
                 (title (runs ((text . "Second")))))))
         (sec `((itemSectionRenderer (contents . ,(list vid))))))
    (dolist (root '(onResponseReceivedCommands
                    onResponseReceivedActions
                    onResponseReceivedEndpoints))
      (let* ((json (yeetube-scraper-test--cont-json
                    (list '((unrelatedCommand (x . t)))
                          (yeetube-scraper-test--append-action (list sec)))
                    root))
             (parsed (yeetube-scraper-parse-continuation-response json))
             (items (plist-get parsed :items)))
        (should (= 1 (length items)))
        (should (equal "n2" (plist-get (car items) :id)))))))

(ert-deftest yeetube-scraper-test-continuation-contents-containers ()
  "Grid and section continuation containers yield items and next tokens."
  (let* ((vid `((videoRenderer
                 (videoId . "c1")
                 (title (runs ((text . "Container")))))))
         (sec `((itemSectionRenderer (contents . ,(list vid)))))
         (continuations '(((nextContinuationData
                            (continuation . "container_tok"))))))
    (dolist (case `((gridContinuation items ,(list vid))
                    (sectionListContinuation contents ,(list sec))))
      (let* ((container-key (nth 0 case))
             (items-key (nth 1 case))
             (entries (nth 2 case))
             (container (list
                         (cons container-key
                               (list (cons items-key entries)
                                     (cons 'continuations continuations)))))
             (json (list (cons 'continuationContents container)))
             (parsed (yeetube-scraper-parse-continuation-response json)))
        (should (equal "c1"
                       (plist-get (car (plist-get parsed :items)) :id)))
        (should (equal "container_tok"
                       (plist-get (plist-get parsed :continuation) :token)))))))

(ert-deftest yeetube-scraper-test-cont-token-still-extracted ()
  "Continuation token/url still extracted when present among cont items."
  (let* ((vid `((videoRenderer
                 (videoId . "t1")
                 (title (runs ((text . "Tok")))))))
         (sec `((itemSectionRenderer (contents . ,(list vid)))))
         (cont (yeetube-scraper-test--make-continuation
                "next_tok" "/youtubei/v1/search"))
         (json (yeetube-scraper-test--cont-json
                (list (yeetube-scraper-test--append-action
                       (list sec cont)))))
         (parsed (yeetube-scraper-parse-continuation-response json))
         (c (plist-get parsed :continuation)))
    (should (equal "t1" (plist-get (car (plist-get parsed :items)) :id)))
    (should (equal "next_tok" (plist-get c :token)))
    (should (equal "/youtubei/v1/search" (plist-get c :url)))))

;;; Group 7: lockup channel identity

(defun yeetube-scraper-test--make-video-lockup-with-channel
    (id title channel browse-id channel-path views date duration)
  "Build VIDEO lockup including channel identity in metadata."
  (let ((item (yeetube-scraper-test--make-video-lockup
               id title views date duration)))
    ;; Inject a channel metadata part with browseEndpoint before views/date.
    (let* ((lockup (alist-get 'lockupViewModel item))
           (meta (alist-get 'metadata lockup))
           (lmvm (alist-get 'lockupMetadataViewModel meta))
           (cm (alist-get 'metadata lmvm))
           (cmvm (alist-get 'contentMetadataViewModel cm))
           (rows (alist-get 'metadataRows cmvm))
           (chan-part
            `((text (content . ,channel)
                    (commandRuns
                     ((onTap
                       (innertubeCommand
                        (browseEndpoint
                         (browseId . ,browse-id)
                         (canonicalBaseUrl . ,channel-path))))))))))
      (setf (alist-get 'metadataRows cmvm)
            (cons `((metadataParts ,chan-part)) rows))
      item)))

(ert-deftest yeetube-scraper-test-lockup-channel-from-json ()
  "VIDEO lockup extracts channel identity when metadata provides it."
  (let* ((item (yeetube-scraper-test--make-video-lockup-with-channel
                "L1" "T" "ChanName" "UCabc" "/@chan"
                "10 views" "1 day ago" "1:00"))
         (result (yeetube-scraper--dispatch-item item)))
    (should (equal "ChanName" (plist-get result :channel)))
    (should (equal "/@chan" (plist-get result :channel-id)))
    (should (equal "UCabc" (plist-get result :browse-id)))
    (should (equal "10 views" (plist-get result :views)))
    (should (equal "1 day ago" (plist-get result :date)))))

(ert-deftest yeetube-scraper-test-lockup-channel-empty-without-source ()
  "VIDEO lockup identity stays empty when neither JSON nor defaults apply."
  (let* ((item (yeetube-scraper-test--make-video-lockup
                "L2" "T" "10 views" "1 day ago" "1:00"))
         (result (yeetube-scraper--dispatch-item item)))
    (should (equal "" (plist-get result :channel)))
    (should (equal "" (plist-get result :channel-id)))
    (should (equal "" (plist-get result :browse-id)))))

(ert-deftest yeetube-scraper-test-fill-channel-identity ()
  "Defaults fill only empty channel fields."
  (let* ((empty '(:id "a" :channel "" :channel-id "" :browse-id "" :type video))
         (partial '(:id "b" :channel "Have" :channel-id "" :browse-id "" :type video))
         (full '(:id "c" :channel "C" :channel-id "/@c" :browse-id "UCc" :type video))
         (defaults '(:channel "Def" :channel-id "/@def" :browse-id "UCdef"))
         (out (yeetube-scraper-fill-channel-identity
               (list empty partial full) defaults)))
    (should (equal "Def" (plist-get (nth 0 out) :channel)))
    (should (equal "/@def" (plist-get (nth 0 out) :channel-id)))
    (should (equal "UCdef" (plist-get (nth 0 out) :browse-id)))
    (should (equal "Have" (plist-get (nth 1 out) :channel)))
    (should (equal "/@def" (plist-get (nth 1 out) :channel-id)))
    (should (equal "UCdef" (plist-get (nth 1 out) :browse-id)))
    (should (equal "C" (plist-get (nth 2 out) :channel)))
    (should (equal "/@c" (plist-get (nth 2 out) :channel-id)))
    (should (equal "UCc" (plist-get (nth 2 out) :browse-id)))))

(ert-deftest yeetube-scraper-test-parse-channel-applies-page-identity ()
  "Channel page header identity fills lockup rows lacking it."
  (with-temp-buffer
    (let* ((lockup (yeetube-scraper-test--make-video-lockup
                    "chv1" "Chan Vid" "1 views" "1 day ago" "2:00"))
           (rich `((richItemRenderer (content . ,lockup))))
           (data (yeetube-scraper-test--make-channel-data (list rich) nil)))
      ;; Real pages put identity under root metadata, not contents.
      (setf data
            (append
             data
             '((metadata
                (channelMetadataRenderer
                 (title . "PageChan")
                 (externalId . "UCpage")
                 (vanityChannelUrl . "https://www.youtube.com/@pagechan"))))))
      (yeetube-scraper-test--insert-yt-initial-data data)
      (let* ((result (yeetube-scraper-parse))
             (item (car (plist-get result :items)))
             (identity (plist-get result :channel-identity)))
        (should (equal "chv1" (plist-get item :id)))
        (should (equal "PageChan" (plist-get item :channel)))
        (should (equal "/@pagechan" (plist-get item :channel-id)))
        (should (equal "UCpage" (plist-get item :browse-id)))
        (should (equal "UCpage" (plist-get identity :browse-id)))))))

(provide 'yeetube-scraper-tests)
;;; yeetube-scraper-tests.el ends here
