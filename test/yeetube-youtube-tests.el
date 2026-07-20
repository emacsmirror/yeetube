;;; yeetube-youtube-tests.el --- Tests for yeetube-youtube  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Run: emacs -Q --batch -L .. -l test/yeetube-youtube-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

;; Workaround: Emacs 31.1 (and Guix grafts of find-func.el) defines a
;; defcustom :set for `find-function-mode-lower-precedence' that
;; references `find-function-mode' before it exists.  Pre-declaring
;; it avoids a void-variable error when ert loads find-func indirectly.
(unless (boundp 'find-function-mode)
  (defvar find-function-mode nil))

(require 'ert)
(require 'cl-lib)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'yeetube-youtube)

;;; Group 1: filter codes

(ert-deftest yeetube-youtube-test-filter-code-relevance ()
  (should (string= "EgIQAQ%253D%253D" (yeetube-youtube--filter-code "Relevance"))))

(ert-deftest yeetube-youtube-test-filter-code-date ()
  (should (string= "CAISAhAB" (yeetube-youtube--filter-code "Date"))))

(ert-deftest yeetube-youtube-test-filter-code-views ()
  (should (string= "CAMSAhAB" (yeetube-youtube--filter-code "Views"))))

(ert-deftest yeetube-youtube-test-filter-code-rating ()
  (should (string= "CAESAhAB" (yeetube-youtube--filter-code "Rating"))))

(ert-deftest yeetube-youtube-test-filter-code-unknown ()
  (should-not (yeetube-youtube--filter-code "Nonexistent")))

;;; Group 2: search request

(ert-deftest yeetube-youtube-test-search-request-appends-filter ()
  (let ((yeetube-youtube-filter "Views"))
    (should (equal "https://youtube.com/search?q=foo%20bar&sp=CAMSAhAB"
                   (plist-get (yeetube-backend-search-request 'youtube "foo bar")
                              :url)))))

(ert-deftest yeetube-youtube-test-search-request-skips-unknown-filter ()
  "An unknown filter must not leak \"&sp=nil\" into the URL."
  (let ((yeetube-youtube-filter "Nonexistent"))
    (should (equal "https://youtube.com/search?q=foo"
                   (plist-get (yeetube-backend-search-request 'youtube "foo")
                              :url)))))

;;; Group 3: channel URLs & requests

(ert-deftest yeetube-youtube-test-channel-url-handle ()
  "Channel URLs are built without duplicate slashes for handles."
  (should (equal "https://youtube.com/@foo/videos"
                 (yeetube-youtube--channel-url "@foo" "videos"))))

(ert-deftest yeetube-youtube-test-channel-url-channel-path ()
  "Channel URLs are built without duplicate slashes for channel paths."
  (should (equal "https://youtube.com/channel/UCsystemcrafters/streams"
                 (yeetube-youtube--channel-url "/channel/UCsystemcrafters"
                                               "streams"))))

(ert-deftest yeetube-youtube-test-channel-request-videos ()
  (should (equal "https://youtube.com/@foo/videos"
                 (plist-get (yeetube-backend-channel-request 'youtube "@foo" 'videos)
                            :url))))

(ert-deftest yeetube-youtube-test-channel-request-streams ()
  (should (equal "https://youtube.com/@foo/streams"
                 (plist-get (yeetube-backend-channel-request 'youtube "@foo" 'streams)
                            :url))))

(ert-deftest yeetube-youtube-test-channel-request-search ()
  (should (equal "https://youtube.com/@foo/search?query=foo%20bar"
                 (plist-get (yeetube-backend-channel-request
                             'youtube "@foo" 'search "foo bar")
                            :url))))

(ert-deftest yeetube-youtube-test-channel-request-unknown-what-errors ()
  (should-error (yeetube-backend-channel-request 'youtube "@foo" 'comments)))

;;; Group 4: continuation request

(ert-deftest yeetube-youtube-test-continuation-request-shape ()
  "Continuation requests POST the token to the InnerTube API path."
  (let ((request (yeetube-backend-continuation-request
                  'youtube '(:token "TOKEN123" :url "/youtubei/v1/search"))))
    (should (equal "https://www.youtube.com/youtubei/v1/search"
                   (plist-get request :url)))
    (should (equal "POST" (plist-get request :method)))
    (should (equal '(("Content-Type" . "application/json"))
                   (plist-get request :headers)))
    (should (string-match-p "TOKEN123" (plist-get request :data)))
    (should (string-match-p "\"clientName\":\"WEB\""
                            (plist-get request :data)))))

(ert-deftest yeetube-youtube-test-continuation-request-rejects-empty-url ()
  "Continuation requests reject absent API paths."
  (should-error
   (yeetube-backend-continuation-request 'youtube '(:token "abc" :url ""))
   :type 'user-error)
  (should-error
   (yeetube-backend-continuation-request 'youtube '(:token "abc" :url nil))
   :type 'user-error))

;;; Group 5: item & browse URLs

(ert-deftest yeetube-youtube-test-item-url-video ()
  (let ((yeetube-youtube-video-url "https://youtube.com/watch?v="))
    (should (equal "https://youtube.com/watch?v=abc"
                   (yeetube-backend-item-url 'youtube "abc" 'video)))))

(ert-deftest yeetube-youtube-test-item-url-playlist ()
  (let ((yeetube-youtube-playlist-url "https://youtube.com/playlist?list="))
    (should (equal "https://youtube.com/playlist?list=PLxyz"
                   (yeetube-backend-item-url 'youtube "PLxyz" 'playlist)))))

(ert-deftest yeetube-youtube-test-browse-url-uses-invidious-instance ()
  (let ((yeetube-youtube-invidious-instances "inv.example.org"))
    (should (equal "https://inv.example.org/watch?v=abc"
                   (yeetube-backend-browse-url 'youtube "abc" 'video)))))

(ert-deftest yeetube-youtube-test-browse-url-errors-without-instances ()
  (let ((yeetube-youtube-invidious-instances nil))
    (should-error (yeetube-backend-browse-url 'youtube "abc" 'video)
                  :type 'user-error)))

;;; Group 6: channel input

(ert-deftest yeetube-youtube-test-read-channel-errors-on-empty-input ()
  "Manual channel input rejects empty strings."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  ")))
    (should-error (yeetube-backend-read-channel 'youtube) :type 'user-error)))

(ert-deftest yeetube-youtube-test-read-channel-adds-handle-prefix ()
  "Manual channel input treats plain names as handles."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "foo")))
    (should (equal "@foo" (yeetube-backend-read-channel 'youtube)))))

(ert-deftest yeetube-youtube-test-read-channel-preserves-prefixed-input ()
  "Manual channel input preserves handles and channel paths."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "@foo")))
    (should (equal "@foo" (yeetube-backend-read-channel 'youtube))))
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _) "/channel/UCsystemcrafters")))
    (should (equal "/channel/UCsystemcrafters"
                   (yeetube-backend-read-channel 'youtube)))))

;;; Group 7: RSS feeds

(ert-deftest yeetube-youtube-test-feed-url ()
  (should (equal "https://www.youtube.com/feeds/videos.xml?channel_id=UCabc"
                 (yeetube-backend-feed-url 'youtube "UCabc"))))

(defconst yeetube-youtube-test-rss-feed
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:yt=\"http://www.youtube.com/xml/schemas/2015\">
  <entry>
    <yt:videoId>abc123</yt:videoId>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>First video</title>
    <published>2026-05-01T12:00:00+00:00</published>
    <updated>2026-05-02T12:00:00+00:00</updated>
    <author>
      <name>System Crafters</name>
      <uri>https://www.youtube.com/channel/UCsystemcrafters</uri>
    </author>
  </entry>
  <entry>
    <yt:videoId>def456</yt:videoId>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>Second video</title>
    <updated>2026-05-03T12:00:00+00:00</updated>
    <author>
      <name>System Crafters</name>
    </author>
  </entry>
</feed>")

(ert-deftest yeetube-youtube-test-rss-parse-buffer-converts-videos ()
  "RSS entries are converted to yeetube item plists."
  (let ((items (with-temp-buffer
                 (insert yeetube-youtube-test-rss-feed)
                 (yeetube-youtube--rss-parse-buffer))))
    (should (= 2 (length items)))
    (let ((item (car items)))
      (should (equal "abc123" (plist-get item :id)))
      (should (equal "First video" (plist-get item :title)))
      (should (equal "System Crafters" (plist-get item :channel)))
      (should (equal "/channel/UCsystemcrafters" (plist-get item :channel-id)))
      (should (equal "UCsystemcrafters" (plist-get item :browse-id)))
      (should (equal "https://i.ytimg.com/vi/abc123/default.jpg"
                     (plist-get item :thumbnail-url)))
      (should (equal "" (plist-get item :views)))
      (should (equal "" (plist-get item :duration)))
      (should (equal "2026-05-01T12:00:00+00:00" (plist-get item :date)))
      (should (eq 'video (plist-get item :type))))
    (should (equal "def456" (plist-get (cadr items) :id)))
    (should (equal "2026-05-03T12:00:00+00:00"
                   (plist-get (cadr items) :date)))
    (should (equal "" (plist-get (cadr items) :views)))
    (should (equal "" (plist-get (cadr items) :duration)))))

(ert-deftest yeetube-youtube-test-rss-parse-buffer-filters-entries-without-video-id ()
  "RSS entries without video IDs are ignored."
  (let ((items (with-temp-buffer
                 (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\" xmlns:yt=\"http://www.youtube.com/xml/schemas/2015\">
  <entry>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>Missing video ID</title>
  </entry>
  <entry>
    <yt:videoId>abc123</yt:videoId>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>Valid video</title>
  </entry>
</feed>")
                 (yeetube-youtube--rss-parse-buffer))))
    (should (= 1 (length items)))
    (should (equal "abc123" (plist-get (car items) :id)))))

(ert-deftest yeetube-youtube-test-rss-entry-views-from-media-statistics ()
  "RSS :views is populated from media:statistics views attribute."
  (let ((items (with-temp-buffer
                 (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\"
      xmlns:yt=\"http://www.youtube.com/xml/schemas/2015\"
      xmlns:media=\"http://search.yahoo.com/mrss/\">
  <entry>
    <yt:videoId>abc123</yt:videoId>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>Stats</title>
    <media:group>
      <media:statistics views=\"4242\"/>
    </media:group>
  </entry>
</feed>")
                 (yeetube-youtube--rss-parse-buffer))))
    (should (equal "4242" (plist-get (car items) :views)))))

(ert-deftest yeetube-youtube-test-rss-entry-thumbnail-from-media-thumbnail ()
  "RSS :thumbnail-url uses media:thumbnail and rewrites hqdefault."
  (let ((items (with-temp-buffer
                 (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\"
      xmlns:yt=\"http://www.youtube.com/xml/schemas/2015\"
      xmlns:media=\"http://search.yahoo.com/mrss/\">
  <entry>
    <yt:videoId>abc123</yt:videoId>
    <yt:channelId>UCsystemcrafters</yt:channelId>
    <title>Thumb</title>
    <media:group>
      <media:thumbnail url=\"https://i.ytimg.com/vi/abc123/hqdefault.jpg\"
                       width=\"480\" height=\"360\"/>
    </media:group>
  </entry>
</feed>")
                 (yeetube-youtube--rss-parse-buffer))))
    (should (equal "https://i.ytimg.com/vi/abc123/mqdefault.jpg"
                   (plist-get (car items) :thumbnail-url)))))

(provide 'yeetube-youtube-tests)
;;; yeetube-youtube-tests.el ends here
