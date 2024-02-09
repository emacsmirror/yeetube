[![MELPA](https://melpa.org/packages/yeetube-badge.svg)](https://melpa.org/#/yeetube)

# YeeTube

![yeetube showcase](/showcase/example.gif?raw=true "Showcase Yeetube")
## About 

This package provides the ability to scrape YouTube, with the results
displayed in a proced-like buffer.


Package functionality includes:

- Query YouTube
- Play video url *by default using mpv*
- Bookmark/Save video url
- Download video *using yt-dlp*
- A minimal yt-dlp front-end, *which is independent of the rest
  YouTube functionality*.

*This package does not use Invidious or YouTube's API, just "parses"
html & json.*


## Installation 
This package is available via [MELPA](https://melpa.org/#/yeetube)

### Straight.el

``` emacs-lisp
(straight-use-package 
 '(yeetube :type git
	       :host nil
	       :repo "https://git.thanosapollo.org/yeetube"))
```


### Manual
``` shell
$ git clone https://git.thanosapollo.org/yeetube
```

*Add this to your emacs configuration:*

``` emacs-lisp
   (add-to-list 'load-path "/path/to/yeetube")
   (load-file "~/path/to/yeetube.el")
   (require 'yeetube)
```

### Dependencies
- [mpv](https://mpv.io/): default multimedia player
- [yt-dlp](https://github.com/yt-dlp/yt-dlp): download functionality

*Debian/Ubuntu*
``` shell
$ sudo apt install mpv yt-dlp
```

## Configuration 
### Media Player 
By default `yeetube-player` is set to `yeetube-mpv-play`, you can
use [mpv.el](https://github.com/kljohann/mpv.el),
[GNU/Emms](https://www.gnu.org/software/emms/) or other similar
packages like so:

``` emacs-lisp
(setf yeetube-player #'emms-play-url)
```

Make sure that the media player of your choice can directly play
youtube urls.

### Apply Filters

To filter the search results based on a specific criterion, you can
modify the `yeetube-filter` value to your preferred option.

For example:

```emacs-lisp
(setf yeetube-filter "Views")
```

This will filter & sort the search results according to the number of views.

### Torsocks

If you are using `yeetube-mpv-play` as your media player & running
[tor](https://wiki.archlinux.org/title/Tor), you can use torsocks to
route your traffic via the tor network.

``` emacs-lisp
(setf yeetube-mpv-enable-torsocks t)
```

*You can toggle that option with `yeetube-mpv-toggle-torsocks`*

### Video Quality

If you are using `yeetube-mpv-play` as your media player, you can
specify video quality using `yeetube-mpv-change-video-quality`
*recommended* or by using this snippet.

``` emacs-lisp
(setf yeetube-mpv-video-quality "720") ;; Accepted values include: 1080, 720, 480, 360, 240, 144
```

