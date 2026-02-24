;;; flatpak-apps.scm --- Flatpak package definitions  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from koxia-flatpak-apps.org.
;;; Do not modify manually.

(define-module (koxia packages flatpak-apps)
  #:use-module (koxia flatpak))

;;; Commentary:
;;;
;;; Flatpak package definitions for the Koxia channel.
;;;
;;; Code:

(define-public flatpak-discord
  (flatpak-package
   (id "com.discordapp.Discord")))

(define-public flatpak-slack
  (flatpak-package
   (id "com.slack.Slack")))

(define-public flatpak-zulip
  (flatpak-package
   (id "org.zulip.Zulip")))

(define-public flatpak-telegram
  (flatpak-package
   (id "org.telegram.desktop")))

(define-public flatpak-zoom
  (flatpak-package
   (id "us.zoom.Zoom")))

(define-public flatpak-wechat
  (flatpak-package
   (id "com.tencent.WeChat")
   (overrides
    (flatpak-overrides
     (environment '(("QT_IM_MODULE" . "fcitx")))))))

(define-public flatpak-brave-browser
  (flatpak-package
   (id "com.brave.Browser")))

(define-public flatpak-obsidian
  (flatpak-package
   (id "md.obsidian.Obsidian")))

(define-public flatpak-zotero
  (flatpak-package
   (id "org.zotero.Zotero")))

(define-public flatpak-spotify
  (flatpak-package
   (id "com.spotify.Client")))

(define-public flatpak-qqmusic
  (flatpak-package
   (id "com.qq.QQmusic")))

(define-public flatpak-netease-music
  (flatpak-package
   (id "com.netease.CloudMusic")))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; flatpak-apps.scm ends here.
