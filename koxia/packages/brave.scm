;;; brave.scm --- The Koxia Guix Channel  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from koxia-brave.org.
;;; Do not modify manually.

(define-module (koxia packages brave)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (nonguix build-system chromium-binary))

;;; Commentary:
;;;
;;; Brave.
;;;
;;; Code:

(define-public brave-browser
  (package
    (name "brave")
    (version "1.87.190")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/brave/brave-browser/releases/"
                           "download/v" version "/brave-browser-" version
                           "-linux-amd64.zip"))
       (sha256
        (base32 "08y76fwigpzrnld923zqzbn25kr3zqjqax9whivab5y45qhzq2sz"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f ; nss libraries are wrapped in LD_LIBRARY_PATH
      #:substitutable? #f
      #:install-plan
      #~'(("." "share/brave-browser"))
      #:wrapper-plan
      #~'("chrome_crashpad_handler" "brave")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install-wrapper 'install-entrypoint
            (lambda _
              (let* ([bin (string-append #$output "/bin")])
                (mkdir-p bin)
                (symlink (string-append #$output "/share/brave-browser/brave")
                         (string-append bin "/brave")))))
          (add-after 'install 'create-desktop-file
            (lambda _
              (make-desktop-entry-file
               (string-append #$output
                              "/share/applications/brave-browser.desktop")
               #:name "Brave"
               #:generic-name "Web Browser"
               #:startup-notify #t
               #:startup-w-m-class "brave-browser"
               #:exec (string-append #$output
                                     "/bin/brave --ozone-platform-hint=auto")
               #:icon "brave-desktop"
               #:keywords '("brave")
               #:comment
               '(("en" "Brave")
                 (#f "Brave"))))))))
    (synopsis "Brave browser")
    (supported-systems '("x86_64-linux"))
    (description "The new browser that blocks ads and trackers and protects your
privacy.")
    (home-page "https://brave.com/")
    (license license:mpl2.0)))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; brave.scm ends here.
