;;; gnome.scm --- The Koxia Guix Channel  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from koxia-gnome.org.
;;; Do not modify manually.

(define-module (koxia packages gnome)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((gnu packages gnome)
                #:select (gsettings-desktop-schemas
                          xdg-desktop-portal-gnome)))

;;; Commentary:
;;;
;;; GNOME desktop packages (gsettings-desktop-schemas,
;;; xdg-desktop-portal-gnome).
;;;
;;; Code:

(define-public gsettings-desktop-schemas-47
  (package/inherit gsettings-desktop-schemas
    (version "47.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/"
                           "gsettings-desktop-schemas" "/"
                           (version-major version) "/"
                           "gsettings-desktop-schemas"
                           "-" version ".tar.xz"))
       (sha256
        (base32 "0598gb6bzn25m56vhd3a0z35zfj0hfik9lfhsrjb58f0r7ch80m6"))))))

(define-public xdg-desktop-portal-gnome-47
  (package/inherit xdg-desktop-portal-gnome
    (version "47.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/"
                           "xdg-desktop-portal-gnome" "/"
                           (version-major version) "/"
                           "xdg-desktop-portal-gnome"
                           "-" version ".tar.xz"))
       (sha256
        (base32 "0i0y6vrvrfwkic0nx7izm9zr516b4x7pd54lbs8pqdl3nla4nvlz"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (delete-file-recursively "subprojects")))))
    (inputs
     (modify-inputs (package-inputs xdg-desktop-portal-gnome)
       (replace "gsettings-desktop-schemas"
         gsettings-desktop-schemas-47)))))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; gnome.scm ends here.
