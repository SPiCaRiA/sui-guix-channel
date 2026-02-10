;;; linux.scm --- The Sui Guix Channel  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from sui-linux.org.
;;; Do not modify manually.

(define-module (sui packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((gnu packages linux)
                #:select (alsa-ucm-conf
                          alsa-topology-conf
                          alsa-lib
                          pipewire
                          wireplumber)))

;;; Commentary:
;;;
;;; Linux packages (ALSA stack, PipeWire, WirePlumber).
;;;
;;; Code:

(define-public alsa-ucm-conf-1.2.15
  (package/inherit alsa-ucm-conf
    (version "1.2.15.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.alsa-project.org/files/pub/lib/"
             "alsa-ucm-conf-" version ".tar.bz2"))
       (sha256
        (base32 "0ccab0pvkbzszkhhf9nvhas52jhsvbyc8xfx8vx6rj4gq09yhycz"))))))

(define-public alsa-topology-conf-1.2.5
  (package/inherit alsa-topology-conf
    (version "1.2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.alsa-project.org/files/pub/lib/"
             "alsa-topology-conf-" version ".tar.bz2"))
       (sha256
        (base32 "0ydifvbhlpkgq3qs12qqxami23il5kkz95xw4hwdgg2sakhvmigp"))))))

(define-public alsa-lib-1.2.15
  (package/inherit alsa-lib
    (version "1.2.15.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.alsa-project.org/files/pub/lib/"
             "alsa-lib-" version ".tar.bz2"))
       (sha256
        (base32 "07l6prq8j35cqix7by3xldvhh797cm73dcldmgkssb2q9mhrs1vv"))))
    (inputs
     (modify-inputs (package-inputs alsa-lib)
       (replace "alsa-ucm-conf" alsa-ucm-conf-1.2.15)
       (replace "alsa-topology-conf" alsa-topology-conf-1.2.5)))))

(define-public pipewire-1.5.85
  (package
    (inherit pipewire)
    (inputs
     (modify-inputs (package-inputs pipewire)
       (replace "alsa-lib" alsa-lib-1.2.15)))))

(define-public wireplumber-0.5.13
  (package
    (inherit wireplumber)
    (version "0.5.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.freedesktop.org/pipewire/wireplumber.git")
              (commit version)))
       (file-name (git-file-name "wireplumber" version))
       (sha256
        (base32 "0jk9a7i2c9sfh2i5jwa0pvngimndfl6s8b4g0cdblik69m7lq2l9"))))
    (inputs
     (modify-inputs (package-inputs wireplumber)
       (replace "pipewire" pipewire-1.5.85)))))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; linux.scm ends here.
