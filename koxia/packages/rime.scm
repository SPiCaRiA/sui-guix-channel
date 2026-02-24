;;; rime.scm --- The Koxia Guix Channel  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from koxia-rime.org.
;;; Do not modify manually.

(define-module (koxia packages rime)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages fcitx5)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages lua))

;;; Commentary:
;;;
;;; Rime input method packages: Wanxiang pinyin schema, LMDG grammar model,
;;; librime plugins (Lua, octagram), and an fcitx5-rime override that bundles
;;; everything together.
;;;
;;; Code:

(define-public rime-wanxiang
  (package
    (name "rime-wanxiang")
    (version "14.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/amzxyz/rime_wanxiang")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0najw7k3n203m8a8si5adf06m800innqlbn0cj1jvala26f2xbk7"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'prepare
            (lambda _
              (for-each delete-file-recursively '("custom"))
              (for-each delete-file
                        (filter file-exists?
                                '("README.md" "LICENSE" "CHANGELOG.md"
                                  "release-please-config.json" "version.txt"
                                  "custom_phrase.txt" "weasel.yaml")))
              (rename-file "default.yaml"
                           "wanxiang_suggested_default.yaml"))))
      #:install-plan #~'(("." "share/rime-data"))))
    (home-page "https://github.com/amzxyz/rime_wanxiang")
    (synopsis "Feature-rich pinyin schema for Rime")
    (description "Wanxiang Pinyin is a comprehensive Rime schema with Chinese-
English mixed input, emoji support, Lua-driven intelligent filtering, and
extensive dictionaries.

This package is built from the upstream repository snapshots, and includes all
the auxiliary encodings.

The upstream default.yaml is included as wanxiang_suggested_default.yaml.  To
enable it, please modify your default.custom.yaml as such:

@example
patch:
  __include: wanxiang_suggested_default:/
@end example")
    (license license:cc-by4.0)))

(define-public rime-lmdg
  (package
    (name "rime-lmdg")
    (version "0-unstable-2026-02-16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jetcookies/RIME-LMDG-tracker"
             "/releases/download/" version
             "/wanxiang-lts-zh-hans.gram"))
       (sha256
        (base32 "153zlmfp416f9bl99szqs91ypwsz6z0139l543n3blibj8fhf6yx"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((dest (string-append %output "/share/rime-data")))
           (mkdir-p dest)
           (copy-file (assoc-ref %build-inputs "source")
                      (string-append dest "/wanxiang-lts-zh-hans.gram"))))))
    (home-page "https://github.com/amzxyz/RIME-LMDG")
    (synopsis "Wanxiang grammar model for Rime")
    (description "Language model for grammar-aware candidate ranking
in Wanxiang Pinyin.  Requires librime with the octagram plugin.")
    (license license:cc-by4.0)))

(define-public librime-octagram
  (hidden-package
   (package
     (name "librime-octagram")
     (version "0-unstable-2024-11-18")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lotem/librime-octagram")
               (commit "dfcc15115788c828d9dd7b4bff68067d3ce2ffb8")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15vcg6m4pq99y895f36afyiwm9pjfpfxrr3zf62bkwqmblgjq1bn"))))
     (build-system copy-build-system)
     (arguments
      (list #:install-plan #~'(("." "."))))
     (home-page "https://github.com/lotem/librime-octagram")
     (synopsis "RIME essay grammar plugin")
     (description "Source files for the librime octagram plugin.
Built as part of librime, not standalone.")
     (license license:gpl3))))

(define-public librime-lua
  (hidden-package
   (package
     (name "librime-lua")
     (version "0-unstable-2025-07-07")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hchunhui/librime-lua")
               (commit "68f9c364a2d25a04c7d4794981d7c796b05ab627")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1759pk44bslsfv890gmkrg8r3qrnmimidp1hf3nc2c15s9fymgwv"))))
     (build-system copy-build-system)
     (arguments
      (list #:install-plan
            #~'(("src" "src")
                ("CMakeLists.txt" "CMakeLists.txt")
                ("LICENSE" "LICENSE")
                ("README.md" "README.md"))))
     (propagated-inputs (list lua))
     (home-page "https://github.com/hchunhui/librime-lua")
     (synopsis "Extending RIME with Lua scripts")
     (description "Source files for the librime Lua plugin.
Built as part of librime, not standalone.")
     (license license:bsd-3))))

(define-public librime-wanxiang
  (package/inherit librime
    (name "librime-wanxiang")
    (version "1.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/rime/librime")
              (commit version)))
       (file-name (git-file-name "librime" version))
       (sha256
        (base32 "1x8sa4y996kdvvkbk1aqa6jb7qhm19ir2n0b4wclsxvzzd53mfi5"))))
    (inputs
     (modify-inputs (package-inputs librime)
       (append librime-lua)
       (append librime-octagram)))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-glog-compat
            (lambda _
              ;; glog < 0.6.0 lacks IsGoogleLoggingInitialized.
              ;; SetupLogging is only called once per process so
              ;; replacing with false (always initialize) is safe.
              (substitute* "src/rime/setup.cc"
                (("google::IsGoogleLoggingInitialized\\(\\)")
                 "false"))))
          (add-before 'configure 'copy-plugins
            (lambda* (#:key inputs #:allow-other-keys)
              (mkdir-p "plugins")
              (for-each
               (lambda (name)
                 (let ((src (assoc-ref inputs name)))
                   (when src
                     (copy-recursively src
                                       (string-append "plugins/" name)))))
               '("librime-lua" "librime-octagram"))))
          (add-before 'configure 'fix-lua-pkg-config
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Guix's lua 5.3 provides lua5.3.pc but librime-lua's
              ;; cmake looks for "lua" via pkg-config.  Create a shim.
              (let* ((lua (assoc-ref inputs "lua"))
                     (pc-dir (string-append lua "/lib/pkgconfig"))
                     (shim (string-append (getcwd) "/pkgconfig-shim"))
                     (pc-files (find-files pc-dir "\\.pc$")))
                (when (pair? pc-files)
                  (mkdir-p shim)
                  (symlink (car pc-files)
                           (string-append shim "/lua.pc"))
                  (setenv "PKG_CONFIG_PATH"
                          (string-append shim ":"
                                         (or (getenv "PKG_CONFIG_PATH")
                                             ""))))))))))))

(define-public fcitx5-rime-wanxiang
  (package/inherit fcitx5-rime
    (name "fcitx5-rime-wanxiang")
    (inputs
     (modify-inputs (package-inputs fcitx5-rime)
       (replace "librime" librime-wanxiang)
       (append rime-wanxiang)
       (append rime-lmdg)))
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:configure-flags
      #~(list (string-append "-DRIME_DATA_DIR=" #$output "/share/rime-data"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'merge-rime-data
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((dest (string-append #$output "/share/rime-data")))
                (mkdir-p dest)
                (for-each
                 (lambda (name)
                   (let ((src (string-append (assoc-ref inputs name)
                                             "/share/rime-data")))
                     (when (file-exists? src)
                       (copy-recursively src dest
                                         #:follow-symlinks? #t
                                         #:copy-file
                                         (lambda (old new)
                                           (unless (file-exists? new)
                                             (copy-file old new)))))))
                 '("rime-data" "rime-wanxiang" "rime-lmdg"))
                ;; Ensure default.yaml exists (may be empty).
                (let ((default (string-append dest "/default.yaml")))
                  (unless (file-exists? default)
                    (close-port (open-output-file default))))))))))))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; rime.scm ends here.
