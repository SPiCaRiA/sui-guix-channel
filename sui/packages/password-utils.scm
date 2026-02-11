;;; password-utils.scm --- The Sui Guix Channel  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from sui-password-utils.org.
;;; Do not modify manually.

(define-module (sui packages password-utils)
  #:use-module (guix build-system copy)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module ((nonguix licenses) #:prefix licensenon:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nongnu packages mozilla)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages mate)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages nss))

;;; Commentary:
;;;
;;; Password utilities.
;;;
;;; Code:

(define-public 1password-cli
  (package
    (name "1password-cli")
    (version "2.30.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cache.agilebits.com/dist/1P/op2/pkg/v"
                           version "/op_linux_amd64_v" version ".zip"))
       (sha256
        (base32 "0a7fsmbmwa2x5kg1cijggj4w1hw0qgqg8vvs3l4zsh6brvmhfqx1"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (arguments
     (list
      #:install-plan #~'(("op" "bin/op"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-completions
            (lambda _
              ;; op tries to create ~/.config/op/ even for completion output.
              (setenv "HOME" "/tmp")
              (let ([op (string-append #$output "/bin/op")]
                    [bash-comp (string-append
                                #$output "/share/bash-completion/completions")]
                    [zsh-comp (string-append
                               #$output "/share/zsh/site-functions")]
                    [fish-comp (string-append
                                #$output "/share/fish/vendor_completions.d")])
                (mkdir-p bash-comp)
                (mkdir-p zsh-comp)
                (mkdir-p fish-comp)
                (with-output-to-file (string-append bash-comp "/op")
                  (lambda () (invoke op "completion" "bash")))
                (with-output-to-file (string-append zsh-comp "/_op")
                  (lambda () (invoke op "completion" "zsh")))
                (with-output-to-file (string-append fish-comp "/op.fish")
                  (lambda () (invoke op "completion" "fish")))))))))
    (home-page "https://1password.com/downloads/command-line")
    (synopsis "1Password CLI")
    (description "1Password CLI brings 1Password to your terminal.  It can
integrate with your 1Password app and sign in with Touch ID, Windows Hello, or
another system authentication option.

Add this to your @code{operating-system}'s @code{privileged-programs} field to
enable 1Password desktop app integration:

@example
(privileged-programs
 (cons*
  (privileged-program
   (program (file-append 1password-cli \"/bin/op\"))
   (setgid? #t)
   (user \"your-user-name\")
   (group \"onepassword-cli\"))
  %default-privileged-programs))
@end example")
    (license
     (licensenon:nonfree "https://1password.com/legal/terms-of-service"))))

(define prebuilt-libffmpeg
  (package
    (name "prebuilt-libffmpeg")
    (version "0.98.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/nwjs-ffmpeg-prebuilt"
                           "/nwjs-ffmpeg-prebuilt/releases/download/"
                           version "/" version "-linux-x64.zip"))
       (sha256
        (base32 "1nqdpnpbvp5fzyqlj6dwfgf2hprmhkd499pjn4npl75rs8lmj9cg"))))
    (build-system binary-build-system)
    (arguments
     (list
      ;; #:phases
      ;; #~(modify-phases %standard-phases
      ;;     (add-before 'patchelf 'patchelf-writable
      ;;       (lambda _
      ;;         (make-file-writable "libffmpeg.so"))))
      #:patchelf-plan #~'(("libffmpeg.so" ("glibc" "gcc-toolchain")))
      #:install-plan #~'(("libffmpeg.so" "lib/"))))
    (inputs (list glibc gcc-toolchain))
    (native-inputs (list unzip))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/nwjs-ffmpeg-prebuilt/nwjs-ffmpeg-prebuilt")
    (synopsis "FFmpeg prebuilt for NW.js")
    (description "FFmpeg prebuilt binaries with proprietary codecs and build
instructions for Window, Linux and macOS.")
    (license license:gpl2)))

(define-public 1password
  (package
    (name "1password")
    (version "8.10.76")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.1password.com/linux/tar/stable"
                           "/x86_64/1password-" version ".x64.tar.gz"))
       (sha256
        (base32 "0d57bnfrb7qqppvmx9cc2mmay5mp980s6zv5yzd42hqnzmjfcimw"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:wrapper-plan
      #~'(("1password" ("prebuilt-libffmpeg"
                        ("nss" "/lib/nss")))
          "1Password-BrowserSupport"
          "1Password-Crash-Handler"
          "1Password-LastPass-Exporter"
          "chrome-sandbox"
          "op-ssh-sign")
      #:install-plan
      #~'(("./" "share/1Password/")
          ("resources/1password.desktop" "share/applications/")
          ("resources/icons/" "share/icons/")
          ("resources/custom_allowed_browsers" "share/doc/1password/examples/"))
      #:phases
      #~(modify-phases %standard-phases
          ;; (add-before 'patchelf 'patchelf-writable
          ;;   (lambda _
          ;;     (for-each make-file-writable
          ;;               '("1password"
          ;;                 "1Password-BrowserSupport"
          ;;                 "1Password-Crash-Handler"
          ;;                 "1Password-LastPass-Exporter"
          ;;                 "chrome-sandbox"
          ;;                 "op-ssh-sign"))))

          ;; All the following phases are based on the official after-install.sh
          ;; script in the tarball.
          (add-after 'unpack 'set-sandbox-permissions
            (lambda _
              (chmod "chrome-sandbox" #o4755)))
          (add-after 'install 'fix-desktop-file
            (lambda _
              (substitute*
                  (string-append
                   #$output "/share/applications/1password.desktop")
                (("Exec=/opt/1Password/1password")
                 (string-append "Exec=" #$output "/bin/1password")))))
          (add-after 'install 'create-symlinks
            (lambda _
              (let ([src-dir (string-append #$output "/share/1Password")]
                    [target-dir (string-append #$output "/bin")])
                (mkdir-p target-dir)
                (symlink (string-append src-dir "/1password")
                         (string-append target-dir "/1password"))))))))
    (inputs (list prebuilt-libffmpeg))
    (propagated-inputs (list polkit mate-polkit))
    (supported-systems '("x86_64-linux"))
    (home-page "https://support.1password.com/install-linux")
    (synopsis "1Password Desktop")
    (description
     "1Password is a multi-platform password manager.

Add this to your @code{operating-system}'s @code{privileged-programs} field:

@example
(privileged-programs
 (cons*
  (privileged-program
   (program (file-append 1password \"/share/1Password/1Password-BrowserSupport\"))
   (setgid? #t)
   (group \"onepassword\"))
  %default-privileged-programs))
@end example

Use @code{1password-service-type} from @code{(sui services authentication)} to
set up groups, setgid wrappers, and the Polkit policy.")
    (license
     (licensenon:nonfree "https://1password.com/legal/terms-of-service"))))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; password-utils.scm ends here.
