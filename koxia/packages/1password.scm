(define-module (koxia packages 1password)
  #:use-module (guix build-system copy)
  #:use-module (nonguix build-system binary)
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

(define-public prebuilt-libffmpeg
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
    `(#:phases
      (modify-phases %standard-phases
                     (add-before 'patchelf 'patchelf-writable
                                 (lambda _
                                   (make-file-writable "libffmpeg.so"))))
      #:patchelf-plan `(("libffmpeg.so" ("glibc" "gcc-toolchain")))
      #:install-plan `(("libffmpeg.so", "lib/"))))
   (inputs (list glibc
                 gcc-toolchain))
   (native-inputs (list unzip))
   (supported-systems '("x86_64-linux"))
   (home-page "https://github.com/nwjs-ffmpeg-prebuilt/nwjs-ffmpeg-prebuilt")
   (synopsis "FFmpeg prebuilt for NW.js")
   (description "FFmpeg prebuilt binaries with proprietary codecs and build instructions for Window, Linux and macOS.")
   (license license:gpl2)))


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
    `(#:install-plan '(("op" "bin/op"))))
   (home-page "https://1password.com/downloads/command-line")
   (synopsis "1Password CLI")
   (description "1Password CLI brings 1Password to your terminal.  It can integrate with your 1Password app and sign in with Touch ID, Windows Hello, or another system authentication option.

Add this to your @code{operating-system}'s @code{privileged-programs} field to enable 1Password desktop app integration:

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
   (license (licensenon:nonfree "https://1password.com/legal/terms-of-service"))))

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
   (build-system binary-build-system)
   (arguments
    `(#:strip-directories
      '()
      #:validate-runpath? #f
      #:phases
      (modify-phases %standard-phases
                     (add-before 'patchelf 'patchelf-writable
                                 (lambda _
                                   (for-each make-file-writable
                                             '("1password"
                                               "1Password-BrowserSupport"
                                               "1Password-Crash-Handler"
                                               "1Password-LastPass-Exporter"
                                               "chrome-sandbox"
                                               "op-ssh-sign"))))
                     ;; All the following phases are based on the official after-install.sh
                     ;; script in the tarball.
                     (add-after 'unpack 'prepare-policy-file
                                (lambda _
                                  ;; Replace POLICY_OWNERS template with a fixed group reference
                                  (substitute* "com.1password.1Password.policy.tpl"
                                               (("[$]{1}[{]{1}POLICY_OWNERS[}]{1}") "unix-group:onepassword"))
                                  (copy-file "com.1password.1Password.policy.tpl"
                                             "com.1password.1Password.policy")
                                  #t))
                     (add-after 'unpack 'set-sandbox-permissions
                                (lambda _
                                  (chmod "chrome-sandbox" #o4755)))
                     (add-after 'install 'fix-desktop-file
                                (lambda* (#:key outputs #:allow-other-keys)
                                  (let* ((out (assoc-ref outputs "out"))
                                         (desktop-file (string-append out "/share/applications/1password.desktop")))
                                    (substitute* desktop-file
                                                 (("Exec=/opt/1Password/1password")
                                                  (string-append "Exec=" out "/bin/1password"))))))
                     (add-after 'install 'create-symlinks
                                (lambda* (#:key outputs #:allow-other-keys)
                                  (let* ((out (assoc-ref outputs "out"))
                                         (src-dir (string-append out "/share/1Password"))
                                         (target-dir (string-append out "/bin")))
                                    (mkdir-p target-dir)

                                    ;; Create symlink for the main executable.
                                    (symlink
                                     (string-append src-dir "/1password")
                                     (string-append target-dir "/1password"))))))
      #:patchelf-plan
      `(("1password" ("alsa-lib"
                      "at-spi2-core"
                      "cairo"
                      "cups"
                      "dbus"
                      "eudev"
                      "expat"
                      "gcc-toolchain"
                      "glib"
                      "glibc"
                      "gtk+"
                      "libx11"
                      "libxcb"
                      "libxcomposite"
                      "libxdamage"
                      "libxext"
                      "libxfixes"
                      "libxkbcommon"
                      "libxrandr"
                      "mesa"
                      "nspr"
                      "pango"
                      "prebuilt-libffmpeg"
                      ("nss" "/lib/nss")))
        ("1Password-BrowserSupport" ("gcc-toolchain" "eudev"))
        ("1Password-Crash-Handler" ("gcc-toolchain" "eudev"))
        ("1Password-LastPass-Exporter" ("gcc-toolchain"))
        ("chrome-sandbox" ("gcc-toolchain"))
        ("op-ssh-sign" ("gcc-toolchain")))
      #:install-plan
      `(;; The official after-install.sh script installs everything to
        ;; /opt/1Password.  Here we do something similar, but instead use
        ;; share/1Password.
        ("./" "share/1Password/")

        ;; Add .desktop file and icons.
        ("resources/1password.desktop" "share/applications/")
        ("resources/icons/" "share/icons/")

        ;; Install policy kit file for system authentication.
        ("com.1password.1Password.policy" "share/polkit-1/actions/")
        ;; Install examples.
        ("resources/custom_allowed_browsers" "share/doc/1password/examples/"))))
   (inputs
    (list alsa-lib
          at-spi2-core
          cairo
          cups
          dbus
          eudev
          expat
          gcc-toolchain
          glib
          glibc
          gtk+
          libx11
          libxcb
          libxcomposite
          libxdamage
          libxext
          libxfixes
          libxkbcommon
          libxrandr
          mesa
          nspr
          nss
          pango
          prebuilt-libffmpeg))
   (propagated-inputs (list polkit
                            mate-polkit))
   (supported-systems '("x86_64-linux"))
   (home-page "https://support.1password.com/install-linux")
   (synopsis "1Password Desktop")
   (description "1Password is a multi-platform password manager.

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

And a Polkit service with the action file at @example{share/polkit-1/actions/com.1password.1Password.policy}.")
   (license (licensenon:nonfree "https://1password.com/legal/terms-of-service"))))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; 1password.scm ends here.
