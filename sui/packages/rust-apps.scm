;;; rust-apps.scm --- The Sui Guix Channel  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from sui-rust-apps.org.
;;; Do not modify manually.

(define-module (sui packages rust-apps)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (sui packages rust-crates))

;;; Commentary:
;;;
;;; Rust applications.
;;;
;;; Code:

(define-public tex-fmt
  (package
    (name "tex-fmt")
    (version "0.5.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/WGUNDERWOOD/tex-fmt")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "110557jjf4v5bfc2hy38jls5594rj326030v8i6ay581rp5phl65"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-extras
            (lambda _
              (let ([man1 (string-append #$output "/share/man/man1")]
                    [bash (string-append #$output "/share/bash-completion/completions")]
                    [fish (string-append #$output "/share/fish/vendor_completions.d")]
                    [zsh  (string-append #$output "/share/zsh/site-functions")])
                (install-file "man/tex-fmt.1" man1)
                (install-file "completion/tex-fmt.bash" bash)
                (install-file "completion/tex-fmt.fish" fish)
                (install-file "completion/_tex-fmt" zsh)))))))
    (inputs (cargo-inputs 'tex-fmt
                          #:module '(sui packages rust-crates)))
    (home-page "https://github.com/WGUNDERWOOD/tex-fmt")
    (synopsis "LaTeX formatter written in Rust")
    (description "An extremely fast LaTeX formatter written in Rust.  Provides
formatting for @file{.tex}, @file{.bib}, @file{.cls}, and @file{.sty} files.")
    (license license:expat)))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; rust-apps.scm ends here.
