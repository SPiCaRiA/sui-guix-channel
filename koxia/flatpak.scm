;;; flatpak.scm --- Flatpak package records and utilities  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from koxia-flatpak.org.
;;; Do not modify manually.

(define-module (koxia flatpak)
  #:use-module (gnu services configuration)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:export (flatpak-remote
            flatpak-remote?
            flatpak-remote-name
            flatpak-remote-url
            flatpak-remote-gpg-import

            flatpak-overrides
            flatpak-overrides?
            flatpak-overrides-filesystems
            flatpak-overrides-environment
            flatpak-overrides-extra

            flatpak-package
            flatpak-package?
            flatpak-package-id
            flatpak-package-remote
            flatpak-package-commit
            flatpak-package-overrides
            flatpak-package/override

            list-of-flatpak-remotes?
            list-of-flatpak-packages?

            flatpak-overrides->keyfile))

;;; Commentary:
;;;
;;; Record types and serialization utilities for declarative Flatpak
;;; package management.
;;;
;;; Code:

(define-maybe string (no-serialization))

(define-configuration/no-serialization flatpak-remote
  (name
   (symbol 'flathub)
   "Remote identifier symbol, e.g.@: @code{'flathub}.")
  (url
   (string "https://dl.flathub.org/repo/flathub.flatpakrepo")
   "Flatpak repository URL or path to a @file{.flatpakrepo} file.")
  (gpg-import
   (maybe-string)
   "Optional path to a GPG key file for signature verification."))

(define-configuration/no-serialization flatpak-overrides
  (filesystems
   (list '())
   "List of filesystem path strings to grant access to.
Accepts the same values as @command{flatpak-metadata(5)}: @code{home},
@code{host}, @code{xdg-*} names, absolute paths, @code{~/relative} paths,
with optional @code{:ro} or @code{:create} suffix.")
  (environment
   (list '())
   "Association list of @code{(\"KEY\" . \"VALUE\")} string pairs written to
the @code{[Environment]} section of the override keyfile.")
  (extra
   (list '())
   "Association list of @code{(\"SECTION\" . ((\"KEY\" . VALUE) @dots{}))}
for additional override keyfile sections.  VALUE is either a string
(written verbatim as @code{key=value}) or a list of strings (joined with
semicolons as @code{key=val1;val2;}).  Entries are merged with built-in
sections (@code{filesystems} merges into @samp{Context}, @code{environment}
merges into @samp{Environment}).

@lisp
(extra '((\"Context\" . ((\"sockets\" . (\"wayland\" \"!x11\"))
                         (\"devices\" . (\"dri\"))))
         (\"Session Bus Policy\"
          . ((\"org.freedesktop.Notifications\" . \"talk\")))))
@end lisp"))

(define-configuration/no-serialization flatpak-package
  (id
   (string)
   "Flatpak application ID, e.g.@: @code{\"org.mozilla.firefox\"}.")
  (remote
   (symbol 'flathub)
   "Remote name to install from.")
  (commit
   (maybe-string)
   "Optional OSTree commit hash to pin.  Pinned packages are excluded from
updates.")
  (overrides
   (flatpak-overrides (flatpak-overrides))
   "Sandbox permission overrides for this application."))

(define list-of-flatpak-remotes?
  (list-of flatpak-remote?))

(define list-of-flatpak-packages?
  (list-of flatpak-package?))

(define-syntax-rule (flatpak-package/override base field ...)
  "Like (flatpak-package (inherit BASE) ...), but FIELD ... are applied
to the package's overrides rather than the package itself."
  (let ([b base])
    (flatpak-package
     (inherit b)
     (overrides
      (flatpak-overrides
       (inherit (flatpak-package-overrides b))
       field ...)))))

(define (alist-set alist key value)
  (alist-cons key value (alist-delete key alist)))

(define (merge-key-into-section entries key value)
  "Merge VALUE into KEY of ENTRIES.
If the field is a list, append the values.  Otherwise, rewrite the field
value with the new one."
  (let ([existing (assoc key entries)])
    (alist-set entries key
               (if (and existing (pair? (cdr existing)) (pair? value))
                   (append (cdr existing) value)
                   value))))

(define (merge-section sections name entries)
  "Merge ENTRIES into NAME of SECTIONS.
Each field under the section is merged with `merge-key-into-section'."
  (let ([existing (assoc name sections)])
    (alist-set sections name
               (if existing
                   (fold (lambda (entry sec)
                           (merge-key-into-section sec (car entry) (cdr entry)))
                         (cdr existing)
                         entries)
                   entries))))

(define (serialize-keyfile-value value)
  "Serialize VALUE into the override file format."
  (if (pair? value) (format #f "~{~a;~}" value) value))

(define (serialize-sections sections)
  "Serialize SECTIONS into the override file."
  (format #f "~{~a~^~%~%~}"
          (filter-map
           (match-lambda
             ((section . entries)
              (and (not (null? entries))
                   (format #f "[~a]~%~{~a~^~%~}"
                           section
                           (map (match-lambda
                                  ((key . value)
                                   (format #f "~a=~a" key
                                           (serialize-keyfile-value value))))
                                entries)))))
           sections)))

(define (flatpak-overrides->keyfile overrides)
  (let* ([fs    (flatpak-overrides-filesystems overrides)]
         [env   (flatpak-overrides-environment overrides)]
         [extra (flatpak-overrides-extra overrides)]
         [sections extra]
         [sections
          (if (null? fs)
              sections
              (merge-section sections "Context"
                             (list (cons "filesystems" fs))))]
         [sections
          (if (null? env)
              sections
              (merge-section sections "Environment" env))])
    (serialize-sections sections)))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; flatpak.scm ends here.
