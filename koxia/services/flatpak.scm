;;; flatpak.scm --- Flatpak system and home services  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from koxia-flatpak.org.
;;; Do not modify manually.

(define-module (koxia services flatpak)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module ((gnu packages package-management) #:select (flatpak))
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (koxia flatpak)

  #:export (flatpak-configuration
            flatpak-configuration?
            flatpak-configuration-flatpak
            flatpak-configuration-remotes
            flatpak-configuration-global-overrides
            flatpak-configuration-packages
            flatpak-configuration-uninstall-unmanaged?
            flatpak-configuration-update-on-activation?
            flatpak-configuration-cleanup-unused?
            flatpak-configuration-tmpdir

            flatpak-service-type

            home-flatpak-configuration
            home-flatpak-configuration?
            home-flatpak-configuration-global-overrides
            home-flatpak-configuration-overrides
            home-flatpak-configuration-sandbox-files

            home-flatpak-service-type))

;;; Commentary:
;;;
;;; Declarative Flatpak services for GNU Guix.
;;;
;;; The system service (flatpak-service-type) installs and manages Flatpak
;;; applications system-wide, writes override keyfiles, and tracks state.
;;;
;;; The home service (home-flatpak-service-type) is passive: it deploys
;;; per-user override keyfiles, sandbox files, and sets XDG_DATA_DIRS.
;;;
;;; Code:

(define-maybe string (no-serialization))

(define (flatpak-override-data packages global-overrides)
  "Return an association list of (ID . KEYFILE-STRING) pairs for non-empty
overrides derived from PACKAGES and GLOBAL-OVERRIDES."
  (let* ([pkg-pairs
          (filter-map
           (lambda (pkg)
             (let ([kf (flatpak-overrides->keyfile
                        (flatpak-package-overrides pkg))])
               (and (not (string-null? kf))
                    (cons (flatpak-package-id pkg) kf))))
           packages)]
         [global-kf (flatpak-overrides->keyfile global-overrides)]
         [global-pair (and (not (string-null? global-kf))
                           (cons "global" global-kf))])
    (if global-pair (cons global-pair pkg-pairs) pkg-pairs)))

(define-configuration/no-serialization flatpak-configuration
  (flatpak
   (file-like flatpak)
   "The Flatpak package to use.")
  (remotes
   (list-of-flatpak-remotes (list (flatpak-remote)))
   "List of @code{flatpak-remote} records.")
  (global-overrides
   (flatpak-overrides (flatpak-overrides))
   "System-wide permission overrides applied to all applications.  Written to
@file{/var/lib/flatpak/overrides/global}.")
  (packages
   (list-of-flatpak-packages '())
   "List of @code{flatpak-package} records to install system-wide.")
  (uninstall-unmanaged?
   (boolean #f)
   "When @code{#t}, uninstall previously-managed packages that have been
removed from the declaration.")
  (update-on-activation?
   (boolean #f)
   "When @code{#t}, update unpinned declared packages during activation.")
  (cleanup-unused?
   (boolean #f)
   "When @code{#t}, run @command{flatpak uninstall --unused} to remove
orphaned runtimes after activation.")
  (tmpdir
   (maybe-string)
   "When set, override @env{TMPDIR} for Flatpak operations.  Useful on
tmpfs-root systems where @file{/var/tmp} does not exist."))

(define (flatpak-activation-script config)
  (match-record config <flatpak-configuration>
                (flatpak remotes packages uninstall-unmanaged?
                         update-on-activation? cleanup-unused? tmpdir)
    (let ([remote-data
           (map (lambda (r)
                  (list (symbol->string (flatpak-remote-name r))
                        (flatpak-remote-url r)
                        (and (maybe-value-set? (flatpak-remote-gpg-import r))
                             (flatpak-remote-gpg-import r))))
                remotes)]
          [pkg-data
           (map (lambda (pkg)
                  (list (flatpak-package-id pkg)
                        (symbol->string (flatpak-package-remote pkg))
                        (and (maybe-value-set? (flatpak-package-commit pkg))
                             (flatpak-package-commit pkg))))
                packages)]
          [declared-ids
           (map flatpak-package-id packages)]
          [declared-remote-names
           (map (lambda (r) (symbol->string (flatpak-remote-name r)))
                remotes)]
          [unpinned-ids
           (filter-map
            (lambda (pkg)
              (and (not (maybe-value-set? (flatpak-package-commit pkg)))
                   (flatpak-package-id pkg)))
            packages)]
          [override-pairs
           (flatpak-override-data
            packages (flatpak-configuration-global-overrides config))])
      (program-file
       "flatpak-activation"
       (with-imported-modules (source-module-closure '((guix build utils)))
         #~(begin
             (use-modules (guix build utils)
                          (ice-9 match))

             (define flatpak-bin
               #$(file-append flatpak "/bin/flatpak"))

             (define tmpdir
               '#$(and (maybe-value-set? tmpdir) tmpdir))

             (define state-dir "/var/lib/flatpak-guix")

             (define state-path
               (string-append state-dir "/state.scm"))

             (define prev-state
               (if (file-exists? state-path)
                   (call-with-input-file state-path read)
                   '()))

             (define prev-managed
               (or (assoc-ref prev-state 'managed-packages) '()))

             (define prev-remotes
               (or (assoc-ref prev-state 'managed-remotes) '()))

             (define remote-data '#$remote-data)
             (define pkg-data '#$pkg-data)
             (define declared-ids '#$declared-ids)
             (define declared-remotes '#$declared-remote-names)
             (define unpinned-ids '#$unpinned-ids)

             (define (flatpak* . args)
               (apply system*
                      (if tmpdir
                          (cons* "env" (string-append "TMPDIR=" tmpdir)
                                 flatpak-bin args)
                          (cons* flatpak-bin args))))

             (define (flatpak! . args)
               (let ([st (apply flatpak* args)])
                 (unless (zero? st)
                   (error "flatpak command failed" args st))))

             (catch #t
                    (lambda ()
                      ;; 1. Write override keyfiles.
                      (mkdir-p "/var/lib/flatpak/overrides")
                      (for-each
                       (lambda (ov)
                         (match ov
                           ((id . content)
                            (call-with-output-file
                                (string-append "/var/lib/flatpak/overrides/" id)
                              (lambda (port) (display content port))))))
                       '#$override-pairs)

                      ;; 2. Setup remotes.
                      (for-each
                       (lambda (r)
                         (match r
                           ((name url gpg)
                            (apply flatpak! "remote-add" "--if-not-exists"
                                   (append
                                    (if gpg
                                        (list (string-append "--gpg-import=" gpg))
                                        '())
                                    (list name url))))))
                       remote-data)

                      ;; 3. Remove remotes no longer in config.
                      (for-each
                       (lambda (old-name)
                         (unless (member old-name declared-remotes)
                           (flatpak* "remote-delete" "--force" old-name)))
                       prev-remotes)

                      ;; 4. Install declared packages.
                      (for-each
                       (lambda (pkg)
                         (match pkg
                           ((id remote commit)
                            (apply
                             flatpak! "install"
                             "--or-update" "--noninteractive"
                             (append (if commit
                                         (list (string-append "--commit=" commit))
                                         '())
                                     (list remote id))))))
                       pkg-data)

                      ;; 5. Uninstall removed managed packages.
                      (when #$uninstall-unmanaged?
                        (for-each
                         (lambda (old-id)
                           (unless (member old-id declared-ids)
                             (flatpak* "uninstall" "--noninteractive" old-id)))
                         prev-managed))

                      ;; 6. Update unpinned packages.
                      (when #$update-on-activation?
                        (unless (null? unpinned-ids)
                          (apply flatpak* "update" "--noninteractive"
                                 unpinned-ids)))

                      ;; 7. Cleanup unused runtimes.
                      (when #$cleanup-unused?
                        (flatpak* "uninstall" "--unused" "--noninteractive"))

                      ;; 8. Write state file.
                      (mkdir-p state-dir)
                      (let ([tmp (string-append state-path ".tmp")])
                        (call-with-output-file tmp
                          (lambda (port)
                            (write `((managed-packages . ,declared-ids)
                                     (managed-remotes  . ,declared-remotes))
                                   port)
                            (newline port)))
                        (rename-file tmp state-path)))

                    (lambda (key . args)
                      (format (current-error-port)
                              "warning: flatpak activation failed (~a): ~s~%"
                              key args)))))))))

(define (flatpak-activation config)
  #~(primitive-load #$(flatpak-activation-script config)))

(define flatpak-service-type
  (service-type
    (name 'flatpak)
    (extensions
     (list (service-extension activation-service-type
                              flatpak-activation)
           (service-extension profile-service-type
                              (compose list flatpak-configuration-flatpak))))
    (compose concatenate)
    (extend (lambda (config extra-packages)
              (flatpak-configuration
               (inherit config)
               (packages (append (flatpak-configuration-packages config)
                                 extra-packages)))))
    (default-value (flatpak-configuration))
    (description "Install and manage Flatpak applications system-wide.")))

(define-configuration/no-serialization home-flatpak-configuration
  (global-overrides
   (flatpak-overrides (flatpak-overrides))
   "User-level global permission overrides (highest priority).  Written to
@file{~/.local/share/flatpak/overrides/global}.")
  (overrides
   (list '())
   "Association list of @code{(app-id . flatpak-overrides)} for per-user
per-application overrides.  Written to
@file{~/.local/share/flatpak/overrides/@var{app-id}}.")
  (sandbox-files
   (list '())
   "Association list of @code{(app-id . ((dest file-like) @dots{}))} for
per-application sandbox files.  Each @var{dest} is relative to the app's
sandbox home under @file{~/.var/app/@var{app-id}/}."))

(define (home-flatpak-override-files config)
  (let* ([per-app
          (filter-map
           (match-lambda
             ((id . ov)
              (let ([kf (flatpak-overrides->keyfile ov)])
                (and (not (string-null? kf))
                     (list (string-append "flatpak/overrides/" id)
                           (plain-file id kf))))))
           (home-flatpak-configuration-overrides config))]
         [global-kf (flatpak-overrides->keyfile
                     (home-flatpak-configuration-global-overrides config))]
         [global (and (not (string-null? global-kf))
                      (list "flatpak/overrides/global"
                            (plain-file "global" global-kf)))])
    (if global (cons global per-app) per-app)))

(define (home-flatpak-sandbox-files config)
  (append-map
   (match-lambda
     ((id . files)
      (map (match-lambda
             ((dest file)
              (list (string-append ".var/app/" id "/" dest) file)))
           files)))
   (home-flatpak-configuration-sandbox-files config)))

(define %home-flatpak-environment
  '(("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:/var/lib/flatpak/exports/share")))

(define home-flatpak-service-type
  (service-type
    (name 'home-flatpak)
    (extensions
     (list (service-extension home-xdg-data-files-service-type
                              home-flatpak-override-files)
           (service-extension home-files-service-type
                              home-flatpak-sandbox-files)
           (service-extension home-environment-variables-service-type
                              (const %home-flatpak-environment))))
    (default-value (home-flatpak-configuration))
    (description "Per-user Flatpak overrides, sandbox files, and
@env{XDG_DATA_DIRS} configuration.")))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; flatpak.scm ends here.
