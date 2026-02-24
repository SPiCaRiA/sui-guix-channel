;;; authentication.scm --- The Koxia Guix Channel  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from koxia-authentication.org.
;;; Do not modify manually.

(define-module (koxia services authentication)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu system accounts)
  #:use-module (gnu system pam)
  #:use-module (gnu system privilege)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-26)
  #:use-module (koxia packages password-utils)
  #:export (1password-configuration
            1password-service-type
            home-1password-service-type
            gdm-fingerprint-service-type
            fprintd-pam-configuration
            fprintd-pam-service-type))

;;; Commentary:
;;;
;;; Authentication services (1Password, fprintd).
;;;
;;; Code:

(define-configuration/no-serialization 1password-configuration
  (1password
   (file-like 1password)
   "1Password package to use.")
  (1password-cli
   (file-like 1password-cli)
   "1Password CLI package to use.")
  (owners
   list-of-strings
   "List of usernames to set as polkit policy owners.  These users will be
authorized for 1Password CLI and SSH agent access via polkit."))

(define 1password-groups
  (list
   (user-group (name "onepassword"))
   (user-group (name "onepassword-cli"))))

;; Setgid wrappers for the CLI and browser integration binaries.
;; These give no extra permissions; they only harden against environmental
;; tampering (matching the official after-install.sh behavior).
(define 1password-privileged-programs
  (match-record-lambda <1password-configuration>
      (1password 1password-cli)
    (list
     (privileged-program
       (program (file-append 1password-cli "/bin/op"))
       (setgid? #t)
       (group "onepassword-cli"))
     (privileged-program
       (program (file-append 1password
                             "/share/1Password/1Password-BrowserSupport"))
       (setgid? #t)
       (group "onepassword")))))

(define 1password-polkit-policies
  (match-record-lambda <1password-configuration>
      (1password owners)
    (let ([template
           (file-append 1password
                        "/share/1Password/com.1password.1Password.policy.tpl")]
          [owners-string
           (string-join (map (cut string-append "unix-user:" <>) owners) " ")])
      (list
       (computed-file
        "1password-polkit"
        (with-imported-modules '((guix build utils))
          #~(begin
              (use-modules (guix build utils))
              (let ([dst (string-append #$output "/share/polkit-1/actions/"
                                        "com.1password.1Password.policy")])
                (mkdir-p (dirname dst))
                (copy-file #$template dst)
                (substitute* dst
                  (("\\$\\{POLICY_OWNERS\\}") #$owners-string))))))))))

(define 1password-packages
  (match-record-lambda <1password-configuration>
      (1password 1password-cli)
    (list 1password 1password-cli)))

;; 1Password checks that /run/dbus is owned by root:root (standard distro
;; behavior).  Guix's dbus service creates it owned by messagebus.  A one-shot
;; shepherd service fixes the ownership after dbus starts.
(define 1password-dbus-fix
  (list (shepherd-service
          (documentation "Fix /run/dbus ownership for 1Password.")
          (provision '(1password-dbus-fix))
          (requirement '(dbus-system))
          (one-shot? #t)
          (start #~(lambda ()
                     (chown "/run/dbus" 0 0))))))

(define 1password-service-type
  (service-type
    (name '1password)
    (extensions
     (list (service-extension account-service-type
                              (const 1password-groups))
           (service-extension privileged-program-service-type
                              1password-privileged-programs)
           (service-extension polkit-service-type
                              1password-polkit-policies)
           (service-extension profile-service-type
                              1password-packages)
           (service-extension shepherd-root-service-type
                              (const 1password-dbus-fix))))
    (description "Set user groups, setuid programs, and polkit policies for
1Password.")))

(define home-1password-service-type
  (service-type
    (name 'home-1password)
    (extensions
     (list (service-extension
            home-shepherd-service-type
            (const
             (list (shepherd-service
                     (documentation "Start 1Password.")
                     (provision '(1password))
                     (start #~(make-forkexec-constructor
                               (list #$(file-append 1password
                                                    "/bin/1password"))))
                     (stop #~(make-kill-destructor))))))))
    (default-value #f)
    (description "Home service for running 1Password.")))

(define (gdm-fingerprint-pam-service fprintd-package)
  (list (pam-service
          (inherit (unix-pam-service "gdm-fingerprint"
                                     #:login-uid? #t))
          (auth (list (pam-entry
                        (control "required")
                        (module (file-append fprintd-package
                                             "/lib/security/pam_fprintd.so"))))))))

(define gdm-fingerprint-service-type
  (service-type
    (name 'gdm-fingerprint)
    (extensions
     (list (service-extension pam-root-service-type
                              gdm-fingerprint-pam-service)))
    (default-value fprintd)
    (description "Add a @code{gdm-fingerprint} PAM service for GDM fingerprint
login.  Requires @code{fprintd-service-type} to be present.")))

(define-configuration/no-serialization fprintd-pam-configuration
  (fprintd
   (file-like fprintd)
   "The fprintd package to use.")
  (pam-services
   (list-of-strings '())
   "PAM service names to prepend fingerprint authentication to."))

(define fprintd-pam-extension
  (match-record-lambda <fprintd-pam-configuration>
      (fprintd pam-services)
    (if (null? pam-services)
        '()
        (list (pam-extension
                (transformer
                 (lambda (pam)
                   (if (member (pam-service-name pam) pam-services)
                       (pam-service
                         (inherit pam)
                         (auth (cons (pam-entry
                                       (control "sufficient")
                                       (module (file-append
                                                fprintd
                                                "/lib/security/pam_fprintd.so")))
                                     (pam-service-auth pam))))
                       pam))))))))

(define fprintd-pam-service-type
  (service-type
    (name 'fprintd-pam)
    (extensions
     (list (service-extension pam-root-service-type
                              fprintd-pam-extension)))
    (default-value (fprintd-pam-configuration))
    (description "Prepend fprintd fingerprint authentication to the specified
PAM services.  Requires @code{fprintd-service-type} to be present.")))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; authentication.scm ends here.
