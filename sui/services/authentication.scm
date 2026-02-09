;;; authentication.scm --- The Sui Guix Channel  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from sui-authentication.org.
;;; Do not modify manually.

(define-module (sui services authentication)
  #:use-module (gnu services)
  #:use-module (gnu services authentication)
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
  #:use-module (sui packages password-utils)
  #:export (1password-configuration
            1password-service-type
            home-1password-service-type
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
  (user
   string
   "User to set for the 1password-cli privileged program."))

(define 1password-groups
  (list
   (user-group (name "onepassword"))
   (user-group (name "onepassword-cli"))))

(define 1password-privileged-programs
  (match-record-lambda <1password-configuration>
      (1password 1password-cli user)
    (list
     (privileged-program
       (program (file-append 1password-cli "/bin/op"))
       (setgid? #t)
       (user user)
       (group "onepassword-cli"))
     (privileged-program
       (program (file-append 1password
                             "/share/1Password/1Password-BrowserSupport"))
       (setgid? #t)
       (group "onepassword")))))

(define 1password-polkit-policies
  (match-record-lambda <1password-configuration>
      (1password)
    (list 1password)))

(define 1password-packages
  (match-record-lambda <1password-configuration>
      (1password 1password-cli)
    (list 1password 1password-cli)))

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
                              1password-packages)))
    (description "Set user groups, setuid programs, and polkit policies for
1Password.")))

(define home-1password-service-type
  (service-type
    (name 'home-1password)
    (extensions
     (list (service-extension home-shepherd-service-type
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

(define-configuration/no-serialization fprintd-pam-configuration
  (fprintd
   (file-like fprintd)
   "The fprintd package to use.")
  (gdm-fingerprint?
   (boolean #t)
   "Whether to create a @code{gdm-fingerprint} PAM service for GDM.")
  (pam-services
   (list-of-strings '())
   "Additional PAM service names to prepend fingerprint authentication to."))

(define fprintd-pam-fprintd-configuration
  (match-record-lambda <fprintd-pam-configuration>
      (fprintd)
    (fprintd-configuration
      (fprintd fprintd))))

(define fprintd-pam-extension
  (match-record-lambda <fprintd-pam-configuration>
      (fprintd gdm-fingerprint? pam-services)
    (define fprintd-module
      (file-append fprintd "/lib/security/pam_fprintd.so"))
    (append
     ;; Create a dedicated gdm-fingerprint PAM service for GDM.
     (if gdm-fingerprint?
         (list (pam-service
                 (inherit (unix-pam-service "gdm-fingerprint"
                                            #:login-uid? #t))
                 (auth (list (pam-entry
                               (control "required")
                               (module fprintd-module))))))
         '())
     ;; Transformer for additional PAM services (e.g. polkit-1).
     (if (null? pam-services)
         '()
         (let ((fprintd-entry
                (pam-entry
                  (control "sufficient")
                  (module fprintd-module))))
           (list (pam-extension
                   (transformer
                    (lambda (pam)
                      (if (member (pam-service-name pam) pam-services)
                          (pam-service
                            (inherit pam)
                            (auth (cons fprintd-entry
                                        (pam-service-auth pam))))
                          pam))))))))))

(define fprintd-pam-service-type
  (service-type
    (name 'fprintd-pam)
    (extensions
     (list (service-extension fprintd-service-type
                              fprintd-pam-fprintd-configuration)
           (service-extension pam-root-service-type
                              fprintd-pam-extension)))
    (default-value (fprintd-pam-configuration))
    (description "Run fprintd with PAM integration for fingerprint
authentication.  Extends the stock @code{fprintd-service-type} and adds PAM
entries for fingerprint login.")))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; authentication.scm ends here.
