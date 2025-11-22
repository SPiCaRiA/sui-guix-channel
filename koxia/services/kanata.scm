(define-module (koxia services kanata)
  #:use-module ((gnu packages linux) #:select (kmod))
  #:use-module ((gnu packages rust-apps) #:select (kanata))
  #:use-module ((gnu services) #:select (service-type
                                         service-extension))
  #:use-module ((gnu services base) #:select (udev-service-type
                                              udev-rule))
  #:use-module ((gnu services configuration)
                #:select (define-configuration/no-serialization))
  #:use-module ((gnu services linux)
                #:select (kernel-module-loader-service-type))
  #:use-module ((gnu system accounts) #:select (user-group))
  #:use-module ((gnu system shadow) #:select (account-service-type))
  #:use-module ((guix gexp) #:select (file-append))
  #:use-module ((guix records) #:select (match-record-lambda))
  #:export (kanata-service-type))

;;; Commentary:

;;; Code:

(define kanata-groups
  (list
   (user-group (name "uinput") (system? #t))
   (user-group (name "input") (system? #t))))

(define kanata-udev-rules
  (udev-rule
   "99-kanata.rules"
   "KERNEL==\"uinput\", MODE=\"0660\", GROUP=\"uinput\", OPTIONS+=\"static_node=uinput\""))

(define kanata-service-type
  (service-type
   (name 'kanata)
   (extensions
    (list (service-extension account-service-type
                             (const kanata-groups))
          (service-extension udev-service-type
                             (const (list kanata-udev-rules)))
          (service-extension kernel-module-loader-service-type
                             (const '("uinput")))))
   (description "Kanata keyboard remapping service.")))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; kanata.scm ends here.
