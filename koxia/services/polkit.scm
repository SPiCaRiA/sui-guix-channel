(define-module (koxia services authentication)
  #:use-module ((gnu services) #:select (simple-service))
  #:use-module ((gnu services dbus) #:select (polkit-service-type))
  #:use-module ((guix gexp) #:select (file-union
                                      file-append))
  #:use-module ((koxia packages 1password) #:select (1password))
  #:export (polkit-1password-service))

;;; Commentary:

;;; Code:

(define polkit-1password
  (file-union
   "polkit-1password"
   `(("share/polkit-1/actions/com.1password.Password.policy"
      ,(file-append 1password
                    "share/polkit-1/actions/com.1password.Password.policy")))))

(define polkit-1password-service
  (simple-service 'polkit-1password
                  polkit-service-type
                  (list polkit-1password)))
