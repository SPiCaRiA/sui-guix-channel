(define-module (koxia services authentication)
  #:use-module ((gnu services) #:select (service-type
                                         simple-service
                                         service-extension
                                         privileged-program-service-type))
  #:use-module ((gnu services configuration)
                #:select (define-configuration/no-serialization))
  #:use-module ((gnu services dbus) #:select (polkit-service-type))
  #:use-module ((gnu system) #:select (account-service-type))
  #:use-module ((gnu system accounts) #:select (user-group))
  #:use-module ((gnu system privilege) #:select (privileged-program))
  #:use-module ((guix gexp) #:select (file-union
                                      file-append))
  #:use-module ((guix records) #:select (match-record-lambda))
  #:use-module ((koxia packages 1password) #:select (1password
                                                     1password-cli))
  #:export (1password-configuration
            1password-service-type))

;;; Commentary:

;;; Code:

;;; 1Password
;;;

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

(define 1password-service-type
  (service-type
   (name '1password)
   (extensions
    (list (service-extension account-service-type
                             (const 1password-groups))
          (service-extension privileged-program-service-type
                             1password-privileged-programs)
          (service-extension polkit-service-type
                             1password-polkit-policies)))
   (description "Set user groups, setuid programs, and polkit policies for
1Password.")))
