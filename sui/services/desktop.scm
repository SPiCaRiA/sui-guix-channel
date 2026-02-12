;;; desktop.scm --- The Sui Guix Channel  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from sui-desktop.org.
;;; Do not modify manually.

(define-module (sui services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages polkit)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-hyprpolkitagent-service-type))

;;; Commentary:
;;;
;;; Desktop services (polkit agent, etc.).
;;;
;;; Code:

(define home-hyprpolkitagent-shepherd
  (list (shepherd-service
          (documentation "Start hyprpolkitagent.")
          (provision '(hyprpolkitagent))
          (start
           #~(make-forkexec-constructor
              (list #$(file-append hyprpolkitagent
                                   "/libexec/hyprpolkitagent"))))
          (stop #~(make-kill-destructor)))))

(define home-hyprpolkitagent-service-type
  (service-type
    (name 'home-hyprpolkitagent)
    (extensions
     (list (service-extension home-shepherd-service-type
                              (const home-hyprpolkitagent-shepherd))))
    (default-value #f)
    (description "Run hyprpolkitagent, a polkit authentication agent for
Wayland compositors.")))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; desktop.scm ends here.
