;;; niri.scm --- The Koxia Guix Channel  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from koxia-niri.org.
;;; Do not modify manually.

(define-module (koxia services niri)
  #:use-module (gnu home services)
  #:use-module ((gnu packages freedesktop) #:select (xdg-desktop-portal-gtk))
  #:use-module ((koxia packages gnome) #:select (xdg-desktop-portal-gnome-47))
  #:use-module ((gnu packages gnome) #:select (gnome-keyring))
  #:use-module ((gnu packages wm) #:select (niri))
  #:use-module ((gnu packages xorg) #:select (xwayland-satellite))
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (srfi srfi-1)

  #:export (niri-configuration
            home-niri-service-type))

(define-configuration/no-serialization niri-configuration
  (packages
   (list-of-packages %niri-default-packages)
   "List of packages to add to the profile."))

(define %niri-default-packages
  (list xdg-desktop-portal-gnome-47
        xdg-desktop-portal-gtk
        gnome-keyring
        xwayland-satellite))

(define home-niri-service-type
  (service-type
    (name 'home-niri)
    (extensions
     (list (service-extension home-profile-service-type
                              niri-configuration-packages)))
    (compose concatenate)
    (extend (lambda (config extra-packages)
              (niri-configuration
               (inherit config)
               (packages (append (niri-configuration-packages config)
                                 extra-packages)))))
    (description "Install and configure Niri, a scrollable tiling Wayland
compositor.  It ensures Niri's dependencies are available in the user's
profile.")
    (default-value (niri-configuration))))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; niri.scm ends here.
