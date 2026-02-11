;;; dotfiles.scm --- The Sui Guix Channel  -*- mode: scheme; -*-
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is generated from sui-dotfiles.org.
;;; Do not modify manually.

(define-module (sui services dotfiles)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (home-sui-dotfiles-service-type
            home-sui-dotfiles-configuration))

;;; Commentary:
;;;
;;; Dotfiles home service.
;;;
;;; Code:

(define list-of-strings?
  (list-of string?))

(define (dotfile-entry? entry)
  (match entry
    [((? string?) (? string?)) #t]
    [((? string?) (? string?) (? boolean?)) #t]
    [((? string?) (? string?) 'mutable) #t]
    [_ #f]))
(define list-of-dotfile-entries?
  (list-of dotfile-entry?))

(define-configuration/no-serialization home-sui-dotfiles-configuration
  (source-directory
   (string)
   "Base directory for resolving relative source paths.")
  (home-files
   (list-of-dotfile-entries '())
   "List of (dest src [recursive?]) for home files.")
  (xdg-config-files
   (list-of-dotfile-entries '())
   "List of (dest src [recursive?]) for XDG config files.")
  (xdg-data-files
   (list-of-dotfile-entries '())
   "List of (dest src [recursive?]) for XDG data files.")
  (excluded
   (list-of-strings '())
   "List of file patterns to exclude."))

(define (make-exclusion-predicate excluded)
  (let ([rx (make-regexp
             (string-append "^(" (string-join excluded "|") ")$"))])
    (lambda (file stat)
      (not (regexp-exec rx (basename file))))))

(define (sanitize-store-name path)
  "Strip leading dot from basename of PATH for use as a store name."
  (let ((base (basename path)))
    (if (string-prefix? "." base)
        (string-drop base 1)
        base)))

(define (dotfile->file-entry source-directory select?)
  (match-lambda
    ;; Mutable directory: enumerate top-level children individually so that
    ;; the parent directory is a real (writable) directory, not a store symlink.
    [(dest src 'mutable)
     (let* ((src-dir (in-vicinity source-directory src))
            (all-children (or (scandir src-dir
                                       (lambda (f)
                                         (not (member f '("." "..")))))
                              '()))
            (children (if select?
                          (filter (lambda (child)
                                    (let ((p (in-vicinity src-dir child)))
                                      (select? p (lstat p))))
                                  all-children)
                          all-children)))
       (map (lambda (child)
              (let* ((child-path (in-vicinity src-dir child))
                     (child-dest (in-vicinity dest child))
                     (dir? (eq? 'directory
                                (stat:type (lstat child-path)))))
                (if dir?
                    (list child-dest
                          (local-file child-path
                                      (sanitize-store-name child)
                                      #:recursive? #t
                                      #:select? select?))
                    (list child-dest
                          (local-file child-path
                                      (sanitize-store-name child))))))
            children))]
    ;; Recursive directory (immutable).
    [(dest src #t)
     (list
      (list dest (local-file (in-vicinity source-directory src)
                             (sanitize-store-name src)
                             #:recursive? #t #:select? select?)))]
    ;; Single file.
    [(or (dest src #f) (dest src))
     (list
      (list dest (local-file (in-vicinity source-directory src)
                             (sanitize-store-name src))))]))

(define (make-entries getter)
  (lambda (config)
    (let* ([source-directory
            (home-sui-dotfiles-configuration-source-directory config)]
           [excluded (home-sui-dotfiles-configuration-excluded config)]
           [select? (and (not (null? excluded))
                         (make-exclusion-predicate excluded))])
      (append-map (dotfile->file-entry source-directory select?)
                  (getter config)))))

(define home-sui-dotfiles-service-type
  (service-type
    (name 'home-sui-dotfiles)
    (extensions
     (list (service-extension
            home-files-service-type
            (make-entries home-sui-dotfiles-configuration-home-files))
           (service-extension
            home-xdg-configuration-files-service-type
            (make-entries home-sui-dotfiles-configuration-xdg-config-files))
           (service-extension
            home-xdg-data-files-service-type
            (make-entries home-sui-dotfiles-configuration-xdg-data-files))))
    (description "Manage dotfiles across home, XDG config, and XDG data
directories.")))

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

;;; dotfiles.scm ends here.
