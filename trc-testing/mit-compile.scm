;;; -*- Mode: Scheme -*-

;;;; Testing Utility for Scheme
;;;; MIT Scheme Compilation Script

;;; Copyright (C) 2007, 2009 Taylor R. Campbell.
;;;
;;; This file is part of TRC-Testing.
;;;
;;; TRC-Testing is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; TRC-Testing is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with TRC-Testing.  If not, see
;;; <http://www.gnu.org/licenses/>.

(declare (usual-integrations))

(load-option 'CREF)

(with-working-directory-pathname
    (directory-pathname (current-load-pathname))
  (lambda ()
    (fluid-let ((sf/default-declarations '((USUAL-INTEGRATIONS))))
      (cf "syn-param")
      (cf "test")
      (cf "mit-test")
      (if (name->package '(EDWIN))
          (fluid-let ((sf/default-syntax-table (->environment '(EDWIN))))
            (cf "mit-edwin"))))
    (cref/generate-constructors "mit-test" 'ALL)))
