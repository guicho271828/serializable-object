#|

This file is a part of SERIALIZABLE-OBJECT project.
Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

SERIALIZABLE-OBJECT is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

SERIALIZABLE-OBJECT is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
SERIALIZABLE-OBJECT.  If not, see <http://www.gnu.org/licenses/>.

Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
Copyright (c) 2019 IBM Corporation

|#

(in-package :cl-user)
(uiop:define-package serializable-object
  (:mix :closer-mop :cl)
  (:use :alexandria)
  (:export serializable-class serializable-object save))
(in-package :serializable-object)

;; blah blah blah.


(defclass serializable-class (standard-class) ())
(defmethod validate-superclass ((c1 serializable-class) (c2 standard-class)) t)
(defmethod validate-superclass ((c1 standard-class) (c2 serializable-class)) t)

(defclass serializable-object () ((pathname :initarg :pathname :initform nil)) (:metaclass serializable-class))

(defgeneric save (instance &key pathname store verbose &allow-other-keys)
  (:documentation "Save an instance to a FASL file using the value of PATHNAME slot in the instance.
When PATHNAME is given as an argument,

+ the object is stored in the file specified by PATHNAME,
+ the slot value is _temporarily_ set to this value while saving the instance,
+ and the value of PATHNAME is used to save the PATHNAME slot in the saved object.

If STORE is non-nil when PATHNAME is given, PATHNAME also overwrites the slot value in the runtime object.
Otherwise the PATHNAME slot value is restored to the original value after returning from this function.

When an error occurs during the call to SAVE (e.g. nonexisting directory or permission error),
the path is reverted to the original value."))

(defmethod save :around ((instance serializable-object) &key pathname store &allow-other-keys)
  (with-slots ((pathp pathname)) instance
    (let ((oldpath pathp))
      (assert (or oldpath pathname))
      (when pathname
        (setf pathp pathname))
      (unwind-protect-case ()
          (call-next-method)
        (:normal (unless (and pathname store)
                   (setf pathp oldpath)))
        (:abort  (setf pathp oldpath))))))

(defvar *magic-storage* (make-hash-table))
(defvar *magic-object*)
(defmacro magic-form () `(setf (gethash (bt:current-thread) *magic-storage*) ,*magic-object*))

(defmethod make-load-form ((instance serializable-object) &optional env)
  (make-load-form-saving-slots instance :environment env))

(defmethod save ((instance serializable-object) &key verbose &allow-other-keys)
  (with-slots (pathname) instance
    (when verbose
      (format t "~&Saving object ~A to ~a ~%" instance (compile-file-pathname pathname)))
    (uiop:with-temporary-file (:stream s :pathname magic-pathname :keep t)
      ;; create a temporary file that contains this form only
      (prin1 `(magic-form) s)
      (finish-output s)
      :close-stream
      ;; (format t "~& magic-pathname ~a ~%" magic-pathname)
      (let ((*magic-object* instance))
        ;; the file compiler expands MAGIC-FORM into a class object literal, calls MAKE-LOAD-FORM, compile, write the result into the pathname
        (compile-file magic-pathname :output-file pathname
                      :verbose verbose)))))

(defmethod make-instance ((class serializable-class) &rest args
                          &key pathname (load nil load-specified-p) verbose &allow-other-keys)
  (remf args :load)
  (remf args :verbose)
  (flet ((do-load ()
           (load (compile-file-pathname pathname) :verbose verbose)
           (multiple-value-bind (obj present) (gethash (bt:current-thread) *magic-storage*)
             (assert present)
             (remhash (bt:current-thread) *magic-storage*)
             obj)))
    
    (cond
      (load
       (assert (and pathname (probe-file (compile-file-pathname pathname))))
       (do-load))
      
      ((and load-specified-p (null load))
       ;; when specified to nil, do not load
       (apply #'call-next-method class args))

      (t
       ;; if unspecified, then load optionally
       (if (and pathname (probe-file (compile-file-pathname pathname)))
           (do-load)
           (apply #'call-next-method class args))))))
