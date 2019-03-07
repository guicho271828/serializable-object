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
  (:use :alexandria))
(in-package :serializable-object)

;; blah blah blah.


(defclass serializable-class (standard-class) ())
(defmethod validate-superclass ((c1 serializable-class) (c2 standard-class)) t)
(defmethod validate-superclass ((c1 standard-class) (c2 serializable-class)) t)

(defclass serializable-object () ((pathname :initarg :pathname))
  (:metaclass serializable-class))

(defgeneric save (instance &key pathname store verbose &allow-other-keys)
  (:documentation "Save an instance to a FASL file using the value of PATHNAME slot in the instance.
When PATHNAME is given as an argument,
the object is stored in this file,
the slot value is _temporarily_ set to this value while saving the instance,
and the value of PATHNAME is used to save the PATHNAME slot in the saved object.

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
                   (setf pathp pathname)))
        (:abort  (setf pathp oldpath))))))

(defvar *magic-storage* (make-hash-table))
(defvar *magic-object*)
(defmacro magic-form () `(setf (gethash (bt:current-thread) *magic-storage*) ,*magic-object*))

(defmethod make-load-form ((instance serializable-object) &optional env)
  (make-load-form-saving-slots instance :environment env))

(defmethod save ((instance serializable-object) &key verbose &allow-other-keys)
  (with-slots (pathname) instance
    (when verbose
      (format t "~&Saving object ~A to ~a...~%" instance pathname))
    (uiop:with-temporary-file (:stream s :pathname magic-pathname)
      ;; create a temporary file that contains this form only
      (prin1 `(magic-form) s)
      :close-stream
      (let ((*magic-object* instance))
        ;; the file compiler expands MAGIC-FORM into a class object literal, calls MAKE-LOAD-FORM, compile, write the result into the pathname
        (compile-file magic-pathname :output-file pathname)))))

(defmethod make-instance ((class serializable-class) &key pathname (load t) &allow-other-keys)
  (if (and pathname (probe-file pathname) load)
      (progn
        (load pathname)
        (let ((obj (gethash (bt:current-thread) *magic-storage*)))
          (remhash (bt:current-thread) *magic-storage*)
          obj))
      (call-next-method)))

