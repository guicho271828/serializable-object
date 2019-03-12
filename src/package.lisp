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
(defpackage serializable-object
  (:use :cl :alexandria)
  (:export serializable-object save load-instance))
(in-package :serializable-object)

;; blah blah blah.

(defclass serializable-object () ((pathname :initarg :pathname :initform nil)))

(defmethod make-load-form ((instance serializable-object) &optional env)
  (make-load-form-saving-slots instance :environment env))

(defgeneric save (instance &key pathname store verbose parents &allow-other-keys)
  (:documentation "Save an instance to a FASL file using the value of PATHNAME slot in the instance.
When PATHNAME is given as an argument,

+ the object is stored in the file specified by PATHNAME,
+ the slot value is _temporarily_ set to this value while saving the instance,
+ and the value of PATHNAME is used to save the PATHNAME slot in the saved object.

If STORE is non-nil when PATHNAME is given, PATHNAME also overwrites the slot value in the runtime object.
Otherwise the PATHNAME slot value is restored to the original value after returning from this function.

If PARENTS is non-nil (default: t), ENSURE-DIRECTORIES-EXIST is called to
ensure that the path exists.

When an error occurs during the call to SAVE (e.g. nonexisting directory or permission error),
the path is reverted to the original value.

How it works:

1. SAVE generic-function stores a single line of macro (initialization-form) to
   a temporary file and compiles a file under the dynamic environment where a
   special variable *instance* is bound to the object to be stored.

2. The file compiler expands the macro to the code that sets *instance* to the
   value of *instance*.  MAKE-LOAD-FORM expands the value into a loadable form.

3. To load the instance, LOAD-INSTANCE function sets up a dynamic binding for
   *instance* and load the compiled file in this dynamic environment. The
   compiled code sets the newly created object (by evaluating the form produced
   by make-load-form) to *instance*. LOAD-INSTANCE retrieves this value.
"))

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

(defvar *instance*)
(defmacro initialization-form ()
  `(setf *instance* ,*instance*))

(defmethod save ((instance serializable-object) &key verbose (parents t) &allow-other-keys)
  (with-slots (pathname) instance
    (let ((fasl (compile-file-pathname pathname)))
      (when verbose
        (format t "~&Saving object ~A to ~a ~%" instance fasl))
      (when parents
        (ensure-directories-exist fasl :verbose verbose))
      (uiop:with-temporary-file (:stream s :pathname source)
        (when verbose
          (format t "~&Writing a magic code to ~a ~%" source))
        (prin1 `(initialization-form) s)
        :close-stream
        (let ((*instance* instance))
          (compile-file source
                        :output-file fasl
                        :verbose verbose))))))

(defun load-instance (class &rest args &key pathname (if-does-not-exist t) verbose &allow-other-keys)
  "Load an instance from a file.

+ When IF-DOES-NOT-EXIST is non-nil (default), it checks the file existence.
+ When IF-DOES-NOT-EXIST is nil and the file does not exist, it calls MAKE-INSTANCE with the specified arguments.
+ When verbose is non-nil, it writes messages to the standard output.
+ It always checks if the loaded object is of the specified class.
"
  (remf args :if-does-not-exist)
  (remf args :verbose)
  (let ((fasl (when pathname
                (compile-file-pathname pathname))))
    (flet ((do-load ()
             (let (*instance*)
               (load fasl :verbose verbose)
               (assert (typep *instance* class))
               *instance*)))
      (if if-does-not-exist
          (progn
            (assert (and fasl (probe-file fasl)))
            (do-load))
          (if (and fasl (probe-file fasl))
              (do-load)
              (apply #'make-instance class args))))))
