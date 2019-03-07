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
(defpackage :serializable-object.test
  (:use :cl
        :serializable-object
        :fiveam :alexandria))
(in-package :serializable-object.test)



(def-suite :serializable-object)
(in-suite :serializable-object)

;; run test with (run! test-name) 

(defclass my (serializable-object)
  ((value :initarg :value :initform nil))
  (:metaclass serializable-class))

(defun my= (o1 o2)
  (eql (slot-value o1 'value)
       (slot-value o2 'value)))

(test test1
  (uiop:with-temporary-file (:pathname p)
    (let ((a (make-instance 'my :verbose t)))
      (save a :verbose t :pathname p)
      (finishes (describe a))
      (let ((b (make-instance 'my :verbose t :pathname p)))
        (finishes (describe b))

        (is (my= a b))))))

(test test2
  (uiop:with-temporary-file (:pathname p)
    (let ((a (make-instance 'my :verbose t :value 5)))
      (save a :verbose t :pathname p)
      (finishes (describe a))
      (let ((b (make-instance 'my :verbose t :pathname p)))
        (finishes (describe b))

        (is (my= a b))))))


(test test3
  (signals error
    (make-instance 'my :verbose t :value 5 :pathname "/no/such/file" :load t))
  (uiop:with-temporary-file (:pathname p)
    (signals error
      (make-instance 'my :verbose t :value 5 :pathname p :load t))))

(test test4
  (uiop:with-temporary-file (:pathname p)
    (finishes
      (make-instance 'my :verbose t :value 5 :pathname p :load nil))))


(test test5
  (uiop:with-temporary-file (:pathname p1)
    (uiop:with-temporary-file (:pathname p2)
      (let ((a (make-instance 'my :verbose t :value 5)))
        (finishes
          (save a :verbose t :pathname p1))
        (signals error
          (save a :verbose t))
        (finishes
          (save a :verbose t :pathname p1 :store t))
        (finishes
          (save a :verbose t))
        (finishes
          (save a :verbose t :pathname p2))
        (is (eql (slot-value a 'pathname) p1))))))
