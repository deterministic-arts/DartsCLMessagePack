#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Message Pack
  Copyright (c) 2014 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#
  
(in-package "DARTS.LIB.MESSAGE-PACK")


(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-buffer (&optional (length '*))
  `(array octet (,length)))


(defmacro define-encoder-pair (conc-name (value &rest args) &body body)
  (let ((writer-name (intern (concatenate 'string "WRITE-PACKED-" (symbol-name conc-name)) :msgpack))
        (packer-name (intern (concatenate 'string "PACK-" (symbol-name conc-name)) :msgpack))
        (destination (gensym))
        (input (gensym)))
    `(progn
       (defun ,writer-name (,input ,destination ,@args)
         (macrolet ((put (value) (list 'write-byte value ',destination)))
           (let ((,value ,input))
             ,@body))
         ,input)
       (defun ,packer-name (,input ,destination ,@args)
         (macrolet ((put (value) (list 'vector-push-extend value ',destination)))
           (let ((,value ,input))
             ,@body))
         ,input))))


(defun write-packed-null (stream)
  (write-byte #xC0 stream)
  nil)

(defun pack-null (array)
  (vector-push-extend #xC0 array)
  nil)


(define-encoder-pair boolean (value)
  (put (if value #xC2 #xC3)))


(define-encoder-pair integer (value)
  (labels ((encode (code bits value)
             (loop 
               :initially (put code)
               :for p :downfrom (- bits 8) :to 0 :by 8
               :do (put (ldb (byte 8 p) value))))
           (bad-value ()
             (error 'type-error :datum value
                    :expected-type `(integer #.(- (expt 2 63)) #.(- (expt 2 64) 1)))))
    (declare (inline encode))
    (if (not (minusp value))
        (cond
          ((<= value #x7F) (put value))
          ((<= value #xFF) (put #xCC) (put value))
          ((<= value #xFFFF) (encode #xCD 16 value))
          ((<= value #xFFFFFFFF) (encode #xCE 32 value))
          ((<= value #xFFFFFFFFFFFFFFFF) (encode #xCF 64 value))
          (t (bad-value)))
        (macrolet ((bitrangep (value bits)
                     `(<= ,(- (expt 2 (- bits 1))) 
                          ,value 
                          ,(- (expt 2 (- bits 1)) 1))))
          (cond
            ((<= -32 value -1) (put (logand #xFF value)))
            ((bitrangep value 8) (put #xD0) (put (logand value #xFF)))
            ((bitrangep value 16) (encode #xD1 16 (logand value #xFFFF)))
            ((bitrangep value 32) (encode #xD2 16 (logand value #xFFFFFFFF)))
            ((bitrangep value 64) (encode #xD3 16 (logand value #xFFFFFFFFFFFFFFFF)))
            (t (bad-value)))))))


(define-encoder-pair single-float (value)
  (let ((value (encode-float32 (coerce value 'single-float))))
    (loop 
      :initially (put #xCA)
      :for p :downfrom 24 :to 0 :by 8
      :do (put (ldb (byte 8 p) value)))))


(define-encoder-pair double-float (value)
  (let ((value (encode-float64 (coerce value 'double-float))))
    (loop 
      :initially (put #xCB)
      :for p :downfrom 56 :to 0 :by 8
      :do (put (ldb (byte 8 p) value)))))


(define-encoder-pair string (value &key (start 0) end (encoding *default-character-encoding*))
  (flet ((write-length (code bits value)
           (loop 
             :initially (put code)
             :for p :downfrom (- bits 8) :to 0 :by 8
             :do (put (ldb (byte 8 p) value)))))
    (let* ((end (or end (length value)))
           (octets (string-to-octets value 
                                     :start start :end end 
                                     :encoding encoding))
           (length (length octets)))
        (cond
          ((<= length 31) (put (logior #b10100000 length)))
          ((<= length 255) (put #xD9) (put length))
          ((<= length #.(- (expt 2 16) 1)) (write-length #xDA 16 length))
          ((<= length #.(- (expt 2 32) 1)) (write-length #xDB 32 length))
          (t (error "string too long")))
        (loop
          :for byte :across octets :do (put byte)))))


(define-encoder-pair octet-array-header (length)
  (flet ((write-length (code bits value)
           (loop 
             :initially (put code)
             :for p :downfrom (- bits 8) :to 0 :by 8
             :do (put (ldb (byte 8 p) value)))))
    (cond
      ((<= length 255) (put #xC4) (put length))
      ((<= length #.(- (expt 2 16) 1)) (write-length #xC5 16 length))
      ((<= length #.(- (expt 2 32) 1)) (write-length #xC6 32 length))
      (t (error "byte array too long")))))


(define-encoder-pair octet-array (value &key (start 0) end)
  (flet ((write-length (code bits value)
           (loop 
             :initially (put code)
             :for p :downfrom (- bits 8) :to 0 :by 8
             :do (put (ldb (byte 8 p) value)))))
    (let* ((end (or end (length value)))
           (length (- end start)))
      (cond
        ((<= length 255) (put #xC4) (put length))
        ((<= length #.(- (expt 2 16) 1)) (write-length #xC5 16 length))
        ((<= length #.(- (expt 2 32) 1)) (write-length #xC6 32 length))
        (t (error "byte array too long")))
      (loop
        :for index :upfrom start :below end
        :do (put (aref value index))))))


(define-encoder-pair array-header (length)
  (flet ((write-length (code bits value)
           (loop 
             :initially (put code)
             :for p :downfrom (- bits 8) :to 0 :by 8
             :do (put (ldb (byte 8 p) value)))))
    (cond
      ((<= length 15) (put (logior #b10010000 length)))
      ((<= length #.(- (expt 2 16) 1)) (write-length #xDC 16 length))
      ((<= length #.(- (expt 2 32) 1)) (write-length #xDD 32 length))
      (t (error "object array too large")))))


(define-encoder-pair map-header (length)
  (flet ((write-length (code bits value)
           (loop 
             :initially (put code)
             :for p :downfrom (- bits 8) :to 0 :by 8
             :do (put (ldb (byte 8 p) value)))))
    (cond
      ((<= length 15) (put (logior #b10000000 length)))
      ((<= length #.(- (expt 2 16) 1)) (write-length #xDE 16 length))
      ((<= length #.(- (expt 2 32) 1)) (write-length #xDF 32 length))
      (t (error "object array too large")))))


(defun read-packed-value-or-header (stream)
  (labels
      ((maskp (mask byte value) (= (logand mask byte) value))
       (read-length (bytes)
         (loop
           :with value := 0
           :repeat bytes
           :do (let ((byte (read-byte stream)))
                 (setf value (logior (ash value 8) byte)))
           :finally (return value)))
       (twos-complement (unsigned bytes)
         (- (1+ (logxor unsigned (- (expt 2 (* bytes 8)) 1)))))
       (read-signed (bytes)
         (let ((unsigned (read-length bytes)))
           (if (logbitp (- (* bytes 8) 1) unsigned)
               unsigned
               (twos-complement unsigned bytes))))
       (read-single ()
         (let ((ub (read-length 4)))
           (decode-float32 ub)))
       (read-double ()
         (let ((ub (read-length 8)))
           (decode-float64 ub))))
    (let ((tag (read-byte stream nil nil)))
      (cond
        ((null tag) (values nil nil))
        ((zerop (logand #b10000000 tag)) (values tag :integer))
        ((maskp #b11100000 tag #b10100000) (values (logand tag #b11111) :string-header))
        ((maskp #b11100000 tag #b11100000) (values (twos-complement tag 1) :integer))
        ((maskp #b11110000 tag #b10000000) (values (logand tag #b1111) :map-header))
        ((maskp #b11110000 tag #b10010000) (values (logand tag #b1111) :array-header))
        ((eql tag #xC0) (values nil :null))
        ((eql tag #xC2) (values nil :boolean))
        ((eql tag #xC3) (values t :boolean))
        ((eql tag #xC4) (values (read-length 1) :octet-array-header))
        ((eql tag #xC5) (values (read-length 2) :octet-array-header))
        ((eql tag #xC6) (values (read-length 4) :octet-array-header))
        ((eql tag #xC7) (error "extensions not yet implemented"))
        ((eql tag #xC8) (error "extensions not yet implemented"))
        ((eql tag #xC9) (error "extensions not yet implemented"))
        ((eql tag #xCA) (values (read-single) :number))
        ((eql tag #xCB) (values (read-double) :number))
        ((eql tag #xCC) (values (read-byte stream) :integer))
        ((eql tag #xCD) (values (read-length 2) :integer))
        ((eql tag #xCE) (values (read-length 4) :integer))
        ((eql tag #xCF) (values (read-length 8) :integer))
        ((eql tag #xD0) (values (read-signed 2) :integer))
        ((eql tag #xD1) (values (read-signed 4) :integer))
        ((eql tag #xD2) (values (read-signed 8) :integer))
        ((eql tag #xD4) (error "extensions not yet implemented"))
        ((eql tag #xD5) (error "extensions not yet implemented"))
        ((eql tag #xD6) (error "extensions not yet implemented"))
        ((eql tag #xD7) (error "extensions not yet implemented"))
        ((eql tag #xD8) (error "extensions not yet implemented"))
        ((eql tag #xD9) (values (read-length 1) :string-header))
        ((eql tag #xDA) (values (read-length 2) :string-header))
        ((eql tag #xDB) (values (read-length 4) :string-header))
        ((eql tag #xDC) (values (read-length 2) :array-header))
        ((eql tag #xDD) (values (read-length 4) :array-header))
        ((eql tag #xDE) (values (read-length 2) :map-header))
        ((eql tag #xDF) (values (read-length 4) :map-header))
        (t (error "invalid tag byte value ~X" tag))))))



(defun read-packed-octet-array-data (length stream)
  (let* ((buffer (make-array length :element-type 'octet))
         (bytes-read (read-sequence buffer stream)))
    (unless (eql bytes-read length)
      (error "premature end of input in octet array"))
    buffer))


(defun read-packed-string-data (length stream &key (encoding *default-character-encoding*))
  (let* ((buffer (make-array length :element-type 'octet))
         (read (read-sequence buffer stream)))
    (unless (eql read length)
      (error "premature end of input in string"))
    (octets-to-string buffer :encoding encoding)))


(defun read-packed-value (stream 
                          &key (map-reader :alist) (array-reader :vector) (accept-eof nil) 
                               (default nil) (encoding *default-character-encoding*))
  (labels
      ((recurse (&optional accept-eof)
         (multiple-value-bind (value type) (read-packed-value-or-header stream)
           (cond
             ((null type)
              (if accept-eof
                  (values default nil)
                  (error "premature end of input stream")))
             ((eq type :map-header) (read-map value))
             ((eq type :array-header) (read-array value))
             ((eq type :string-header) (read-packed-string-data value stream :encoding encoding))
             ((eq type :octet-array-header) (read-packed-octet-array-data value stream))
             (t (values value type)))))
       (read-map (length)
         (macrolet ((read-pair (form)
                      `(let ((key (recurse))
                             (value (recurse)))
                         ,form)))
           (values
            (cond
              ((eq map-reader :alist)
               (loop
                 :repeat length
                 :collecting (read-pair (cons key value))))
              ((eq map-reader :plist)
               (loop
                 :repeat length
                 :nconcing (read-pair (list key value))))
              ((eq map-reader :hash)
               (loop
                 :with table := (make-hash-table :test 'equal)
                 :repeat length
                 :do (read-pair (setf (gethash key table) value))
                 :finally (return table)))
              (t (funcall map-reader length stream)))
            :map)))
       (read-array (length)
         (values
          (cond
            ((eq array-reader :list) 
             (loop
               :repeat length
               :collecting (recurse)))
            ((eq array-reader :vector)
             (loop
               :with buffer := (make-array length :element-type 't)
               :for k :upfrom 0 :below length
               :do (setf (aref buffer k) (recurse))
               :finally (return buffer)))
            (t (funcall array-reader length stream))))))
    (recurse accept-eof)))

