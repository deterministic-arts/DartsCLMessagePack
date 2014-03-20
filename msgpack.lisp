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


(define-condition unencodable-object-error (error)
  ((datum 
     :initarg :datum :initform nil
     :reader unencodable-object-error-datum)
   (reason
     :initarg :reason :initform nil
     :reader unencodable-object-error-reason)
   (message
     :initarg :message :initform nil
     :reader unencodable-object-error-message))
  (:report (lambda (object stream)
             (let ((datum (unencodable-object-error-datum object))
                   (reason (unencodable-object-error-reason object))
                   (message (unencodable-object-error-message object)))
               (format stream "Failed to encode value ")
               (let ((*print-length* 16)
                     (*print-circle* t))
                 (prin1 datum stream))
               (format stream " using msgpack~@[ (~A)~]~@[: ~A~]"
                       reason message))))
  (:documentation "A condition of this type is signalled, if a given value
    cannot be encoded in the msgpack format."))

(defun unencodable-object-error (datum &optional reason control &rest arguments)
  (let ((message (and control (apply #'format nil control arguments))))
    (error 'unencodable-object-error
           :datum datum :reason reason
           :message message)))


(define-condition protocol-error (error) ())

(define-condition premature-end-of-input-error (end-of-file protocol-error) ()
  (:report (lambda (object stream)
             (format stream "reading from ~S: the end of the input stream was reached before all pending objects could be read completely" 
                           (stream-error-stream object)))))

(define-condition invalid-tag-byte-error (protocol-error stream-error)
  ((byte
     :initarg :value :initform nil
     :reader protocol-error-byte))
  (:report (lambda (object stream)
             (format stream "reading from ~S: the value ~D (~X) does not denote a supported opcode"
                     (stream-error-stream object)
                     (protocol-error-byte object)
                     (protocol-error-byte object)))))

(defun protocol-error-stream (object)
  (stream-error-stream object))




(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-buffer (&optional (length '*))
  `(array octet (,length)))


(defmacro define-encoder-pair (conc-name (value &rest args) &body body)
  (let ((writer-name (intern (concatenate 'string "WRITE-PACKED-" (symbol-name conc-name)) "DARTS.LIB.MESSAGE-PACK"))
        (packer-name (intern (concatenate 'string "PACK-" (symbol-name conc-name)) "DARTS.LIB.MESSAGE-PACK"))
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
  (put (if value #xC3 #xC2)))


(define-encoder-pair integer (value)
  (labels ((encode (code bits value)
             (loop 
               :initially (put code)
               :for p :downfrom (- bits 8) :to 0 :by 8
               :do (put (ldb (byte 8 p) value))))
           (bad-value ()
             (unencodable-object-error value :reason :out-of-range
                                       "encodable values must be of type ~S" '(integer #.(- (expt 2 63)) #.(- (expt 2 64) 1)))))
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
            ((bitrangep value 8) (put #xD0) (put (logand #xFF value)))
            ((bitrangep value 16) (encode #xD1 16 value))
            ((bitrangep value 32) (encode #xD2 32 value))
            ((bitrangep value 64) (encode #xD3 64 value))
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
          ;; This would be correct, but the current main raison d'etre for
          ;; this library cannot cope with the #xD9 single-byte-length encoding
          ;; ((<= length 255) (put #xD9) (put length))
          ((<= length #.(- (expt 2 16) 1)) (write-length #xDA 16 length))
          ((<= length #.(- (expt 2 32) 1)) (write-length #xDB 32 length))
          (t (unencodable-object-error value :too-large "value is too large")))
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
      (t (unencodable-object-error nil :too-large "length of ~D exceeds supported range" length)))))


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
        (t (unencodable-object-error value :too-large "value is too large")))
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
      (t (unencodable-object-error nil :too-large "length of ~D exceeds supported range" length)))))


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
      (t (unencodable-object-error nil :too-large "length of ~D exceeds supported range" length)))))


(defun write-packed-extension-header (type length stream)
  (labels ((put (byte) (write-byte byte stream))
           (write-length (code bits value)
             (loop 
               :initially (put code)
               :for p :downfrom (- bits 8) :to 0 :by 8
               :do (put (ldb (byte 8 p) value)))))
    (cond
      ((not (<= -128 type 127)) (error 'type-error :datum type :expected-type '(integer -128 127)))
      ((< length 0) (error 'type-error :datum length :expected-type '(integer 1 #xffffffff)))
      ((eql length  1) (put #xD4) (put (logand type #xFF)))
      ((eql length  2) (put #xD5) (put (logand type #xFF)))
      ((eql length  4) (put #xD6) (put (logand type #xFF)))
      ((eql length  8) (put #xD7) (put (logand type #xFF)))
      ((eql length 16) (put #xD8) (put (logand type #xFF)))
      ((<= length #xFF) (write-length #xC7 8 length) (put (logand type #xFF)))
      ((<= length #xFFFF) (write-length #xC8 16 length) (put (logand type #xFF)))
      ((<= length #xFFFFFFFF) (write-length #xC9 32 length) (put (logand type #xFF)))
      (t (unencodable-object-error nil :too-large "length of ~D exceeds supported range" length)))))



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
           (if (not (logbitp (- (* bytes 8) 1) unsigned))
               unsigned
               (twos-complement unsigned bytes))))
       (read-single ()
         (let ((ub (read-length 4)))
           (decode-float32 ub)))
       (read-double ()
         (let ((ub (read-length 8)))
           (decode-float64 ub)))
       (read-ext-header (size-size)
         (let ((size (read-length size-size)))
           (values size :extension (read-signed 1)))))
    (let ((tag (read-byte stream nil nil)))
      (cond
        ((null tag) (values nil nil))
        ((zerop (logand #b10000000 tag)) (values tag :integer nil))
        ((maskp #b11100000 tag #b10100000) (values (logand tag #b11111) :string-header nil))
        ((maskp #b11100000 tag #b11100000) (values (twos-complement tag 1) :integer nil))
        ((maskp #b11110000 tag #b10000000) (values (logand tag #b1111) :map-header nil))
        ((maskp #b11110000 tag #b10010000) (values (logand tag #b1111) :array-header nil))
        ((eql tag #xC0) (values nil :null nil))
        ((eql tag #xC2) (values nil :boolean nil))
        ((eql tag #xC3) (values t :boolean nil))
        ((eql tag #xC4) (values (read-length 1) :octet-array-header nil))
        ((eql tag #xC5) (values (read-length 2) :octet-array-header nil))
        ((eql tag #xC6) (values (read-length 4) :octet-array-header nil))
        ((eql tag #xC7) (read-ext-header 1))
        ((eql tag #xC8) (read-ext-header 2))
        ((eql tag #xC9) (read-ext-header 4))
        ((eql tag #xCA) (values (read-single) :number))
        ((eql tag #xCB) (values (read-double) :number))
        ((eql tag #xCC) (values (read-byte stream) :integer))
        ((eql tag #xCD) (values (read-length 2) :integer))
        ((eql tag #xCE) (values (read-length 4) :integer))
        ((eql tag #xCF) (values (read-length 8) :integer))
        ((eql tag #xD0) (values (read-signed 1) :integer))
        ((eql tag #xD1) (values (read-signed 2) :integer))
        ((eql tag #xD2) (values (read-signed 4) :integer))
        ((eql tag #xD3) (values (read-signed 8) :integer))
        ((eql tag #xD4) (values 1 :extension (read-signed 1)))
        ((eql tag #xD5) (values 2 :extension (read-signed 1)))
        ((eql tag #xD6) (values 4 :extension (read-signed 1)))
        ((eql tag #xD7) (values 8 :extension (read-signed 1)))
        ((eql tag #xD8) (values 16 :extension (read-signed 1)))
        ((eql tag #xD9) (values (read-length 1) :string-header))
        ((eql tag #xDA) (values (read-length 2) :string-header))
        ((eql tag #xDB) (values (read-length 4) :string-header))
        ((eql tag #xDC) (values (read-length 2) :array-header))
        ((eql tag #xDD) (values (read-length 4) :array-header))
        ((eql tag #xDE) (values (read-length 2) :map-header))
        ((eql tag #xDF) (values (read-length 4) :map-header))
        (t (error 'invalid-tag-byte-error :stream stream :byte tag))))))



(defun read-packed-octet-array-data (length stream)
  (let* ((buffer (make-array length :element-type 'octet))
         (bytes-read (read-sequence buffer stream)))
    (unless (eql bytes-read length)
      (error 'premature-end-of-input-error :stream stream))
    buffer))


(defun read-packed-string-data (length stream &key (encoding *default-character-encoding*))
  (let* ((buffer (make-array length :element-type 'octet))
         (read (read-sequence buffer stream)))
    (unless (eql read length)
      (error 'premature-end-of-input-error :stream stream))
    (octets-to-string buffer :encoding encoding)))


(defun read-packed-value (stream 
                          &key (map-reader :alist) (array-reader :vector) (extension-reader :buffer) 
                               (accept-eof nil) (default nil) (encoding *default-character-encoding*))
  (labels
      ((recurse (&optional accept-eof)
         (multiple-value-bind (value type tag) (read-packed-value-or-header stream)
           (cond
             ((null type)
              (if accept-eof
                  (values default nil)
                  (error 'premature-end-of-input-error)))
             ((eq type :map-header) (read-map value))
             ((eq type :array-header) (read-array value))
             ((eq type :string-header) (read-packed-string-data value stream :encoding encoding))
             ((eq type :octet-array-header) (read-packed-octet-array-data value stream))
             ((eq type :extension) (read-extension tag value))
             (t (values value type)))))
       (read-extension (type length)
         (if (eq extension-reader :buffer)
             (let* ((buffer (make-array length :element-type 'octet))
                    (read (read-sequence buffer stream)))
               (unless (eql read length) (error "too few bytes for extension ~D with length ~D (~D read)" type length read))
               (values buffer :extension type))
             (funcall extension-reader type length stream)))
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
            (t (funcall array-reader length stream)))
          :array)))
    (recurse accept-eof)))

