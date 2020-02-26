#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Message Pack
  Copyright (c) 2014, 2020 Dirk Esser

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

(defpackage #:darts.lib.message-pack
  (:use #:common-lisp #:ieee-floats #:babel)
  (:export 
    #:write-packed-null #:write-packed-boolean #:write-packed-integer
    #:write-packed-single-float #:write-packed-double-float #:write-packed-string 
    #:write-packed-octet-array #:write-packed-octet-array-header
    #:write-packed-array-header #:write-packed-map-header #:write-packed-extension-header
    #:read-packed-value-or-header #:read-packed-string-data #:read-packed-octet-array-data 
    #:read-packed-value #:unencodable-object-error #:unencodable-object-error-datum 
    #:unencodable-object-error-reason #:unencodable-object-error-message #:protocol-error 
    #:protocol-error-byte #:protocol-error-stream #:premature-end-of-input-error 
    #:invalid-tag-byte-error #:*default-character-encoding*)
  (:documentation "A simple implementation of the message pack binary data
    encoding format. All value families are supported."))
