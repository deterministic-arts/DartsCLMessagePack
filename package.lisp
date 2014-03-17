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

(defpackage "DARTS.LIB.MESSAGE-PACK"
  (:use "COMMON-LISP" "IEEE-FLOATS" "BABEL")
  (:export 
    "WRITE-PACKED-NULL" "WRITE-PACKED-BOOLEAN" "WRITE-PACKED-INTEGER"
    "WRITE-PACKED-SINGLE-FLOAT" "WRITE-PACKED-DOUBLE-FLOAT" 
    "WRITE-PACKED-STRING" "WRITE-PACKED-OCTET-ARRAY" "WRITE-PACKED-OCTET-ARRAY-HEADER"
    "WRITE-PACKED-ARRAY-HEADER" "WRITE-PACKED-MAP-HEADER" "WRITE-PACKED-EXTENSION-HEADER"
    
    "READ-PACKED-VALUE-OR-HEADER" "READ-PACKED-STRING-DATA" 
    "READ-PACKED-OCTET-ARRAY-DATA" "READ-PACKED-VALUE"
           
    ;; Re-export from babel
    "*DEFAULT-CHARACTER-ENCODING*"))
