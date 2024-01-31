; newlang960
; Copyright (c) 2024, Joshua Scoggins
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(defclass MAIN::register
  (is-a USER)
  (slot value
        (type SYMBOL)
        (allowed-symbols g0 g1 g2 g3
                         g4 g5 g6 g7
                         g8 g9 g10 g11
                         g12 g13 g14 fp
                         pfp sp rip r3
                         r4 r5 r6 r7 
                         r8 r9 r10 r11
                         r12 r13 r14 r15)
        (access initialize-only)
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::literal
  (is-a USER)
  (slot value
        (type INTEGER)
        (range 0 31)
        (access initialize-only)
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::special-register
  (is-a USER)
  (slot value
        (type SYMBOL)
        (allowed-symbols ip pc ac tc)
        (access initialize-only)
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::float-literal
  (is-a USER)
  (slot value
        (type FLOAT)
        (allowed-values +1.0
                        +0.0)
        (access initialize-only)
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass MAIN::float-register
  (is-a USER)
  (slot value
        (type SYMBOL)
        (allowed-symbols fp0 fp1 fp2 fp3)
        (access initialize-only)
        (visibility public)
        (storage local)
        (default ?NONE)))
