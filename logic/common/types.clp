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

(defclass MAIN::has-parent
  (is-a USER)
  (slot parent
        (type SYMBOL
              INSTANCE)
        (allowed-symbols FALSE)
        (storage local)
        (visibility public)))

(defclass MAIN::has-title
  (is-a USER)
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass MAIN::has-description
  (is-a USER)
  (slot description
        (type LEXEME)
        (storage local)
        (visibility public)))
(defclass MAIN::has-children
  (is-a USER)
  (multislot children
             (storage local)
             (visibility public)))


(deffunction MAIN::join-string
             (?character ?first $?elements)
             (bind ?output
                   ?first)
             (progn$ (?item ?elements)
                     (bind ?output
                           ?output
                           ?character
                           ?item))
             (if (multifieldp ?output) then
               (str-cat (expand$ ?output))
               else
               ?output))

(defclass MAIN::expression
  (is-a has-parent
        has-children))
  
(defclass MAIN::named-expression
  (is-a expression
        has-title))

(deffunction MAIN::apply$
             (?function $?arguments)
             (bind ?output
                   (create$))
             (progn$ (?arg ?arguments)
                     (bind ?output
                           ?output
                           (funcall ?function ?arg)))

             ?output)
(deffunction MAIN::apply-message$
             (?message $?arguments)
             (bind ?output
                   (create$))
             (progn$ (?arg ?arguments)
                     (bind ?output
                           ?output
                           (send ?arg 
                                 ?message)))
             ?output)
