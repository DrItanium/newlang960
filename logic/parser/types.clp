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

(defclass MAIN::expression
  "A container of expressions and other kinds of values"
  (is-a has-parent)
  (multislot contents
             (storage local)
             (visibility public)))
(defclass MAIN::atom
  "An element that is not an expression but is storable in one"
    (is-a has-parent)
    (slot kind
          (type SYMBOL)
          (storage local)
          (visibility public)
          (default ?NONE))
    (slot value
          (storage local)
          (visibility public)
          (default ?NONE))
    )

(defclass MAIN::file-expression
  "The parsed contents of a file"
  (is-a expression)
  (slot file-handle
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass LispParser::parser 
          (is-a USER)
          (slot parsed 
                (type SYMBOL)
                (allowed-symbols FALSE
                                 TRUE)
                (storage local)
                (visibility public))
          (slot parsing 
                (type SYMBOL)
                (allowed-symbols FALSE
                                 TRUE)
                (storage local)
                (visibility public))
          (slot valid 
                (type SYMBOL)
                (allowed-symbols FALSE
                                 TRUE)
                (storage local)
                (visibility public))
          (slot path
                (type LEXEME)
                (storage local)
                (visibility public)
                (default ?NONE))
          (slot id 
                (type SYMBOL)
                (storage local)
                (visibility public)
                (default-dynamic (gensym*)))
          (slot top-element
                (type INSTANCE)
                (storage local)
                (visibility public))
          (slot current-element 
                (type INSTANCE)
                (storage local)
                (visibility public))
          (multislot current-token
                     (storage local)
                     (visibility public))
          (message-handler init after)
          )
(defmessage-handler LispParser::parser init after 
                    ()
                    (bind ?self:valid
                          (open ?self:path
                                ?self:id
                                "r"))
                    (bind ?self:parsing
                          ?self:valid)
                    (bind ?self:top-element
                          (make-instance of file-container
                                         (parent FALSE)
                                         (file-name ?self:path)))
                    (bind ?self:current-element
                          ?self:top-element))


