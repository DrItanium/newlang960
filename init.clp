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
; A simple expert system to make it easier to write gal equations with more complex features
(defmodule MAIN
           (export ?ALL))
; include modules
(include logic/parser/module.clp)
; include types
(include logic/common/types.clp)
(include logic/stages/types.clp)
(include logic/parser/types.clp)


(defclass MAIN::expression
  (is-a has-parent)
  (role concrete)
  (multislot contents 
             (storage local)
             (visibility public)
             (default ?NONE)))



(defgeneric MAIN::mk-binary-expression)
(defgeneric MAIN::mk-unary-expression)
(defgeneric MAIN::mk-expression)
(defmethod MAIN::mk-expression
  ((?contents MULTIFIELD))
  (make-instance of expression
                 (contents ?contents)))
(defmethod MAIN::mk-expression
  ($?contents)
  (mk-expression ?contents))



(deffunction MAIN::begin
             ()
;             (printout stdout "donuts" crlf)
             )

; declare stages
;(deffacts MAIN::stages
;          (stage (current optimization-stage1)
;                 (rest flatten
;                   discovery
;                   correlate
;                   cleanup
;                   display)))
;
; deffacts/objects etc
; rules include
; 

; one type of declaration expression
; (?type (?name ?value) $?body)
; (let (?name 
;       ?type
;       ($?value))
;       $?body)

        

