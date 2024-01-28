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
; include types
(include logic/stages/types.clp)
(defclass MAIN::has-parent
  (is-a USER)
  (role concrete)
  (slot parent
        (type INSTANCE
              SYMBOL)
        (allowed-symbols FALSE)
        (storage local)
        (visibility public))
  )


(defclass MAIN::expression
  (is-a has-parent)
  (role concrete)
  (slot operator
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot rest
             (type INSTANCE
                   LEXEME)
             (storage local)
             (visibility public)
             (default ?NONE)))



(defgeneric MAIN::mk-binary-expression)
(defgeneric MAIN::mk-unary-expression)
(defgeneric MAIN::mk-expression)
(defmethod MAIN::mk-expression
  ((?operator SYMBOL)
   (?rest MULTIFIELD))
  (make-instance of expression
                 (operator ?operator)
                 (rest ?rest)))
(defmethod MAIN::mk-expression
  ((?operator SYMBOL)
   $?rest)
  (mk-expression ?operator
                 ?rest))
(defmethod MAIN::mk-binary-expression
  "(?op ?left ?right)"
  ((?operator SYMBOL)
   (?left INSTANCE
          LEXEME)
   (?right INSTANCE
           LEXEME))
  (mk-expression ?operator
                 ?left
                 ?right))

(defmethod MAIN::mk-unary-expression
  "(?op ?target)"
  ((?operator SYMBOL)
   (?target INSTANCE
            LEXEME))
  (mk-expression ?operator
                 ?target))



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
