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

(defrule LanguageGenerator::make-unary-operation
         ?f <- (object (is-a expression)
                       (contents ?first ?second)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of unary-operation
                        (parent ?parent)
                        (operation ?first)
                        (target ?second)))

(defrule LanguageGenerator::make-binary-operation
         ?f <- (object (is-a expression)
                       (contents ?op ?left ?right)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of binary-operation
                        (parent ?parent)
                        (operation ?op)
                        (left ?left)
                        (right ?right)))

(defrule LanguageGenerator::make-procedure-empty-body
         "It is possible that a procedure definition can be turned into a binary expression, convert it from that point to be on the safe side"
         ?f <- (object (is-a binary-operation)
                       (name ?name)
                       (parent ?parent)
                       (operation procedure)
                       (left ?title)
                       (right ?args))
         =>
         (unmake-instance ?f)
         (make-instance ?name of procedure
                        (parent ?parent)
                        (title ?title)
                        (args ?args)
                        (body)))

(defrule LanguageGenerator::make-procedure
         ?f <- (object (is-a expression)
                       (name ?name)
                       (parent ?parent)
                       (contents procedure ?title ?args $?body))
         =>
         (unmake-instance ?f)
         (make-instance ?name of procedure
                        (parent ?parent)
                        (title ?title)
                        (args ?args)
                        (body $?body)))
