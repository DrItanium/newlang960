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
(deffunction LanguageGenerator::is-variable-type
             (?kind)
             (not (neq ?kind 
                       SF_VARIABLE
                       MF_VARIABLE)))
(deffunction LanguageGenerator::is-variadic-type
             (?kind)
             (eq ?kind 
                 MF_VARIABLE))
(defrule LanguageGenerator::make-variable-type
         ?f <- (object (is-a atom)
                       (name ?name)
                       (parent ?parent)
                       (kind ?var&:(is-variable-type ?var))
                       (value ?id))
         =>
         (unmake-instance ?f)
         (make-instance ?name of variable
                        (parent ?parent)
                        (id ?id)
                        (is-variadic (is-variadic-type ?var))))

(defrule LanguageGenerator::make-procedure
         ?f <- (object (is-a expression)
                       (name ?name)
                       (parent ?parent)
                       (contents procedure ?title ?args $?body))
         ?k <- (object (is-a expression)
                       (name ?args)
                       (contents $?decls))
         =>
         (unmake-instance ?f 
                          ?k)
         (make-instance ?name of procedure
                        (parent ?parent)
                        (title ?title)
                        (arguments (make-instance ?args of procedure-arguments
                                                  (parent ?name)
                                                  (arguments $?decls)))
                        (body $?body)))

                 
(defrule LanguageGenerator::make-single-procedure-argument
         (object (is-a procedure-arguments)
                 (arguments $?a ?expr $?c))
         ?f <- (object (is-a expression)
                       (name ?expr)
                       (parent ?p)
                       (contents ?atom ?kind))
         ?z <- (object (is-a variable)
                       (name ?atom)
                       (is-variadic FALSE)
                       (id ?name))
         =>
         (unmake-instance ?f ?z)
         (make-instance ?expr of single-procedure-argument
                        (parent ?p)
                        (id ?name)
                        (kind ?kind)))

