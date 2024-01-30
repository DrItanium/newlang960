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
         (progn$ (?a ?decls)
                 (send ?a put-parent ?name))
         (make-instance ?name of procedure
                        (parent ?parent)
                        (title ?title)
                        (arguments ?decls)
                        (body $?body)))

                 
(defrule LanguageGenerator::make-single-procedure-argument
         (object (is-a procedure)
                 (arguments $? ?curr $?))
         ?f <- (object (is-a expression)
                 (name ?curr)
                 (parent ?parent)
                 (contents ?atom ?kind))
         ?z <- (object (is-a local-singlefield-variable)
                       (name ?atom)
                       (value ?var-name))
         =>
         (unmake-instance ?f 
                          ?z)
         (make-instance ?curr of local-procedure-variable
                        (parent ?parent)
                        (id ?var-name)
                        (kind ?kind)))

(defrule LanguageGenerator::generate-parent-connection-fact
         (object (is-a has-parent)
                 (name ?name)
                 (parent ?parent&~FALSE))
         =>
         (assert (parent-linkage ?name ?parent)))

(defrule LanguageGenerator::generate-indirect-parent-connection-fact
         ?f <- (parent-linkage ?parent ?grandparent)
         ?f2 <- (parent-linkage ?child ?parent)
         (test (neq ?f ?f2))
         =>
         (assert (parent-linkage ?child ?grandparent)))

(defrule LanguageGenerator::denote-expression-is-part-of-a-procedure
         (parent-linkage ?curr ?procedure)
         (object (is-a procedure)
                 (name ?procedure))
         (object (is-a has-parent)
                 (name ?curr))
         =>
         (assert (procedure-linkage (target-procedure ?procedure)
                                    (target-subexpression ?curr))))
