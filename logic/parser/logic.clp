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

(defrule LispParser::generate-parser
         ?f <- (parse-request (kind file)
                              (path ?path))
         =>
         (retract ?f)
         (make-instance of parser
                        (path ?path)))


(defrule LispParser::read-token
         ?f <- (object (is-a parser)
                       (current-token)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id))
         =>
         (modify-instance ?f
                          (current-token (next-token ?id))))
(defrule LispParser::stop-parsing-file
         ?f <- (object (is-a parser)
                       (current-token ?kind ?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id))
         (core-type-translation (kind ?kind)
                                (action stop))
         =>
         (close ?id)
         (modify-instance ?f 
                          (current-token)
                          (parsing FALSE)
                          (parsed TRUE)))

(defrule LispParser::make-new-expression
         ?f <- (object (is-a parser)
                       (current-token ?kind ?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (current-element ?target))
         (core-type-translation (kind ?kind)
                                (action new-expression))
         ?k <- (object (is-a expression)
                       (name ?target)
                       (contents $?prior))
         =>
         (bind ?ncurr
               (make-instance of expression
                              (parent ?target)))
         (modify-instance ?f 
                          (current-token)
                          (current-element ?ncurr))
         (modify-instance ?k
                          (contents ?prior
                                    ?ncurr)))

(defrule LispParser::leave-current-expression:valid
         ?f <- (object (is-a parser)
                       (current-token ?kind ?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (current-element ?target))
         (core-type-translation (kind ?kind)
                                (action end-expression))
         (object (is-a expression)
                 (name ?target)
                 (parent ?parent&~FALSE))
         =>
         (modify-instance ?f 
                          (current-token)
                          (current-element ?parent)))

(defrule LispParser::leave-current-expression:invalid
         ?f <- (object (is-a parser)
                       (current-token ?kind ?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (path ?path)
                       (current-element ?target)
                       (name ?name))
         (core-type-translation (kind ?kind)
                                (action end-expression))
         (object (is-a expression)
                 (name ?target)
                 (parent FALSE))
         =>
         (printout stderr
                   "ERROR: mismatched parens, found a right paren without a matching left paren" crlf
                   "Target file: " ?path crlf)
         (halt))

(defrule LispParser::make-atomic-value
         "When it isn't a structural component we want to make atoms out of them!"
         ?f <- (object (is-a parser)
                       (current-token ?kind 
                                      ?value)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (current-element ?target))
         (not (core-type-translation (kind ?kind)))
         ?k <- (object (is-a expression)
                       (name ?target)
                       (contents $?prior))
         =>
         (modify-instance ?f
                          (current-token))
         (modify-instance ?k
                          (contents ?prior
                                    (make-instance of atom
                                                   (parent ?target)
                                                   (kind ?kind)
                                                   (value ?value)
                                                   (contents ?kind ?value)))))
(defrule LispParser::directly-place-atomic-type
         "When it isn't a structural component we want to make atoms out of them!"
         ?f <- (object (is-a parser)
                       (current-token ?kind 
                                      ?value)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (current-element ?target))
         (core-type-translation (kind ?kind)
                                (action hoist))
         ?k <- (object (is-a expression)
                       (name ?target)
                       (contents $?prior))
         =>
         (modify-instance ?f
                          (current-token))
         (modify-instance ?k
                          (contents ?prior
                                    ?value)))
