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
; ripped from galclp
;(include logic/source-ident/module.clp)
;(include logic/pld/module.clp)
;(include logic/parent_ident/module.clp)
(include logic/stages/types.clp)
;(include logic/annotations/types.clp)
;(include logic/pld/types.clp)
;(include logic/parent_ident/types.clp)
;(include logic/expression/types.clp)

(deffunction MAIN::begin
             ()
             (printout stdout "donuts" crlf)
             )

;(deffacts MAIN::stages
;          (stage (current optimization-stage1)
;                 (rest flatten
;                   discovery
;                   correlate
;                   cleanup
;                   display)))
;
;(deffacts MAIN::allowed-identity-conversions
;          (convert-to-identity and-expression)
;          (convert-to-identity or-expression))
;
;(deffacts MAIN::allowed-flattening-targets
;          (can-flatten and-expression)
;          (can-flatten or-expression))
;
;(deffacts MAIN::clone-requests
;          (annotation-clone-request (target-kind reverse-non-expression-node)
;                                    (new-name input-to)))
;
;(deffacts MAIN::pld-emission-requests
;          (annotation (target display)
;                      (kind focus-on-stage)
;                      (reversible FALSE)
;                      (args EmitPLDLogic)))
;
;(deffacts MAIN::structurally-similar-unary-nodes
;          (annotation (target not-expression)
;                      (kind allow-fusion)
;                      (args))
;          (annotation (target identity-expression)
;                      (kind allow-fusion)
;                      (args)))
;
;(include logic/parent_ident/logic.clp)
;(include logic/pld/logic.clp)
;(include logic/annotations/logic.clp)
;(include logic/stages/logic.clp)
;(include logic/expression/logic.clp)

