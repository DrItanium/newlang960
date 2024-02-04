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

; The intermediate language is equivalent to LLVM bitcode or GCC IR. The difference is that I am 
; building this up like was done for assembly level coverage. We have different modules which are 
; used to select what kind of actions to generate
;
; It also is far more expressive in the available types to better map to what the i960 can do. 
; It also makes it trivial to build up the expression lists as we go along.
;
; For example, an absolute branch is really
; (set [ip] ?address)
; a relative branch is:
; (set [ip] (+ [ip] ?displacement))
;
; More specifically, address and displacement are physical types as well.
; A conditional relative branch is:
;
; (if ?cond then (set [ip] (+ [ip] ?displacement)))
; it can also be 
; (set [ip] (if ?cond then (+ [ip] ?displacement))) 
;
; but this second form is more consistent but has some implicit declarations...
;
; If everything comes in the form of set expressions then a bunch of operations become even easier to transform and operate on


; We need to break down any set of actions to their simplest parts including:
; - set expressions
; - get expressions
; - call expressions
; - return expressions
; - compare expressions
; - arithmetic expressions

; looking at the GCC code, pretty much everything is a set operation of some kind


; we define condition codes as the result of a compare operation. Then a check operation is applied to the condition code to see if it is true or not. 
; So (> ?a ?b) becomes (set [cc0] (> ?a ?b)) which we then do (if (lt [cc0])
; then ... else ...) which just takes a look at the resulting pattern to see if
; less than is satisfied (at least conceptually). 
; 
; The bit types are also important as well, I want them to be the boolean values which can only be checked, cleared, set, cleared, inverted, and altered.
