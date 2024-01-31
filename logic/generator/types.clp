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
(defclass MAIN::register
  (is-a USER)
  (slot value
        (type SYMBOL)
        (allowed-symbols g0 g1 g2 g3
                         g4 g5 g6 g7
                         g8 g9 g10 g11
                         g12 g13 g14 fp
                         pfp sp rip r3
                         r4 r5 r6 r7 
                         r8 r9 r10 r11
                         r12 r13 r14 r15)
        (access initialize-only)
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::literal
  (is-a USER)
  (slot value
        (type INTEGER)
        (range 0 31)
        (access initialize-only)
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::special-register
  (is-a USER)
  (slot value
        (type SYMBOL)
        (allowed-symbols ip pc ac tc)
        (access initialize-only)
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::float-literal
  (is-a USER)
  (slot value
        (type FLOAT)
        (allowed-values +1.0
                        +0.0)
        (access initialize-only)
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass MAIN::float-register
  (is-a USER)
  (slot value
        (type SYMBOL)
        (allowed-symbols fp0 fp1 fp2 fp3)
        (access initialize-only)
        (visibility public)
        (storage local)
        (default ?NONE)))

(definstances MAIN::declare-constants
              (g0 of register (value g0))
              (g1 of register (value g1))
              (g2 of register (value g2))
              (g3 of register (value g3))
              (g4 of register (value g4))
              (g5 of register (value g5))
              (g6 of register (value g6))
              (g7 of register (value g7))
              (g8 of register (value g8))
              (g9 of register (value g9))
              (g10 of register (value g10))
              (g11 of register (value g11))
              (g12 of register (value g12))
              (g13 of register (value g13))
              (g14 of register (value g14))
              (fp of register (value fp))
              (pfp of register (value pfp))
              (sp of register (value sp))
              (rip of register (value rip))
              (r3 of register (value r3))
              (r4 of register (value r4))
              (r5 of register (value r5))
              (r6 of register (value r6))
              (r7 of register (value r7))
              (r8 of register (value r8))
              (r9 of register (value r9))
              (r10 of register (value r10))
              (r11 of register (value r11))
              (r12 of register (value r12))
              (r13 of register (value r13))
              (r14 of register (value r14))
              (r15 of register (value r15))
              (ac of special-register (value ac))
              (pc of special-register (value pc))
              (ip of special-register (value ip))
              (tc of special-register (value tc))
              ([f1.0] of float-literal (value +1.0))
              ([f0.0] of float-literal (value +0.0))
              ([lit0] of literal (value 0))
              ([lit1] of literal (value 1))
              ([lit2] of literal (value 2))
              ([lit3] of literal (value 3))
              ([lit4] of literal (value 4))
              ([lit5] of literal (value 5))
              ([lit6] of literal (value 6))
              ([lit7] of literal (value 7))
              ([lit8] of literal (value 8))
              ([lit9] of literal (value 9))
              ([lit10] of literal (value 10))
              ([lit11] of literal (value 11))
              ([lit12] of literal (value 12))
              ([lit13] of literal (value 13))
              ([lit14] of literal (value 14))
              ([lit15] of literal (value 15))
              ([lit16] of literal (value 16))
              ([lit17] of literal (value 17))
              ([lit18] of literal (value 18))
              ([lit19] of literal (value 19))
              ([lit20] of literal (value 20))
              ([lit21] of literal (value 21))
              ([lit22] of literal (value 22))
              ([lit23] of literal (value 23))
              ([lit24] of literal (value 24))
              ([lit25] of literal (value 25))
              ([lit26] of literal (value 26))
              ([lit27] of literal (value 27))
              ([lit28] of literal (value 28))
              ([lit29] of literal (value 29))
              ([lit30] of literal (value 30))
              ([lit31] of literal (value 31)))

(defgeneric MAIN::emit-instruction)
(defgeneric MAIN::addc)
(defmethod MAIN::emit-instruction
  ((?opcode LEXEME))
  ?opcode)
(defmethod MAIN::emit-instruction
  ((?opcode LEXEME)
   (?arg0 LEXEME
          NUMBER))
  (format nil 
          "%s %s"
          (str-cat ?opcode)
          (str-cat ?arg0)))
(defmethod MAIN::emit-instruction
  ((?opcode LEXEME)
   (?arg0 LEXEME
          NUMBER)
   (?arg1 LEXEME
          NUMBER))
  (format nil 
          "%s %s, %s"
          (str-cat ?opcode)
          (str-cat ?arg0)
          (str-cat ?arg1)))

(defmethod MAIN::emit-instruction
  ((?opcode LEXEME)
   (?arg0 LEXEME
          NUMBER)
   (?arg1 LEXEME
          NUMBER)
   (?arg2 LEXEME
          NUMBER))
  (format nil 
          "%s %s, %s, %s"
          (str-cat ?opcode)
          (str-cat ?arg0)
          (str-cat ?arg1)
          (str-cat ?arg2)))
(defmethod MAIN::addc
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction addc
                    (send ?src1 get-value)
                    (send ?src2 get-value)
                    (send ?dest get-value)))
(defmethod MAIN::addi
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction addi
                    (send ?src1 get-value)
                    (send ?src2 get-value)
                    (send ?dest get-value)))

(defmethod MAIN::addo
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction addo
                    (send ?src1 get-value)
                    (send ?src2 get-value)
                    (send ?dest get-value)))

(defmethod MAIN::subc
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction subc
                    (send ?src1 get-value)
                    (send ?src2 get-value)
                    (send ?dest get-value)))

(defmethod MAIN::addr
  ((?src1 float-register
          register
          float-literal)
   (?src2 float-register
          register
          float-literal)
   (?dest float-register
          register))
  (emit-instruction addr
                    (send ?src1 get-value)
                    (send ?src2 get-value)
                    (send ?dest get-value)))

(defmethod MAIN::addrl
  ((?src1 float-register
          register
          float-literal)
   (?src2 float-register
          register
          float-literal)
   (?dest float-register
          register))
  (emit-instruction addrl
                    (send ?src1 get-value)
                    (send ?src2 get-value)
                    (send ?dest get-value)))

(defmethod MAIN::alterbit
  ((?bitpos register
            literal)
   (?src register
         literal)
   (?dest register))
  (emit-instruction alterbit
                    ?bitpos
                    ?src
                    ?dest))
(defmethod MAIN::and
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction and
                    (send ?src1 get-value)
                    (send ?src2 get-value)
                    (send ?dest get-value)))

(defmethod MAIN::andnot
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction andnot
                    (send ?src1 get-value)
                    (send ?src2 get-value)
                    (send ?dest get-value)))
(defmethod MAIN::notand
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction notand
                    (send ?src1 get-value)
                    (send ?src2 get-value)
                    (send ?dest get-value)))
(defmethod MAIN::atadd
  ((?src/dst register)
   (?src register
         literal)
   (?dest register))
  (emit-instruction atadd
                    (send ?src/dst get-value)
                    (send ?src get-value)
                    (send ?dest get-value)))

(defmethod MAIN::atanr
  ((?src1 float-register
          register
          float-literal)
   (?src2 float-register
          register
          float-literal)
   (?dest float-register
          register))
  (emit-instruction atanr
                    (send ?src1 get-value)
                    (send ?src2 get-value)
                    (send ?dest get-value)))

(defmethod MAIN::atanrl
  ((?src1 float-register
          register
          float-literal)
   (?src2 float-register
          register
          float-literal)
   (?dest float-register
          register))
  (emit-instruction atanrl
                    (send ?src1 get-value)
                    (send ?src2 get-value)
                    (send ?dest get-value)))

(defmethod MAIN::atmod
  ((?src/dst register)
   (?src register
         literal)
   (?dest register))
  (emit-instruction atmod
                    (send ?src/dst get-value)
                    (send ?src get-value)
                    (send ?dest get-value)))

(defmethod MAIN::b
  ((?targ LEXEME))
  (emit-instruction b 
                    ?targ))
(defmethod MAIN::bx
  ((?targ LEXEME))
  (emit-instruction bx
                    ?targ))

(defmethod MAIN::bal
  ((?targ LEXEME))
  (emit-instruction bal
                    ?targ))
(defmethod MAIN::balx
  ((?targ LEXEME)
   (?dest register))
  (emit-instruction balx
                    ?targ
                    (send ?dest get-value)))

(defmethod MAIN::bbc
  ((?bitpos register
            literal)
   (?src register)
   (?targ LEXEME))
  (emit-instruction bbc
                    (send ?bitpos get-value)
                    (send ?src get-value)
                    ?targ))
(defmethod MAIN::bbs
  ((?bitpos register
            literal)
   (?src register)
   (?targ LEXEME))
  (emit-instruction bbs
                    (send ?bitpos get-value)
                    (send ?src get-value)
                    ?targ))
