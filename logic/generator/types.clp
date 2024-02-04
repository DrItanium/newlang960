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
(defmessage-handler NUMBER to-string primary
                    ()
                    (str-cat ?self))
(defmessage-handler LEXEME to-string primary
                    ()
                    ?self)
        
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

(defglobal MAIN
           ; quick access versions
           ?*freg* = (create$ float-register
                              register)
           ?*freg/flit* = (create$ ?*freg*
                                   float-literal)
           ?*reg/lit* = (create$ register
                                 literal)
           ?*int/lex* = (create$ INTEGER
                                 LEXEME))
(defmessage-handler register to-string primary () (dynamic-get value))
(defmessage-handler literal to-string primary () (str-cat (dynamic-get value)))
(defmessage-handler float-literal to-string primary () (str-cat (dynamic-get value)))
(defmessage-handler float-register to-string primary () (dynamic-get value))
(defmessage-handler special-register to-string primary () (dynamic-get value))
(definstances MAIN::declare-constants
              (fp0 of float-register (value fp0))
              (fp1 of float-register (value fp1))
              (fp2 of float-register (value fp2))
              (fp3 of float-register (value fp3))
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
(defmethod MAIN::emit-instruction
  ((?opcode LEXEME))
  ?opcode)
(defmethod MAIN::emit-instruction
  ((?opcode LEXEME)
   (?args MULTIFIELD))
  (format nil
          "%s %s"
          ?opcode
          (join-string ,
                       (expand$ (apply-message$ to-string
                                                ?args)))))
(defmethod MAIN::emit-instruction
  ((?opcode LEXEME)
   $?args)
  (emit-instruction ?opcode
                    ?args))

(defmethod MAIN::addc
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction addc
                    ?src1 
                    ?src2 
                    ?dest))
(defmethod MAIN::addi
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction addi
                    ?src1 
                    ?src2 
                    ?dest ))

(defmethod MAIN::addo
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction addo
                    ?src1 
                    ?src2 
                    ?dest ))


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
                    ?src1 
                    ?src2 
                    ?dest ))

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
                    ?src1 
                    ?src2 
                    ?dest ))

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
                    ?src1 
                    ?src2 
                    ?dest ))

(defmethod MAIN::andnot
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction andnot
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::atadd
  ((?src/dest register)
   (?src register
         literal)
   (?dest register))
  (emit-instruction atadd
                    ?src/dest 
                    ?src 
                    ?dest ))

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
                    ?src1 
                    ?src2 
                    ?dest ))

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
                    ?src1 
                    ?src2 
                    ?dest ))

(defmethod MAIN::atmod
  ((?src/dest register)
   (?src register
         literal)
   (?dest register))
  (emit-instruction atmod
                    ?src/dest 
                    ?src 
                    ?dest ))

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
                    ?dest ))

(defmethod MAIN::bbc
  ((?bitpos register
            literal)
   (?src register)
   (?targ LEXEME))
  (emit-instruction bbc
                    ?bitpos 
                    ?src 
                    ?targ))
(defmethod MAIN::bbs
  ((?bitpos register
            literal)
   (?src register)
   (?targ LEXEME))
  (emit-instruction bbs
                    ?bitpos 
                    ?src 
                    ?targ))
(defmethod MAIN::be ((?targ LEXEME)) (emit-instruction be ?targ))
(defmethod MAIN::bne ((?targ LEXEME)) (emit-instruction bne ?targ))
(defmethod MAIN::bl ((?targ LEXEME)) (emit-instruction bl ?targ))
(defmethod MAIN::ble ((?targ LEXEME)) (emit-instruction ble ?targ))
(defmethod MAIN::bg ((?targ LEXEME)) (emit-instruction bg ?targ))
(defmethod MAIN::bge ((?targ LEXEME)) (emit-instruction bge ?targ))
(defmethod MAIN::bo ((?targ LEXEME)) (emit-instruction bo ?targ))
(defmethod MAIN::bno ((?targ LEXEME)) (emit-instruction bno ?targ))
(defmethod MAIN::call ((?targ LEXEME)) (emit-instruction call ?targ))
(defmethod MAIN::calls
  ((?targ register
          literal))
  (emit-instruction calls
                    ?targ ))
(defmethod MAIN::callx
  ((?targ LEXEME))
  (emit-instruction callx
                    ?targ))

(defmethod MAIN::chkbit
  ((?bitpos register
            literal)
   (?src register
         literal))
  (emit-instruction chkbit
                    ?bitpos 
                    ?src ))

(defmethod MAIN::classr
  ((?src float-register
         register
         float-literal))
  (emit-instruction classr
                    ?src ))


(defmethod MAIN::classrl
  ((?src float-register
         register
         float-literal))
  (emit-instruction classrl
                    ?src ))
(defmethod MAIN::clrbit
  ((?bitpos register
            literal)
   (?src register
         literal)
   (?dest register))
  (emit-instruction clrbit
                    ?bitpos
                    ?src
                    ?dest))
(defmethod MAIN::cmpi
  ((?src1 register
          literal)
   (?src2 register
          literal))
  (emit-instruction cmpi
                    ?src1
                    ?src2))


(defmethod MAIN::cmpo
  ((?src1 register
          literal)
   (?src2 register
          literal))
  (emit-instruction cmpo
                    ?src1
                    ?src2))

(defmethod MAIN::cmpdeci
  ((?src1 register 
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction cmpdeci
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::cmpdeco
  ((?src1 register 
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction cmpdeci
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::cmpinci
  ((?src1 register 
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction cmpinci
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::cmpinco
  ((?src1 register 
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction cmpinco
                    ?src1
                    ?src2
                    ?dest))

(defmethod MAIN::cmpor
  ((?src1 register
          float-register
          float-literal)
   (?src2 register
          float-register
          float-literal))
  (emit-instruction cmpor
                    ?src1
                    ?src2))
(defmethod MAIN::cmporl
  ((?src1 register
          float-register
          float-literal)
   (?src2 register
          float-register
          float-literal))
  (emit-instruction cmporl
                    ?src1
                    ?src2))

(defmethod MAIN::cmpr
  ((?src1 register
          float-register
          float-literal)
   (?src2 register
          float-register
          float-literal))
  (emit-instruction cmpr
                    ?src1
                    ?src2))
(defmethod MAIN::cmprl
  ((?src1 register
          float-register
          float-literal)
   (?src2 register
          float-register
          float-literal))
  (emit-instruction cmprl
                    ?src1
                    ?src2))
(defmethod MAIN::cmpstr
  ((?src1 register)
   (?src2 register)
   (?len register
         literal))
  (emit-instruction cmpstr
                    ?src1
                    ?src2
                    ?len))

(defmethod MAIN::cmpibe ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpibe ?src1 ?src2 ?targ))
(defmethod MAIN::cmpibne ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpibne ?src1 ?src2 ?targ))
(defmethod MAIN::cmpibl ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpibl ?src1 ?src2 ?targ))
(defmethod MAIN::cmpible ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpible ?src1 ?src2 ?targ))
(defmethod MAIN::cmpibg ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpibg ?src1 ?src2 ?targ))
(defmethod MAIN::cmpibge ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpibge ?src1 ?src2 ?targ))
(defmethod MAIN::cmpibo ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpibo ?src1 ?src2 ?targ))
(defmethod MAIN::cmpibno ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpibno ?src1 ?src2 ?targ))
(defmethod MAIN::cmpobe ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpobe ?src1 ?src2 ?targ))
(defmethod MAIN::cmpobne ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpobne ?src1 ?src2 ?targ))
(defmethod MAIN::cmpobl ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpobl ?src1 ?src2 ?targ))
(defmethod MAIN::cmpoble ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpoble ?src1 ?src2 ?targ))
(defmethod MAIN::cmpobg ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpobg ?src1 ?src2 ?targ))
(defmethod MAIN::cmpobge ((?src1 register literal) (?src2 register) (?targ INTEGER LEXEME)) (emit-instruction cmpobge ?src1 ?src2 ?targ))

(defmethod MAIN::concmpi
  ((?src1 register
          literal)
   (?src2 register
          literal))
  (emit-instruction concmpi
                    ?src1
                    ?src2))
(defmethod MAIN::concmpo
  ((?src1 register
          literal)
   (?src2 register
          literal))
  (emit-instruction concmpo
                    ?src1
                    ?src2))

(defmethod MAIN::condrec
  ((?src register)
   (?dest register))
  (emit-instruction condrec
                    ?src
                    ?dest))

(defmethod MAIN::condwait
  ((?src register))
  (emit-instruction condwait
                    ?src))

(defmethod MAIN::cosr ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction cosr ?src ?dest))
(defmethod MAIN::cosrl ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction cosrl ?src ?dest))

(defmethod MAIN::cpyrsre
  ((?src1 register 
          float-register
          float-literal)
   (?src2 register
          float-register
          float-literal)
   (?dest register
          float-register))
  (emit-instruction cpyrsre
                    ?src1
                    ?src2
                    ?dest))

(defmethod MAIN::cpysre
  ((?src1 register 
          float-register
          float-literal)
   (?src2 register
          float-register
          float-literal)
   (?dest register
          float-register))
  (emit-instruction cpysre
                    ?src1
                    ?src2
                    ?dest))

(defmethod MAIN::cvtilr ((?src register literal) (?dest float-register register)) (emit-instruction cvtilr ?src ?dest))
(defmethod MAIN::cvtir ((?src register literal) (?dest float-register register)) (emit-instruction cvtir ?src ?dest))

(defmethod MAIN::cvtri
  ((?src register
         float-register
         float-literal)
   (?dest register))
  (emit-instruction cvtri
                    ?src
                    ?dest))
(defmethod MAIN::cvtril
  ((?src register
         float-register
         float-literal)
   (?dest register))
  (emit-instruction cvtril
                    ?src
                    ?dest))
(defmethod MAIN::cvtzri
  ((?src register
         float-register
         float-literal)
   (?dest register))
  (emit-instruction cvtzri
                    ?src
                    ?dest))
(defmethod MAIN::cvtzril
  ((?src register
         float-register
         float-literal)
   (?dest register))
  (emit-instruction cvtzril
                    ?src
                    ?dest))

(defmethod MAIN::daddc
  ((?src1 register)
   (?src2 register)
   (?dest register))
  (emit-instruction daddc
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::dsubc
  ((?src1 register)
   (?src2 register)
   (?dest register))
  (emit-instruction dsubc
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::dmovt
  ((?src1 register)
   (?src2 register)
   (?dest register))
  (emit-instruction dmovt
                    ?src1
                    ?src2
                    ?dest))

(defmethod MAIN::divi
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction divi
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::divo
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction divo
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::divr
  ((?src1 register
          float-register
          float-literal)
   (?src2 float-register
          register
          float-literal)
   (?dest register
          float-register))
  (emit-instruction divr
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::divrl
  ((?src1 register
          float-register
          float-literal)
   (?src2 float-register
          register
          float-literal)
   (?dest register
          float-register))
  (emit-instruction divrl
                    ?src1
                    ?src2
                    ?dest))

(defmethod MAIN::ediv
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction ediv
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::emul
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction emul
                    ?src1
                    ?src2
                    ?dest))

(defmethod MAIN::expr ((?src float-register float-literal register) (?dest float-register float-literal)) (emit-instruction expr ?src ?dest))
(defmethod MAIN::exprl ((?src float-register float-literal register) (?dest float-register float-literal)) (emit-instruction exprl ?src ?dest))

(defmethod MAIN::extract
  ((?bitpos register
            literal)
   (?len register
         literal)
   (?src/dest register))
  (emit-instruction extract
                    ?bitpos
                    ?len
                    ?src/dest))

(defmethod MAIN::faulte () (emit-instruction faulte))
(defmethod MAIN::faultne () (emit-instruction faultne))
(defmethod MAIN::faultl () (emit-instruction faultl))
(defmethod MAIN::faultle () (emit-instruction faultle))
(defmethod MAIN::faultg () (emit-instruction faultg))
(defmethod MAIN::faultge () (emit-instruction faultge))
(defmethod MAIN::faulto () (emit-instruction faulto))
(defmethod MAIN::faultno () (emit-instruction faultno))

(defmethod MAIN::fill
  ((?dest register)
   (?value register
           literal)
   (?len register
         literal))
  (emit-instruction fill
                    ?dest
                    ?value
                    ?len))
(defmethod MAIN::flushreg () (emit-instruction flushreg))
(defmethod MAIN::fmark () (emit-instruction fmark))

(defmethod MAIN::inspacc
  ((?src register)
   (?dest register))
  (emit-instruction inspacc
                    ?src
                    ?dest))

(defmethod MAIN::ld ((?src LEXEME INTEGER) (?dest register)) (emit-instruction ld ?src ?dest))
(defmethod MAIN::ldob ((?src LEXEME INTEGER) (?dest register)) (emit-instruction ldob ?src ?dest))
(defmethod MAIN::ldos ((?src LEXEME INTEGER) (?dest register)) (emit-instruction ldos ?src ?dest))
(defmethod MAIN::ldib ((?src LEXEME INTEGER) (?dest register)) (emit-instruction ldib ?src ?dest))
(defmethod MAIN::ldis ((?src LEXEME INTEGER) (?dest register)) (emit-instruction ldis ?src ?dest))
(defmethod MAIN::ldl ((?src LEXEME INTEGER) (?dest register)) (emit-instruction ldl ?src ?dest))
(defmethod MAIN::ldt ((?src LEXEME INTEGER) (?dest register)) (emit-instruction ldt ?src ?dest))
(defmethod MAIN::ldq ((?src LEXEME INTEGER) (?dest register)) (emit-instruction ldq ?src ?dest))
(defmethod MAIN::lda ((?src LEXEME INTEGER) (?dest register)) (emit-instruction lda ?src ?dest))
(defmethod MAIN::ldphy
  ((?src register)
   (?dest register))
  (emit-instruction ldphy
                    ?src
                    ?dest))
(defmethod MAIN::ldtime
  ((?dest register))
  (emit-instruction ldtime
                    ?dest))

(defmethod MAIN::logbnr ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction logbnr ?src ?dest))
(defmethod MAIN::logbnrl ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction logbnrl ?src ?dest))
(defmethod MAIN::logepr ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction logepr ?src ?dest))
(defmethod MAIN::logeprl ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction logeprl ?src ?dest))
(defmethod MAIN::logr ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction logr ?src ?dest))
(defmethod MAIN::logrl ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction logrl ?src ?dest))
(defmethod MAIN::mark () (emit-instruction mark))

(defmethod MAIN::modac
  ((?mask register
          literal)
   (?src register
         literal)
   (?dest register))
  (emit-instruction modac
                    ?mask
                    ?src
                    ?dest))
(defmethod MAIN::modpc
  ((?mask register
          literal)
   (?src register
         literal)
   (?dest register))
  (emit-instruction modpc
                    ?mask
                    ?src
                    ?dest))
(defmethod MAIN::modtc
  ((?mask register
          literal)
   (?src register
         literal)
   (?dest register))
  (emit-instruction modtc
                    ?mask
                    ?src
                    ?dest))
(defmethod MAIN::modi
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction modi
                    ?src1 
                    ?src2 
                    ?dest ))

(defmethod MAIN::modify*
  ((?mask register
          literal)
   (?src register
         literal)
   (?src/dest register))
  (emit-instruction modify
                    ?mask 
                    ?src 
                    ?src/dest ))

(defmethod MAIN::mov
  ((?src register
         literal)
   (?dest register))
  (emit-instruction mov
                    ?src
                    ?dest))
(defmethod MAIN::movl
  ((?src register
         literal)
   (?dest register))
  (emit-instruction movl
                    ?src
                    ?dest))
(defmethod MAIN::movt
  ((?src register
         literal)
   (?dest register))
  (emit-instruction movt
                    ?src
                    ?dest))
(defmethod MAIN::movq
  ((?src register
         literal)
   (?dest register))
  (emit-instruction movq
                    ?src
                    ?dest))

(defmethod MAIN::movqstr
  ((?dst register)
   (?src register)
   (?len register
         literal))
  (emit-instruction movqstr
                    ?dst
                    ?src
                    ?len))

(defmethod MAIN::movr
  ((?src register
         float-register
         float-literal)
   (?dest register
          float-register))
  (emit-instruction movr
                    ?src
                    ?dest))
(defmethod MAIN::movrl
  ((?src register
         float-register
         float-literal)
   (?dest register
          float-register))
  (emit-instruction movrl
                    ?src
                    ?dest))
(defmethod MAIN::movre
  ((?src register
         float-register
         float-literal)
   (?dest register
          float-register))
  (emit-instruction movre
                    ?src
                    ?dest))

(defmethod MAIN::movstr
  ((?dst register)
   (?src register)
   (?len register
         literal))
  (emit-instruction movstr
                    ?dst
                    ?src
                    ?len))
(defmethod MAIN::muli
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction muli
                    ?src1 
                    ?src2 
                    ?dest ))

(defmethod MAIN::mulo
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction mulo
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::mulr
  ((?src1 register
          float-register
          float-literal)
   (?src2 float-register
          register
          float-literal)
   (?dest register
          float-register))
  (emit-instruction mulr
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::mulrl
  ((?src1 register
          float-register
          float-literal)
   (?src2 float-register
          register
          float-literal)
   (?dest register
          float-register))
  (emit-instruction mulrl
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::nand
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction nand
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::nor
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction nor
                    ?src1 
                    ?src2 
                    ?dest ))

(defmethod MAIN::notand
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction notand
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::not
  ((?src register
         literal)
   (?dest register))
  (emit-instruction not
                    ?src
                    ?dest))
(defmethod MAIN::notbit
  ((?bitpos register
            literal)
   (?src register
         literal)
   (?dest register))
  (emit-instruction notbit
                    ?bitpos
                    ?src
                    ?dest))

(defmethod MAIN::notor
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction notor
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::ornot
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction ornot
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::or
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction or
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::receive
  ((?src register)
   (?dest register))
  (emit-instruction receive
                    ?src
                    ?dest))
(defmethod MAIN::remi
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction remi
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::remo
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction remo
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::remr
  ((?src1 float-register float-literal register)
   (?src2 float-register float-literal register)
   (?dest float-register register))
  (emit-instruction remr
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::remrl
  ((?src1 float-register float-literal register)
   (?src2 float-register float-literal register)
   (?dest float-register register))
  (emit-instruction remrl
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::resumprcs
  ((?src register))
  (emit-instruction resumprcs
                    ?src))

(defmethod MAIN::ret () (emit-instruction ret))
(defmethod MAIN::rotate
  ((?src1 register literal)
   (?src2 register literal)
   (?dest register))
  (emit-instruction rotate
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::roundr ((?src float-register float-literal register) (?dest float-register register)) (emit-instruction roundr ?src ?dest))
(defmethod MAIN::roundrl ((?src float-register float-literal register) (?dest float-register register)) (emit-instruction roundrl ?src ?dest))
(defmethod MAIN::saverprcs () saveprcs)

(defmethod MAIN::scaler
  ((?src1 float-register float-literal register)
   (?src2 float-register float-literal register)
   (?dest float-register register))
  (emit-instruction scaler
                    ?src1
                    ?src2
                    ?dest))
(defmethod MAIN::scalerl
  ((?src1 float-register float-literal register)
   (?src2 float-register float-literal register)
   (?dest float-register register))
  (emit-instruction scalerl
                    ?src1
                    ?src2
                    ?dest))

(defmethod MAIN::scanbit
  ((?src register literal)
   (?dest register))
  (emit-instruction scanbit
                    ?src
                    ?dest))

(defmethod MAIN::scanbyte
  ((?src1 register literal)
   (?src2 register literal))
  (emit-instruction scanbyte
                    ?src1
                    ?src2))
(defmethod MAIN::schedprcs
  ((?src register))
  (emit-instruction schedprcs
                    ?src))
(defmethod MAIN::send*
  ((?dest register)
   (?src1 register
          literal)
   (?src2 register))
  (emit-instruction send
                    ?dest
                    ?src1
                    ?src2))
(defmethod MAIN::sendserv
  ((?src register))
  (emit-instruction sendserv
                    ?src))
(defmethod MAIN::setbit
  ((?bitpos register
            literal)
   (?src register
         literal)
   (?dest register))
  (emit-instruction setbit
                    ?bitpos
                    ?src
                    ?dest))

(defmethod MAIN::shlo ((?len register literal) (?src register literal) (?dest register)) (emit-instruction shlo ?len ?src ?dest))
(defmethod MAIN::shro ((?len register literal) (?src register literal) (?dest register)) (emit-instruction shro ?len ?src ?dest))
(defmethod MAIN::shli ((?len register literal) (?src register literal) (?dest register)) (emit-instruction shli ?len ?src ?dest))
(defmethod MAIN::shri ((?len register literal) (?src register literal) (?dest register)) (emit-instruction shri ?len ?src ?dest))
(defmethod MAIN::shrdi ((?len register literal) (?src register literal) (?dest register)) (emit-instruction shrdi ?len ?src ?dest))

(defmethod MAIN::signal
  ((?dest register))
  (emit-instruction signal
                    ?dest))

(defmethod MAIN::sinr ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction sinr ?src ?dest))
(defmethod MAIN::sinrl ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction sinrl ?src ?dest))

(defmethod MAIN::spanbit
  ((?src register
         literal)
   (?dest register))
  (emit-instruction spanbit
                    ?src
                    ?dest))
(defmethod MAIN::sqrtr ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction sqrtr ?src ?dest))
(defmethod MAIN::sqrtrl ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction sqrtrl ?src ?dest))

(defmethod MAIN::st ((?src register) (?dest LEXEME INTEGER)) (emit-instruction st ?src ?dest))
(defmethod MAIN::stob ((?src register) (?dest LEXEME INTEGER)) (emit-instruction stob ?src ?dest))
(defmethod MAIN::stos ((?src register) (?dest LEXEME INTEGER)) (emit-instruction stos ?src ?dest))
(defmethod MAIN::stib ((?src register) (?dest LEXEME INTEGER)) (emit-instruction stib ?src ?dest))
(defmethod MAIN::stis ((?src register) (?dest LEXEME INTEGER)) (emit-instruction stis ?src ?dest))
(defmethod MAIN::stl ((?src register) (?dest LEXEME INTEGER)) (emit-instruction stl ?src ?dest))
(defmethod MAIN::stt ((?src register) (?dest LEXEME INTEGER)) (emit-instruction stt ?src ?dest))
(defmethod MAIN::stq ((?src register) (?dest LEXEME INTEGER)) (emit-instruction stq ?src ?dest))

(defmethod MAIN::subc
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction subc
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::subi
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction subi
                    ?src1 
                    ?src2 
                    ?dest ))
(defmethod MAIN::subo
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction subo
                    ?src1 
                    ?src2 
                    ?dest ))

(defmethod MAIN::subr ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction subr ?src ?dest))
(defmethod MAIN::subrl ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction subrl ?src ?dest))
(defmethod MAIN::syncf () (emit-instruction syncf))

(defmethod MAIN::synld ((?src register) (?dest register)) (emit-instruction synld ?src ?dest))
(defmethod MAIN::synmov ((?src register) (?dest register)) (emit-instruction synmov ?src ?dest))
(defmethod MAIN::synmovl ((?src register) (?dest register)) (emit-instruction synmovl ?src ?dest))
(defmethod MAIN::synmovq ((?src register) (?dest register)) (emit-instruction synmovq ?src ?dest))

(defmethod MAIN::tanr ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction tanr ?src ?dest))
(defmethod MAIN::tanrl ((?src register float-register float-literal) (?dest register float-register)) (emit-instruction tanrl ?src ?dest))

(defmethod MAIN::teste ((?dest register)) (emit-instruction teste ?dest))
(defmethod MAIN::testne ((?dest register)) (emit-instruction testne ?dest))
(defmethod MAIN::testl ((?dest register)) (emit-instruction testl ?dest))
(defmethod MAIN::testle ((?dest register)) (emit-instruction testle ?dest))
(defmethod MAIN::testg ((?dest register)) (emit-instruction testg ?dest))
(defmethod MAIN::testge ((?dest register)) (emit-instruction testge ?dest))
(defmethod MAIN::testo ((?dest register)) (emit-instruction testo ?dest))
(defmethod MAIN::testno ((?dest register)) (emit-instruction testno ?dest))
(defmethod MAIN::wait
  ((?src register))
  (emit-instruction wait
                    ?src))


(defmethod MAIN::xor
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction xor
                    ?src1 
                    ?src2 
                    ?dest ))

(defmethod MAIN::xnor
  ((?src1 register
          literal)
   (?src2 register
          literal)
   (?dest register))
  (emit-instruction xnor
                    ?src1 
                    ?src2 
                    ?dest ))


(defmethod MAIN::.label
  ((?name SYMBOL))
  (format nil "%s:" ?name))

(defmethod MAIN::.word
  ((?value NUMBER))
  (format nil ".word %d" ?value))
(defmethod MAIN::.word
  ((?value LEXEME))
  (format nil ".word %s" ?value))
(defmethod MAIN::make-scope
  ((?items MULTIFIELD))
  ?items)
(defmethod MAIN::make-scope
  ($?items)
  (make-scope ?items))

(defmethod MAIN::named-scope
  ((?name SYMBOL)
   (?body MULTIFIELD))
  (make-scope (.label ?name)
              ?body))
(defmethod MAIN::named-scope
  ((?name SYMBOL)
   $?body)
  (named-scope ?name
               ?body))
(deffunction MAIN::generate-funcall-collection
             (?function $?items)
             (bind ?output
                   (create$))
             (progn$ (?item ?items)
                     (bind ?output
                           ?output
                           (funcall ?function
                                    ?item)))
             ?output)
(defmethod MAIN::.word
  ((?items MULTIFIELD))
  (generate-funcall-collection .word
                               ?items))

(defmethod MAIN::.word
  ($?items)
  (.word ?items))


(defmethod MAIN::.global
  ((?title SYMBOL))
  (emit-instruction .global
                    ?title))
(defmethod MAIN::.global
  ((?items MULTIFIELD))
  (generate-funcall-collection .global
                               ?items))
(defmethod MAIN::.global
  ($?items)
  (.global ?items))

(defmethod MAIN::.space
  ((?capacity LEXEME
              INTEGER))
  (emit-instruction .space 
                    ?capacity))
(defmethod MAIN::.space
  ((?capacity LEXEME
              INTEGER)
   (?fill LEXEME
          INTEGER))
  (emit-instruction .space
                    ?capacity
                    ?fill))

(defmethod MAIN::.ascii
  ((?string LEXEME))
  (format nil ".ascii \"%s\"" ?string))
(defmethod MAIN::.asciz
  ((?string LEXEME))
  (format nil ".asciz \"%s\"" ?string))

(defmethod MAIN::.declare
  ((?name SYMBOL)
   (?value LEXEME
           NUMBER))
  (format nil "%s = %s"
          ?name
          (send ?value to-string)))

(defmethod MAIN::.text 
  () 
  (emit-instruction .text))
(defmethod MAIN::.text 
  ((?subsection INTEGER)) 
  (emit-instruction .text 
                    ?subsection))
(defmethod MAIN::.text 
  ((?subsection INTEGER)
   (?contents MULTIFIELD)) 
  (make-scope (.text ?subsection) 
              ?contents))
(defmethod MAIN::.text 
  ((?contents MULTIFIELD)) 
  (make-scope (.text) 
              ?contents))
(defmethod MAIN::.text 
  ($?contents)
  (.text ?contents))
(defmethod MAIN::.text 
  ((?subsection INTEGER)
   $?contents)
  (.text ?subsection
         ?contents))
(defmethod MAIN::.data 
  () 
  (emit-instruction .data))
(defmethod MAIN::.data 
  ((?subsection INTEGER)) 
  (emit-instruction .data 
                    ?subsection))
(defmethod MAIN::.data 
  ((?subsection INTEGER)
   (?contents MULTIFIELD)) 
  (make-scope (.data ?subsection) 
              ?contents))
(defmethod MAIN::.data 
  ((?contents MULTIFIELD)) 
  (make-scope (.data) 
              ?contents))
(defmethod MAIN::.data 
  ($?contents)
  (.data ?contents))
(defmethod MAIN::.data 
  ((?subsection INTEGER)
   $?contents)
  (.data ?subsection
         ?contents))

(defmethod MAIN::.bss
  ((?symbol LEXEME)
   (?length LEXEME
            INTEGER)
   (?align INTEGER))
  (emit-instruction .bss
                    ?symbol
                    ?length
                    ?align))

(defmethod MAIN::.directive
  "Emit a custom directive with zero type checking"
  ((?name SYMBOL)
   (?body MULTIFIELD))
  (emit-instruction ?name
                    ?body))
(defmethod MAIN::.directive
  ((?name SYMBOL)
   $?body)
  (.directive ?name 
              ?body))

(defmethod MAIN::ldconst
  ((?value INTEGER
           LEXEME)
   (?dest register))
  (emit-instruction ldconst
                    ?value
                    ?dest))
; ---- layered operations ----


(defmethod MAIN::defprocedure
  ((?name SYMBOL)
   (?body MULTIFIELD))
  (named-scope ?name
               ?body
               (ret)))
(defmethod MAIN::defprocedure
  ((?name SYMBOL)
   $?body)
  (defprocedure ?name
                ?body))
(defmethod MAIN::mov
  ((?src SYMBOL
         (eq ?current-argument
             ac))
   (?dest register))
  (modac [lit0]
         [lit0]
         ?dest))

   
(defmethod MAIN::mov
  ((?src SYMBOL
         (eq ?current-argument
             tc))
   (?dest register))
  (modtc [lit0]
         [lit0]
         ?dest))

(defmethod MAIN::mov
  ((?src SYMBOL
         (eq ?current-argument
             pc))
   (?dest register))
  (modpc [lit0]
         [lit0]
         ?dest))

(defmethod MAIN::mov
  ((?src register)
   (?dest SYMBOL
          (eq ?current-argument
              ac)))
  (modac ?src
         ?src
         ?src))

(defmethod MAIN::mov
  ((?src register)
   (?dest SYMBOL
          (eq ?current-argument
              tc)))
  (modtc ?src
         ?src
         ?src))
(defmethod MAIN::mov
  ((?src register)
   (?dest SYMBOL
          (eq ?current-argument
              pc)))
  (modpc ?src
         ?src
         ?src))

(defmethod MAIN::save-globals
  ((?temporary register))
  (make-scope (ldconst 64 
                       ?temporary)
              (addo [sp]
                    ?temporary
                    [sp])
              (stq [g0] "-64(sp)")
              (stq [g4] "-48(sp)")
              (stq [g8] "-32(sp)")
              (stt [g12] "-16(sp)")))
(defmethod MAIN::restore-globals
  ()
  (make-scope (ldq "-64(sp)" [g0])
              (ldq "-48(sp)" [g4])
              (ldq "-32(sp)" [g8])
              (ldt "-16(sp)" [g12])))
(defmethod MAIN::preserve-globals
  ((?temporary register)
   (?body MULTIFIELD))
  (make-scope (save-globals ?temporary)
              ?body
              (restore-globals)))
(defmethod MAIN::preserve-globals
  ((?temporary register)
   $?body)
  (preserve-globals ?temporary
                    ?body))


(defmethod MAIN::definterrupt-vector
    ((?name SYMBOL)
     (?fn SYMBOL))
    (make-scope (.global ?name)
                (defprocedure ?name
                              (preserve-globals [r4]
                                                (ldconst 0 [g14])
                                                (call ?fn)))))
