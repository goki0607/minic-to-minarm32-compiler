// [NYU Courant Institute] Compiler Construction/Fall 2016/Project Milestone 3 -*-hacs-*-
//
// Contents.
// 1. MiniC Lexical analysis and grammar
// 2. MinARM32 assembler grammar
// 3. Compiler from MiniC to MinARM32
//
// Refer to documentation in \url{http://cs.nyu.edu/courses/fall16/CSCI-GA.2130-001/}.

module edu.nyu.cs.cc.Pr3GoktugKevin {

  
  ////////////////////////////////////////////////////////////////////////
  // 1. MiniC LEXICAL ANALYSIS AND GRAMMAR
  ////////////////////////////////////////////////////////////////////////

  // pr1: refers to http://cs.nyu.edu/courses/fall16/CSCI-GA.2130-001/project1/pr1.pdf

  // TOKENS (pr1:1.1).

  space [ \t\n\r] | '//' [^\n]* | '/*' ( [^*] | '*' [^/] )* '*/'  ; // Inner /* ignored

  token ID    | ⟨LetterEtc⟩ (⟨LetterEtc⟩ | ⟨Digit⟩)* ;
  token INT | ⟨Digit⟩+ ;
  token STR | "\"" ( [^\"\\\n] | \\ ⟨Escape⟩ )* "\"";

  token fragment Letter     | [A-Za-z] ;
  token fragment LetterEtc  | ⟨Letter⟩ | [$_] ;
  token fragment Digit      | [0-9] ;
  
  token fragment Escape  | [\n\\nt"] | "x" ⟨Hex⟩ ⟨Hex⟩ | ⟨Octal⟩;
  token fragment Hex     | [0-9A-Fa-f] ;
  token fragment Octal   | [0-7] | [0-7][0-7] | [0-7][0-7][0-7];
 
  // PROGRAM (pr1:2.6)

  main sort Program  |  ⟦ ⟨Declarations⟩ ⟧ ;

  // DECLARATIONS (pr1:1.5)

  sort Declarations | ⟦ ⟨Declaration⟩ ⟨Declarations⟩ ⟧ | ⟦⟧ ;

  sort Declaration
    |  ⟦ function ⟨Type⟩ ⟨Identifier⟩ ⟨ArgumentSignature⟩ { ⟨Statements⟩ } ⟧
    ;

  sort ArgumentSignature
    |  ⟦ ( ) ⟧
    |  ⟦ ( ⟨Type⟩ ⟨Identifier⟩ ⟨TypeIdentifierTail⟩ ) ⟧
    ;
  sort TypeIdentifierTail |  ⟦ , ⟨Type⟩ ⟨Identifier⟩ ⟨TypeIdentifierTail⟩ ⟧  |  ⟦ ⟧ ;

  // STATEMENTS (pr1:1.4)

  sort Statements | ⟦ ⟨Statement⟩ ⟨Statements⟩ ⟧ | ⟦⟧ ;

  sort Statement
    |  ⟦ { ⟨Statements⟩ } ⟧
    |  ⟦ var ⟨Type⟩ ⟨Identifier⟩ ; ⟧
    |  ⟦ ⟨Expression⟩ = ⟨Expression⟩ ; ⟧
    |  ⟦ if ( ⟨Expression⟩ ) ⟨IfTail⟩ ⟧
    |  ⟦ while ( ⟨Expression⟩ ) ⟨Statement⟩ ⟧
    |  ⟦ return ⟨Expression⟩ ; ⟧
    ;

  sort IfTail | ⟦ ⟨Statement⟩ else ⟨Statement⟩ ⟧ | ⟦ ⟨Statement⟩ ⟧ ;

  // TYPES (pr1:1.3)

  sort Type
    |  ⟦ int ⟧@3
    |  ⟦ char ⟧@3
    |  ⟦ ( ⟨Type⟩ )⟧@3
    |  ⟦ ⟨Type@2⟩ ( ⟨TypeList⟩ )⟧@2
    |  ⟦ * ⟨Type@1⟩ ⟧@1
    ;
    
  sort TypeList | ⟦ ⟨Type⟩ ⟨TypeListTail⟩ ⟧ | ⟦⟧;
  sort TypeListTail | ⟦ , ⟨Type⟩ ⟨TypeListTail⟩ ⟧ | ⟦⟧;  

  // EXPRESSIONS (pr1:2.2)

  sort Expression

    |  sugar ⟦ ( ⟨Expression#e⟩ ) ⟧@10 → #e

    |  ⟦ ⟨Integer⟩ ⟧@10
    |  ⟦ ⟨String⟩ ⟧@10
    |  ⟦ ⟨Identifier⟩ ⟧@10

    |  ⟦ ⟨Expression@9⟩ ( ⟨ExpressionList⟩ ) ⟧@9
    |  ⟦ null ( ⟨Type⟩ ) ⟧@9
    |  ⟦ sizeof ( ⟨Type⟩ )⟧@9

    |  ⟦ ! ⟨Expression@8⟩ ⟧@8
    |  ⟦ - ⟨Expression@8⟩ ⟧@8
    |  ⟦ + ⟨Expression@8⟩ ⟧@8
    |  ⟦ * ⟨Expression@8⟩ ⟧@8
    |  ⟦ & ⟨Expression@8⟩ ⟧@8

    |  ⟦ ⟨Expression@7⟩ * ⟨Expression@8⟩ ⟧@7

    |  ⟦ ⟨Expression@6⟩ + ⟨Expression@7⟩ ⟧@6
    |  ⟦ ⟨Expression@6⟩ - ⟨Expression@7⟩ ⟧@6

    |  ⟦ ⟨Expression@6⟩ < ⟨Expression@6⟩ ⟧@5
    |  ⟦ ⟨Expression@6⟩ > ⟨Expression@6⟩ ⟧@5
    |  ⟦ ⟨Expression@6⟩ <= ⟨Expression@6⟩ ⟧@5
    |  ⟦ ⟨Expression@6⟩ >= ⟨Expression@6⟩ ⟧@5

    |  ⟦ ⟨Expression@5⟩ == ⟨Expression@5⟩ ⟧@4
    |  ⟦ ⟨Expression@5⟩ != ⟨Expression@5⟩ ⟧@4

    |  ⟦ ⟨Expression@3⟩ && ⟨Expression@4⟩ ⟧@3

    |  ⟦ ⟨Expression@2⟩ || ⟨Expression@3⟩ ⟧@2
    ;
    
  // Helper to describe actual list of arguments of function call.
  sort ExpressionList | ⟦ ⟨Expression⟩ ⟨ExpressionListTail⟩ ⟧  |  ⟦⟧ ;
  sort ExpressionListTail | ⟦ , ⟨Expression⟩ ⟨ExpressionListTail⟩ ⟧  |  ⟦⟧ ;  

  sort Integer    | ⟦ ⟨INT⟩ ⟧ ;
  sort String   | ⟦ ⟨STR⟩ ⟧ ;
  sort Identifier | symbol ⟦⟨ID⟩⟧ ;

 
  ////////////////////////////////////////////////////////////////////////
  // 2. MinARM32 ASSEMBLER GRAMMAR
  ////////////////////////////////////////////////////////////////////////

  // arm: refers to http://cs.nyu.edu/courses/fall14/CSCI-GA.2130-001/pr3/MinARM32.pdf
 
  // Instructions.
  sort Instructions | ⟦ ⟨Instruction⟩ ⟨Instructions⟩ ⟧ | ⟦⟧ ;

  // Directives (arm:2.1)
  sort Instruction
    | ⟦DEF ⟨ID⟩ = ⟨Integer⟩ ¶⟧  // define identifier
    | ⟦¶⟨Label⟩ ⟧               // define address label
    | ⟦DCI ⟨Integers⟩ ¶⟧        // allocate integers
    | ⟦DCS ⟨String⟩ ¶⟧          // allocate strings
    | ⟦⟨Op⟩ ¶⟧                  // machine instruction
    ;

  sort Integers | ⟦ ⟨Integer⟩, ⟨Integers⟩ ⟧ | ⟦ ⟨Integer⟩ ⟧ ;

  sort Label | symbol ⟦⟨ID⟩⟧ ;
 
  // Syntax of individual machine instructions (arm:2.2).
  sort Op

    | ⟦MOV ⟨Reg⟩, ⟨Arg⟩ ⟧   // move
    | ⟦MVN ⟨Reg⟩, ⟨Arg⟩ ⟧   // move not
    | ⟦ADD ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧  // add
    | ⟦SUB ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧  // subtract
    | ⟦RSB ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧  // reverse subtract
    | ⟦AND ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧  // bitwise and
    | ⟦ORR ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧  // bitwise or
    | ⟦EOR ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧  // bitwise exclusive or
    | ⟦CMP ⟨Reg⟩, ⟨Arg⟩ ⟧       // compare
    | ⟦MUL ⟨Reg⟩, ⟨Reg⟩, ⟨Reg⟩ ⟧  // multiply

    | ⟦B ⟨Label⟩ ⟧      // branch always
    | ⟦BEQ ⟨Label⟩ ⟧      // branch if equal
    | ⟦BNE ⟨Label⟩ ⟧      // branch if not equal
    | ⟦BGT ⟨Label⟩ ⟧      // branch if greater than
    | ⟦BLT ⟨Label⟩ ⟧      // branch if less than
    | ⟦BGE ⟨Label⟩ ⟧      // branch if greater than or equal
    | ⟦BLE ⟨Label⟩ ⟧      // branch if less than or equal
    | ⟦BL ⟨Label⟩ ⟧     // branch and link

    | ⟦LDR ⟨Reg⟩, ⟨Mem⟩ ⟧   // load register from memory
    | ⟦STR ⟨Reg⟩, ⟨Mem⟩ ⟧   // store register to memory

    | ⟦LDMFD ⟨Reg⟩! , {⟨Regs⟩} ⟧  // load multiple fully descending (pop)
    | ⟦STMFD ⟨Reg⟩! , {⟨Regs⟩} ⟧  // store multiple fully descending (push)
    ;

  // Arguments.

  sort Reg  | ⟦R0⟧ | ⟦R1⟧ | ⟦R2⟧ | ⟦R3⟧ | ⟦R4⟧ | ⟦R5⟧ | ⟦R6⟧ | ⟦R7⟧
    | ⟦R8⟧ | ⟦R9⟧ | ⟦R10⟧ | ⟦R11⟧ | ⟦R12⟧ | ⟦SP⟧ | ⟦LR⟧ | ⟦PC⟧ ;

  sort Arg | ⟦⟨Constant⟩⟧ | ⟦⟨Reg⟩⟧ | ⟦⟨Reg⟩, LSL ⟨Constant⟩⟧ | ⟦⟨Reg⟩, LSR ⟨Constant⟩⟧ ;

  sort Mem | ⟦[⟨Reg⟩, ⟨Sign⟩⟨Arg⟩]⟧ ;
  sort Sign | ⟦+⟧ | ⟦-⟧ | ⟦⟧ ;

  sort Regs | ⟦⟨Reg⟩⟧ | ⟦⟨Reg⟩-⟨Reg⟩⟧ | ⟦⟨Reg⟩, ⟨Regs⟩⟧ | ⟦⟨Reg⟩-⟨Reg⟩, ⟨Regs⟩⟧ ;

  sort Constant | ⟦#⟨Integer⟩⟧ | ⟦&⟨ID⟩⟧ | ⟦&⟨Label⟩⟧;

  // Helper concatenation/flattening of Instructions.
  sort Instructions | scheme ⟦ { ⟨Instructions⟩ } ⟨Instructions⟩ ⟧ ;
  ⟦ {} ⟨Instructions#⟩ ⟧ → # ;
  ⟦ {⟨Instruction#1⟩ ⟨Instructions#2⟩} ⟨Instructions#3⟩ ⟧
    → ⟦ ⟨Instruction#1⟩ {⟨Instructions#2⟩} ⟨Instructions#3⟩ ⟧ ;

  // Helper data structure for list of registers.
  sort Rs | NoRs | MoRs(Reg, Rs) | scheme AppendRs(Rs, Rs) ;
  AppendRs(NoRs, #Rs) → #Rs ;
  AppendRs(MoRs(#Rn, #Rs1), #Rs2) → MoRs(#Rn, AppendRs(#Rs1, #Rs2)) ;

  // Helper conversion from Regs syntax to register list.
  | scheme XRegs(Regs) ;
  XRegs(⟦⟨Reg#r⟩⟧) → MoRs(#r, NoRs) ;
  XRegs(⟦⟨Reg#r1⟩-⟨Reg#r2⟩⟧) → XRegs1(#r1, #r2) ;
  XRegs(⟦⟨Reg#r⟩, ⟨Regs#Rs⟩⟧) → MoRs(#r, XRegs(#Rs)) ;
  XRegs(⟦⟨Reg#r1⟩-⟨Reg#r2⟩, ⟨Regs#Rs⟩⟧) → AppendRs(XRegs1(#r1, #r2), XRegs(#Rs)) ;

  | scheme XRegs1(Reg, Reg) ;
  XRegs1(#r, #r) → MoRs(#r, NoRs) ;
  default XRegs1(#r1, #r2) → XRegs2(#r1, #r2) ;

  | scheme XRegs2(Reg, Reg) ;
  XRegs2(⟦R0⟧, #r2) → MoRs(⟦R0⟧, XRegs1(⟦R1⟧, #r2)) ;
  XRegs2(⟦R1⟧, #r2) → MoRs(⟦R1⟧, XRegs1(⟦R2⟧, #r2)) ;
  XRegs2(⟦R2⟧, #r2) → MoRs(⟦R2⟧, XRegs1(⟦R3⟧, #r2)) ;
  XRegs2(⟦R3⟧, #r2) → MoRs(⟦R3⟧, XRegs1(⟦R4⟧, #r2)) ;
  XRegs2(⟦R4⟧, #r2) → MoRs(⟦R4⟧, XRegs1(⟦R5⟧, #r2)) ;
  XRegs2(⟦R5⟧, #r2) → MoRs(⟦R5⟧, XRegs1(⟦R6⟧, #r2)) ;
  XRegs2(⟦R6⟧, #r2) → MoRs(⟦R6⟧, XRegs1(⟦R7⟧, #r2)) ;
  XRegs2(⟦R7⟧, #r2) → MoRs(⟦R7⟧, XRegs1(⟦R8⟧, #r2)) ;
  XRegs2(⟦R8⟧, #r2) → MoRs(⟦R8⟧, XRegs1(⟦R9⟧, #r2)) ;
  XRegs2(⟦R9⟧, #r2) → MoRs(⟦R9⟧, XRegs1(⟦R10⟧, #r2)) ;
  XRegs2(⟦R10⟧, #r2) → MoRs(⟦R10⟧, XRegs1(⟦R11⟧, #r2)) ;
  XRegs2(⟦R11⟧, #r2) → MoRs(⟦R11⟧, XRegs1(⟦R12⟧, #r2)) ;
  XRegs2(⟦R12⟧, #r2) → MoRs(⟦R12⟧, NoRs) ;
  XRegs1(⟦SP⟧, #r2) → error⟦MinARM32 error: Cannot use SP in Regs range.⟧ ;
  XRegs1(⟦LR⟧, #r2) → error⟦MinARM32 error: Cannot use LR in Regs range.⟧ ;
  XRegs1(⟦PC⟧, #r2) → error⟦MinARM32 error: Cannot use PC in Regs range.⟧ ;
  
  // Helpers to insert computed assembly constants.
  sort Constant | scheme Immediate(Computed) | scheme Reference(Computed) ;
  Immediate(#x) → ⟦#⟨INT#x⟩⟧ ;
  Reference(#id) → ⟦&⟨ID#id⟩⟧ ;

  sort Mem | scheme FrameAccess(Computed) ;
  FrameAccess(#x)
    → FrameAccess1(#x, ⟦ [R12, ⟨Constant Immediate(#x)⟩] ⟧, ⟦ [R12, -⟨Constant Immediate(⟦0-#x⟧)⟩] ⟧) ;
  | scheme FrameAccess1(Computed, Mem, Mem) ;
  FrameAccess1(#x, #pos, #neg) → FrameAccess2(⟦ #x ≥ 0 ? #pos : #neg ⟧) ;
  | scheme FrameAccess2(Computed) ;
  FrameAccess2(#mem) → #mem ;

  sort Instruction | scheme AddConstant(Reg, Reg, Computed) ;
  AddConstant(#Rd, #Rn, #x)
    → AddConstant1(#x,
       ⟦ ADD ⟨Reg#Rd⟩, ⟨Reg#Rn⟩, ⟨Constant Immediate(#x)⟩ ⟧,
       ⟦ SUB ⟨Reg#Rd⟩, ⟨Reg#Rn⟩, ⟨Constant Immediate(⟦0-#x⟧)⟩ ⟧) ;
  | scheme AddConstant1(Computed, Instruction, Instruction) ;
  AddConstant1(#x, #pos, #neg) → AddConstant2(⟦ #x ≥ 0 ? #pos : #neg ⟧) ;
  | scheme AddConstant2(Computed) ;
  AddConstant2(#add) → #add ;
  

  ////////////////////////////////////////////////////////////////////////
  // 3. COMPILER FROM MiniC TO MinARM32
  ////////////////////////////////////////////////////////////////////////

  // HACS doesn't like to compile with Computed sort
  // unless there exists a scheme that can generate Computed
  sort Computed | scheme Dummy ;
  Dummy → ⟦ 0 ⟧;

  // MAIN SCHEME

  sort Instructions  |  scheme Compile(Program) ;
  Compile(#1) → P2(P1(#1), #1) ;

  // PASS 1

  // Result sort for first pass, with join operation.
  sort After1 | Data1(Instructions, FT) | scheme Join1(After1, After1) ;
  Join1(Data1(#1, #ft1), Data1(#2, #ft2))
    → Data1(⟦ { ⟨Instructions#1⟩ } ⟨Instructions#2⟩ ⟧, AppendFT(#ft1, #ft2)) ;

  // Function to return type environment (list of pairs with append).
  sort FT | NoFT | MoFT(Identifier, Type, FT) | scheme AppendFT(FT, FT) ;
  AppendFT(NoFT, #ft2) → #ft2 ;
  AppendFT(MoFT(#id1, #T1, #ft1), #ft2) → MoFT(#id1, #T1, AppendFT(#ft1, #ft2)) ;

  // Pass 1 recursion.
  sort After1 | scheme P1(Program) ;
  P1(⟦⟨Declarations#Ds⟩⟧) → P1Ds(#Ds) ;

  sort After1 | scheme P1Ds(Declarations);  // Def. \ref{def:P}.
  P1Ds(⟦⟨Declaration#D⟩ ⟨Declarations#Ds⟩⟧) → Join1(D(#D), P1Ds(#Ds)) ;
  P1Ds(⟦⟧) → Data1(⟦⟧, NoFT) ;

  // \sem{D} scheme (Def. \ref{def:D}).
  
  sort After1 | scheme D(Declaration) ;
  D(⟦ function ⟨Type#T⟩ f ⟨ArgumentSignature#As⟩ { ⟨Statements#S⟩ } ⟧)
    → Data1(⟦⟧, MoFT(⟦f⟧, #T, NoFT)) ;

  // PASS 2

  // Pass 2 strategy: first load type environment $ρ$ then tail-call recursion.
  sort Instructions | scheme P2(After1, Program) ;
  P2(Data1(#1, #ft1), #P) → P2Load(#1, #ft1, #P) ;

  // Type environment ($ρ$) is split in two components (by used sorts).
  attribute ↓ft{Identifier : Type} ;  // map from function name to return type
  attribute ↓vt{Identifier : Local} ; // map from local variable name to type\&location
  attribute ↓vt2{Identifier : Type} ; // map from local pointer variable name to just type
  sort Local | RegLocal(Type, Reg) | FrameLocal(Type, Computed) ;  // type\&location

  // Other inherited attributes.
  attribute ↓return(Label) ;    // label of return code
  attribute ↓true(Label) ;    // label to jump for true result
  attribute ↓false(Label) ;   // label to jump for false result
  attribute ↓value(Reg) ;   // register for expression result
  attribute ↓offset(Computed) ;   // frame offset for first unused local
  attribute ↓unused(Rs) ;   // list of unused registers
  attribute ↓next(Label) ;   // label for statement after current statement
  attribute ↓num(Label) ;   // label for DCI and DCS

  // Pass 2 Loader: extract type environment $ρ$ and emit pass 1 directives.
  sort Instructions | scheme P2Load(Instructions, FT, Program) ↓ft ↓vt ↓vt2;
  P2Load(#is, MoFT(⟦f⟧, #T, #ft), #P) → P2Load(#is, #ft, #P) ↓ft{⟦f⟧ : #T} ;
                                                            
  P2Load(#is, NoFT, #P) → ⟦ { ⟨Instructions#is⟩ } ⟨Instructions P(#P)⟩ ⟧ ;

  // Pass 2 recursion.
  sort Instructions | scheme P(Program) ↓ft ↓vt ↓vt2;
  P(⟦ ⟨Declarations#Ds⟩ ⟧) → Ds(#Ds) ;

  sort Instructions | scheme Ds(Declarations) ↓ft ↓vt ↓vt2;
  Ds(⟦ ⟨Declaration#D⟩ ⟨Declarations#Ds⟩ ⟧) → ⟦ { ⟨Instructions F(#D)⟩ } ⟨Instructions Ds(#Ds)⟩ ⟧ ;
  Ds(⟦⟧) → ⟦⟧ ;

  // \sem{F} scheme (Def. \ref{def:F}), with argument signature iteration helpers.
  
  sort Instructions | scheme F(Declaration) ↓ft ↓vt ↓vt2;
  F(⟦ function ⟨Type#T⟩ f ⟨ArgumentSignature#AS⟩ { ⟨Statements#S⟩ } ⟧) → ⟦
  f STMFD SP!, {R4-R11,LR}
    MOV R12, SP
    { ⟨Instructions AS(#AS, XRegs(⟦R0-R3⟧), #S) ↓return(⟦L⟧)⟩ }
  L MOV SP, R12
    LDMFD SP!, {R4-R11,PC}
  ⟧ ;
  
  sort Instructions | scheme AS(ArgumentSignature, Rs, Statements) ↓ft ↓vt ↓vt2 ↓return ;
  AS(⟦ () ⟧, #Rs, #S) → S(#S) ↓offset(⟦0-4⟧) ↓unused(XRegs(⟦R4-R11⟧)) ↓next(⟦⟨Label ⟦end⟧⟩⟧);
  AS(⟦ ( *⟨Type #T⟩ a ⟨TypeIdentifierTail#TIT⟩ ) ⟧, MoRs(#r, #Rs), #S)
    → AS2(#TIT, #Rs, #S) ↓vt{⟦a⟧ : RegLocal(#T, #r)} ↓vt2{⟦a⟧ : #T} ;
  AS(⟦ ( ⟨Type#T⟩ a ⟨TypeIdentifierTail#TIT⟩ ) ⟧, MoRs(#r, #Rs), #S)
    → AS2(#TIT, #Rs, #S) ↓vt{⟦a⟧ : RegLocal(#T, #r)} ;

  sort Instructions | scheme AS2(TypeIdentifierTail, Rs, Statements) ↓ft ↓vt ↓vt2 ↓return ;
  AS2(⟦ ⟧, #Rs, #S) → S(#S) ↓offset(⟦0-4⟧) ↓unused(XRegs(⟦R4-R11⟧)) ↓next(⟦⟨Label ⟦end⟧⟩⟧);
  AS2(⟦ , *⟨Type #T⟩ a ⟨TypeIdentifierTail#TIT⟩ ⟧, MoRs(#r, #Rs), #S)
    → AS2(#TIT, #Rs, #S) ↓vt{⟦a⟧ : RegLocal(#T, #r)} ↓vt2{⟦a⟧ : #T} ;
  AS2(⟦ , ⟨Type#T⟩ a ⟨TypeIdentifierTail#TIT⟩ ⟧, MoRs(#r, #Rs), #S)
    → AS2(#TIT, #Rs, #S) ↓vt{⟦a⟧ : RegLocal(#T, #r)} ;
  AS2(⟦ , ⟨Type#T⟩ a ⟨TypeIdentifierTail#TIT⟩ ⟧, NoRs, #S)
    → error⟦More than four arguments to function not allowed.⟧ ;

  // TODO: REMAINING CODE GENERATION.

  sort Computed | scheme Concat(Computed, Computed);
  Concat(#1, #2) → ⟦ #1@#2 ⟧;

  sort Computed | scheme Decr(Computed) ;
  Decr(#o) → ⟦ #o + 4 ⟧;

  sort Computed | scheme LToI(Label);
  LToI(⟦⟨Label #1⟩⟧) → #1;  // ⟦⟨Identifier #1⟩⟧;
  LToI(#) → error⟦ whoops at L to I⟧ ;

  sort Instructions | scheme Str(Local, Reg) | scheme StrR(Reg, Reg);
  Str(RegLocal(#t, #r1), #r2) →
    ⟦ MOV ⟨Reg#r1⟩, ⟨Reg#r2⟩ ⟧;
  Str(FrameLocal(#t, #addr), #r) →
    ⟦ STR ⟨Reg#r⟩, ⟨Mem FrameAccess(#addr)⟩ ⟧;
  StrR(#r1, #r2) →
    ⟦ STR ⟨Reg#r1⟩, [R12, ⟨Reg#r2⟩] ⟧;

  sort Instructions | scheme Ldr(Local, Reg);
  Ldr(RegLocal(#t, #r1), #r2) →
    ⟦ MOV ⟨Reg#r2⟩, ⟨Reg#r1⟩ ⟧;
  Ldr(FrameLocal(#t, #r1), #r2) →
    ⟦ LDR ⟨Reg#r2⟩, ⟨Mem FrameAccess(#r1)⟩ ⟧;

  sort Instructions | scheme S(Statements) ↓ft ↓vt ↓vt2 ↓return ↓unused ↓offset ↓next;
  S(⟦ { ⟨Statements#1⟩ } ⟨Statements#2⟩ ⟧) →
    ⟦
      { ⟨Instructions S(#1)↓next(⟦s2⟧)⟩ }
      ⟨Label ⟦s2⟧⟩
      { ⟨Instructions S(#2)⟩ }
    ⟧;
  S(⟦ var *⟨Type#1⟩ ⟨Identifier#2⟩ ; ⟨Statements#3⟩ ⟧)↓vt{:#v}↓vt2{:#v2}↓offset(#o) →
    ⟦ 
      { ⟨Instructions S(#3) ↓offset(Decr(#o))↓vt{:#v}↓vt{#2:FrameLocal(⟦⟨Type ⟦ * ⟨Type#1⟩⟧⟩⟧, #o)}↓vt2{:#v2}↓vt2{#2:⟦⟨Type ⟦ * ⟨Type#1⟩⟧⟩⟧}⟩ }
    ⟧;
  S(⟦ var ⟨Type#1⟩ ⟨Identifier#2⟩ ; ⟨Statements#3⟩ ⟧)↓vt{:#v}↓offset(#o) →
    ⟦ 
      { ⟨Instructions S(#3) ↓offset(Decr(#o))↓vt{:#v}↓vt{#2:FrameLocal(#1, #o)}⟩ }
    ⟧;
  S(⟦ ⟨Expression#1⟩ = ⟨Expression#2⟩ ; ⟨Statements#3⟩ ⟧) →
    ⟦ 
      { ⟨Instructions AE(#1,#2)⟩ }
      { ⟨Instructions S(#3)⟩ }
    ⟧;
  S(⟦ if ( ⟨Expression#1⟩ ) ⟨IfTail#2⟩ ⟨Statements#3⟩ ⟧) →
    ⟦
      { ⟨Instructions ECond(#1)↓true(⟦true⟧)↓false(⟦false⟧)↓num(⟦dc⟧)⟩ }
      { ⟨Instructions IT(#2)↓true(⟦true⟧)↓false(⟦false⟧)↓next(⟦after⟧)⟩ }
      ⟨Label ⟦after⟧⟩
      { ⟨Instructions S(#3)⟩ }
    ⟧;
  S(⟦ while ( ⟨Expression#1⟩ ) ⟨Statement#2⟩ ⟨Statements#3⟩ ⟧) →
    ⟦
      ⟨Label ⟦repeat⟧⟩
      { ⟨Instructions ECond(#1)↓true(⟦body⟧)↓false(⟦after⟧)↓num(⟦dc⟧)⟩ }
      ⟨Label ⟦body⟧⟩
      { ⟨Instructions SSingle(#2)↓next(⟦repeat⟧)⟩ }
      ⟨Label ⟦after⟧⟩
      { ⟨Instructions S(#3)⟩ }
    ⟧;
  S(⟦ return ⟨Expression#1⟩ ; ⟨Statements#2⟩ ⟧)↓return(#r) →
    ⟦
      { ⟨Instructions RE(#1)⟩ }
      B ⟨Label #r⟩
      { ⟨Instructions S(#2)⟩ }
    ⟧;
  S(⟦ ⟧) → 
    ⟦ ⟧;

  sort Instructions | scheme SSingle(Statement) ↓ft ↓vt ↓vt2 ↓return ↓unused ↓offset ↓next;
  SSingle(⟦ { ⟨Statements#1⟩ } ⟧) →
    ⟦
      { ⟨Instructions S(#1)⟩ }
    ⟧;
  SSingle(⟦ var ⟨Type#1⟩ ⟨Identifier#2⟩ ; ⟧) →
    ⟦ ⟧;
  SSingle(⟦ ⟨Expression#1⟩ = ⟨Expression#2⟩ ; ⟧) →
    ⟦ 
      { ⟨Instructions AE(#1,#2)⟩ }
    ⟧;
  SSingle(⟦ if ( ⟨Expression#1⟩ ) ⟨IfTail#2⟩ ⟧)↓next(#n) →
    ⟦
      { ⟨Instructions ECond(#1)↓true(⟦true⟧)↓false(⟦false⟧)↓num(⟦dc⟧)⟩ }
      { ⟨Instructions IT(#2)↓true(⟦true⟧)↓false(⟦false⟧)↓next(⟦after⟧)⟩ }
      ⟨Label ⟦after⟧⟩
      B ⟨Label #n⟩
    ⟧;
  SSingle(⟦ while ( ⟨Expression#1⟩ ) ⟨Statement#2⟩ ⟧)↓next(#n) →
    ⟦
      ⟨Label ⟦repeat⟧⟩
      { ⟨Instructions ECond(#1)↓true(⟦body⟧)↓false(⟦after⟧)↓num(⟦dc⟧)⟩ }
      ⟨Label ⟦body⟧⟩
      { ⟨Instructions SSingle(#2)↓next(⟦repeat⟧)⟩ }
      ⟨Label ⟦after⟧⟩
      B ⟨Label #n⟩
    ⟧;
  SSingle(⟦ return ⟨Expression#1⟩ ; ⟧)↓return(#r) →
    ⟦
      { ⟨Instructions RE(#1)⟩ }
      B ⟨Label #r⟩
    ⟧;

  sort Instructions | scheme IT(IfTail) ↓ft ↓vt ↓vt2 ↓return ↓unused ↓offset ↓next;
  IT(⟦ ⟨Statement#1⟩ else ⟨Statement#2⟩ ⟧)↓true(#t)↓false(#f)↓next(#n)↓unused(#u) →
    ⟦
      ⟨Label #t⟩
      { ⟨Instructions SSingle(#1)↓next(#n)↓unused(#u)⟩ }
      ⟨Label #f⟩
      { ⟨Instructions SSingle(#2)↓next(#n)↓unused(#u)⟩ }
    ⟧;
  IT(⟦ ⟨Statement#1⟩ ⟧)↓true(#t)↓false(#f)↓next(#n)↓unused(#u) →
    ⟦
      ⟨Label #t⟩
      { ⟨Instructions SSingle(#1)↓next(#n)↓unused(#u)⟩ }
      ⟨Label #f⟩
      B ⟨Label #n⟩
    ⟧;

  sort Instructions | scheme AE(Expression,Expression) ↓ft ↓vt ↓vt2 ↓return ↓unused ↓offset ↓next;
  AE(⟦ ⟨Identifier#1⟩ ⟧, ⟦ ⟨Expression#2⟩ ⟧)↓vt{¬#1}↓unused(NoRs) →
    error⟦Trying to use an undeclared variable.⟧;
  AE(⟦ ⟨Identifier#1⟩ ⟧, ⟦ ⟨Expression#2⟩ ⟧)↓vt{#1:#v}↓unused(NoRs) → 
    ⟦
      { ⟨Instructions E(#2)↓vt{#1:#v}↓unused(XRegs(⟦R4-R12⟧))↓num(⟦dc⟧)⟩ }
      { ⟨Instructions Str(#v, ⟦R4⟧)⟩ }
    ⟧;
  AE(⟦ ⟨Identifier#1⟩ ⟧, ⟦ ⟨Expression#2⟩ ⟧)↓vt{#1:#v}↓unused(MoRs(#r,#rs)) → 
    ⟦
      { ⟨Instructions E(#2)↓vt{#1:#v}↓unused(MoRs(#r,#rs))↓num(⟦dc⟧)⟩ }
      { ⟨Instructions Str(#v, #r)⟩ }
    ⟧;
  AE(⟦ *⟨Expression#1⟩ ⟧, ⟦ ⟨Expression#2⟩ ⟧)↓unused(NoRs) →
    ⟦
      { ⟨Instructions E(#1)↓unused(XRegs(⟦R4, R6-R12⟧))↓num(⟦dc1⟧)⟩ }
      { ⟨Instructions E(#2)↓unused(XRegs(⟦R5, R6-R12⟧))↓num(⟦dc2⟧)⟩ }
      { ⟨Instructions StrR(⟦R4⟧, ⟦R5⟧)⟩ }
    ⟧;
  AE(⟦ *⟨Expression#1⟩ ⟧, ⟦ ⟨Expression#2⟩ ⟧)↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      { ⟨Instructions E(#1)↓unused(MoRs(#r1,#rs))↓num(⟦dc1⟧)⟩ }
      { ⟨Instructions E(#2)↓unused(MoRs(#r2,#rs))↓num(⟦dc2⟧)⟩ }
      { ⟨Instructions StrR(#r1, #r2)⟩ }
    ⟧;
  AE(#,#) → 
    error⟦Can only assign to l-values.⟧;

  sort Instructions | scheme RE(Expression) ↓ft ↓vt ↓vt2 ↓return ↓unused ↓offset ↓next;
  RE(#1)↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦ 
      { ⟨Instructions E(#1)↓unused(MoRs(#r1,MoRs(#r2,#rs)))↓num(⟦dc⟧)⟩ }
      MOV R0, ⟨Reg#r1⟩
    ⟧;
  RE(#1)↓unused(#) →
    ⟦ 
      { ⟨Instructions E(#1)↓unused(XRegs(⟦R4-R12⟧))↓num(⟦dc⟧)⟩ }
      MOV R0, R4
    ⟧;
  RE(#) → 
    error⟦Whoops at translation of return expression.⟧;

  sort Instructions | scheme FC(ExpressionList, Identifier) ↓ft ↓vt ↓vt2 ↓return ↓unused ↓offset ↓next;
  /*FC(⟦⟧, #i)↓ft{¬#i:#t}↓unused(MoRs(#r1,#rs)) → 
    error ⟦Undeclared function called.⟧;*/
  /*FC(⟦⟧, ⟦div⟧)↓unused(MoRs(#r1,#rs)) → 
    ⟦
      B ⟨Label ⟦div⟧⟩
      MOV ⟨Reg#r1⟩, R0
    ⟧;
   FOR OUR REFERENCE (ABOVE) */
  FC(⟦⟧, #i)↓unused(MoRs(#r1,#rs)) → 
    ⟦
      B ⟨Label #i⟩
      MOV ⟨Reg#r1⟩, R0
    ⟧;
  /*FC(⟦ ⟨Expression#1⟩ ⟨ExpressionListTail#2⟩ ⟧, #i)↓ft{¬#i}↓unused(MoRs(#r1,#rs)) →
    error ⟦Undeclared function called.⟧;*/
  FC(⟦ ⟨Expression#1⟩ ⟨ExpressionListTail#2⟩ ⟧, #i)↓unused(MoRs(#r1,#rs)) →
    ⟦
      STMFD SP!, {R0-R3}
      { ⟨Instructions E(#1)↓unused(MoRs(#r1,#rs))↓num(⟦dc⟧)⟩ }
      MOV R0, ⟨Reg#r1⟩
      { ⟨Instructions FCTail(#2, XRegs(⟦R1-R3⟧))↓unused(MoRs(#r1,#rs))↓next(⟦dc⟧)⟩ }
      B ⟨Label #i⟩
      MOV ⟨Reg#r1⟩, R0
      LDMFD SP!, {R0-R3}
    ⟧;
  FC(#,#) → 
    error⟦Whoops at translation of function call.⟧;

  sort Instructions | scheme FCTail(ExpressionListTail, Rs) ↓ft ↓vt ↓return ↓unused ↓offset ↓next;
  FCTail(⟦ , ⟨Expression#1⟩ ⟨ExpressionListTail#2⟩ ⟧, MoRs(#r, #rs))↓unused(MoRs(#r1,#rs2)) →
    ⟦
      { ⟨Instructions E(#1)↓unused(MoRs(#r1,#rs2))↓num(⟦dc⟧)⟩ }
      MOV ⟨Reg #r⟩, ⟨Reg#r1⟩
      { ⟨Instructions FCTail(#2, #rs)↓unused(MoRs(#r1,#rs2))⟩ }
    ⟧;
  FCTail(⟦ ⟧, #) →
    ⟦ ⟧;
  FCTail(⟦ , ⟨Expression#1⟩ ⟨ExpressionListTail#2⟩ ⟧, NoRs) →
    error⟦Functions can only take 4 arguments.⟧ ;
  FCTail(#,#) → 
    error⟦Whoops at translation of function call tail.⟧;

  sort Instructions | scheme E(Expression) ↓ft ↓vt ↓vt2 ↓return ↓unused ↓offset ↓next ↓num;
  E(⟦ ⟨Integer#1⟩ + ⟨Identifier#2⟩ ⟧)↓vt2{#2: #t}↓unused(NoRs)↓num(#n) →
    ⟦
      ⟨Label #n⟩ DCI ⟨Integer #1⟩
      MOV ⟨Reg ⟦R4⟧⟩, #0
      LDR ⟨Reg ⟦R4⟧⟩, [⟨Reg ⟦R4⟧⟩, & ⟨Label #n⟩]
      { ⟨Instructions E(⟦ sizeof ( ⟨Type #t⟩ )⟧)↓unused(XRegs(⟦R5, R6-R12⟧))⟩ }
      MUL ⟨Reg ⟦R4⟧⟩, ⟨Reg ⟦R4⟧⟩, ⟨Reg ⟦R5⟧⟩
    ⟧;
  E(⟦ ⟨Integer#1⟩ + ⟨Identifier#2⟩ ⟧)↓vt2{#2: #t}↓unused(MoRs(#r1,MoRs(#r2,#rs)))↓num(#n) →
    ⟦
      ⟨Label #n⟩ DCI ⟨Integer #1⟩
      MOV ⟨Reg #r1⟩, #0
      LDR ⟨Reg #r1⟩, [⟨Reg #r1⟩, & ⟨Label #n⟩]
      { ⟨Instructions E(⟦ sizeof ( ⟨Type #t⟩ )⟧)↓unused(MoRs(#r2,#rs))⟩ }
      MUL ⟨Reg #r1⟩, ⟨Reg #r1⟩, ⟨Reg #r2⟩
    ⟧;
  E(⟦ ⟨Identifier#1⟩ + ⟨Integer#2⟩ ⟧)↓vt2{#1: #t}↓unused(NoRs)↓num(#n) →
    ⟦
      { ⟨Instructions E(⟦ sizeof ( ⟨Type #t⟩ )⟧)↓unused(XRegs(⟦R5, R6-R12⟧))⟩ }
      ⟨Label #n⟩ DCI ⟨Integer #2⟩
      MOV ⟨Reg ⟦R5⟧⟩, #0
      LDR ⟨Reg ⟦R5⟧⟩, [⟨Reg ⟦R5⟧⟩, & ⟨Label #n⟩]
      MUL ⟨Reg ⟦R4⟧⟩, ⟨Reg ⟦R4⟧⟩, ⟨Reg ⟦R5⟧⟩
    ⟧;
  E(⟦ ⟨Identifier#1⟩ + ⟨Integer#2⟩ ⟧)↓vt2{#1: #t}↓unused(MoRs(#r1,MoRs(#r2,#rs)))↓num(#n) →
    ⟦
      { ⟨Instructions E(⟦ sizeof ( ⟨Type #t⟩ )⟧)↓unused(MoRs(#r1,#rs))⟩ }
      ⟨Label #n⟩ DCI ⟨Integer #2⟩
      MOV ⟨Reg #r2⟩, #0
      LDR ⟨Reg #r2⟩, [⟨Reg #r2⟩, & ⟨Label #n⟩]
      MUL ⟨Reg #r1⟩, ⟨Reg #r1⟩, ⟨Reg #r2⟩
    ⟧;
  E(⟦ ⟨Integer#1⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r,#rs))↓num(#2) →
    ⟦
      ⟨Label #2⟩ DCI ⟨Integer #1⟩
      MOV ⟨Reg #r⟩, #0
      LDR ⟨Reg #r⟩, [⟨Reg #r⟩, & ⟨Label #2⟩]
    ⟧;
  E(⟦ ⟨String#1⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r,#rs))↓num(#2) →
    ⟦
      ⟨Label #2⟩ DCS ⟨String#1⟩
      MOV ⟨Reg #r⟩, #0
      LDR ⟨Reg #r⟩, [⟨Reg #r⟩,& ⟨Label #2⟩]
    ⟧;
  E(⟦ ⟨Identifier#1⟩ ⟧)↓vt{¬#1}↓unused(MoRs(#r,#rs)) →
    error⟦Trying to use an undeclared variable.⟧;
  E(⟦ ⟨Identifier#1⟩ ⟧)↓vt{#1:#v}↓unused(MoRs(#r,#rs)) →
    ⟦
      { ⟨Instructions Ldr(#v, #r)⟩ }
    ⟧;
  E(⟦ ⟨Identifier#1⟩ ( ⟨ExpressionList#2⟩ ) ⟧)↓unused(NoRs) → 
    ⟦
      { ⟨Instructions FC(#2, #1)↓unused(XRegs(⟦R4-R12⟧))⟩ }
    ⟧;
  E(⟦ ⟨Identifier#1⟩ ( ⟨ExpressionList#2⟩ ) ⟧)↓unused(MoRs(#r,#rs)) → 
    ⟦
      { ⟨Instructions FC(#2, #1)↓unused(MoRs(#r,#rs))⟩ }
    ⟧;
  E(⟦ null ( ⟨Type#1⟩ ) ⟧)↓unused(MoRs(#r,#rs)) →
    ⟦
      MOV ⟨Reg #r⟩, #0
    ⟧;
  E(⟦ sizeof ( char )⟧)↓unused(MoRs(#r,#rs)) →
    ⟦
      MOV ⟨Reg #r⟩, #1
    ⟧;
  E(⟦ sizeof ( ⟨Type#1⟩ )⟧)↓unused(MoRs(#r,#rs)) →
    ⟦
      MOV ⟨Reg #r⟩, #4
    ⟧;
  E(⟦ ! ⟨Expression#1⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4-R12⟧))⟩}
      MVN ⟨Reg ⟦R4⟧⟩, ⟨Reg ⟦R4⟧⟩
    ⟧;
  E(⟦ ! ⟨Expression#1⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs)))⟩}
      MVN ⟨Reg #r1⟩, ⟨Reg #r1⟩
    ⟧;
  E(⟦ - ⟨Expression#1⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4-R12⟧))⟩}
      RSB ⟨Reg ⟦R4⟧⟩, ⟨Reg ⟦R4⟧⟩, #0
    ⟧;
  E(⟦ - ⟨Expression#1⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs)))⟩}
      RSB ⟨Reg #r1⟩, ⟨Reg #r1⟩, #0
    ⟧;
  E(⟦ + ⟨Expression#1⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4-R12⟧))⟩}
      CMP ⟨Reg ⟦R4⟧⟩, #0
      BGE ⟨Label ⟦pos⟧⟩
      RSB ⟨Reg ⟦R4⟧⟩, ⟨Reg ⟦R4⟧⟩, #0
      ⟨Label ⟦pos⟧⟩
    ⟧;
  E(⟦ + ⟨Expression#1⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs)))⟩}
      CMP ⟨Reg #r1⟩, #0
      BGE ⟨Label ⟦pos⟧⟩
      RSB ⟨Reg #r1⟩, ⟨Reg #r1⟩, #0
      ⟨Label ⟦pos⟧⟩
    ⟧;
  E(⟦ * ⟨Expression#1⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4-R12⟧))⟩}
      MOV R4, R4
    ⟧;
    //LDR R4, [R12, R4]
    //LDR ⟨Reg ⟦R4⟧⟩, [R12, ⟨Reg ⟦R4⟧⟩]
  E(⟦ * ⟨Expression#1⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs)))⟩}
      MOV ⟨Reg #r1⟩, ⟨Reg #r1⟩
    ⟧;
    //LDR ⟨Reg #r1⟩, [R12, ⟨Reg #r1⟩]
    // {⟨Instructions Ldr(#r1, #r1)⟩}
  E(⟦ & ⟨Identifier#1⟩ ⟧)↓vt{#1:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions Ldr(#v, ⟦R4⟧)⟩}
    ⟧;
  E(⟦ & ⟨Identifier#1⟩ ⟧)↓vt{#1:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions Ldr(#v, #r1)⟩}
    ⟧;
  E(⟦ ⟨Expression#1⟩ * ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4, R6-R12⟧))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(XRegs(⟦R5, R6-R12⟧))⟩}
      MUL R4, R4, R5
    ⟧;
  E(⟦ ⟨Expression#1⟩ * ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,#rs))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(MoRs(#r2,#rs))⟩}
      MUL ⟨Reg #r1⟩, ⟨Reg #r1⟩, ⟨Reg #r2⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ + ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4, R6-R12⟧))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(XRegs(⟦R5, R6-R12⟧))⟩}
      ADD R4, R4, R5
    ⟧;
  E(⟦ ⟨Expression#1⟩ + ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,#rs))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(MoRs(#r2,#rs))⟩}
      ADD ⟨Reg #r1⟩, ⟨Reg #r1⟩, ⟨Reg #r2⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ - ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4, R6-R12⟧))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(XRegs(⟦R5, R6-R12⟧))⟩}
      SUB R4, R4, R5
    ⟧;
  E(⟦ ⟨Expression#1⟩ - ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,#rs))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(MoRs(#r2,#rs))⟩}
      SUB ⟨Reg #r1⟩, ⟨Reg #r1⟩, ⟨Reg #r2⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ < ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4, R6-R12⟧))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(XRegs(⟦R5, R6-R12⟧))⟩}
      CMP R4, R5
      BLTT ⟨Label ⟦T⟧⟩
      MOV R4, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV R4, #1
      ⟨Label ⟦R⟧⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ < ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,#rs))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(MoRs(#r2,#rs))⟩}
      CMP ⟨Reg #r1⟩, ⟨Reg #r2⟩
      BLT ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #1
      ⟨Label ⟦R⟧⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ > ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4, R6-R12⟧))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(XRegs(⟦R5, R6-R12⟧))⟩}
      CMP R4, R5
      BGT ⟨Label ⟦T⟧⟩
      MOV R4, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV R4, #1
      ⟨Label ⟦R⟧⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ > ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,#rs))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(MoRs(#r2,#rs))⟩}
      CMP ⟨Reg #r1⟩, ⟨Reg #r2⟩
      BGT ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #1
      ⟨Label ⟦R⟧⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ <= ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4, R6-R12⟧))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(XRegs(⟦R5, R6-R12⟧))⟩}
      CMP R4, R5
      BLE ⟨Label ⟦T⟧⟩
      MOV R4, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV R4, #1
      ⟨Label ⟦R⟧⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ <= ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,#rs))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(MoRs(#r2,#rs))⟩}
      CMP ⟨Reg #r1⟩, ⟨Reg #r2⟩
      BLE ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #1
      ⟨Label ⟦R⟧⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ >= ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4, R6-R12⟧))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(XRegs(⟦R5, R6-R12⟧))⟩}
      CMP R4, R5
      BGE ⟨Label ⟦T⟧⟩
      MOV R4, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV R4, #1
      ⟨Label ⟦R⟧⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ >= ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,#rs))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(MoRs(#r2,#rs))⟩}
      CMP ⟨Reg #r1⟩, ⟨Reg #r2⟩
      BGE ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #1
      ⟨Label ⟦R⟧⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ == ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4, R6-R12⟧))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(XRegs(⟦R5, R6-R12⟧))⟩}
      CMP R4, R5
      BEQ ⟨Label ⟦T⟧⟩
      MOV R4, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV R4, #1
      ⟨Label ⟦R⟧⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ == ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,#rs))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(MoRs(#r2,#rs))⟩}
      CMP ⟨Reg #r1⟩, ⟨Reg #r2⟩
      BEQ ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #1
      ⟨Label ⟦R⟧⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ != ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4, R6-R12⟧))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(XRegs(⟦R5, R6-R12⟧))⟩}
      CMP R4, R5
      BNE ⟨Label ⟦T⟧⟩
      MOV R4, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV R4, #1
      ⟨Label ⟦R⟧⟩
    ⟧; 
  E(⟦ ⟨Expression#1⟩ != ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,#rs))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(MoRs(#r2,#rs))⟩}
      CMP ⟨Reg #r1⟩, ⟨Reg #r2⟩
      BNE ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #0
      B ⟨Label ⟦R⟧⟩
      ⟨Label ⟦T⟧⟩
      MOV ⟨Reg #r1⟩, #1
      ⟨Label ⟦R⟧⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ && ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4, R6-R12⟧))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(XRegs(⟦R5, R6-R12⟧))⟩}
      AND R4, R5, R4
    ⟧;
  E(⟦ ⟨Expression#1⟩ && ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,#rs))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(MoRs(#r2,#rs))⟩}
      AND ⟨Reg #r1⟩, ⟨Reg #r1⟩, ⟨Reg #r2⟩
    ⟧;
  E(⟦ ⟨Expression#1⟩ || ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(NoRs) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(XRegs(⟦R4, R6-R12⟧))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(XRegs(⟦R5, R6-R12⟧))⟩}
      ORR R4, R5, R4
    ⟧;
  E(⟦ ⟨Expression#1⟩ || ⟨Expression#2⟩ ⟧)↓vt{:#v}↓unused(MoRs(#r1,MoRs(#r2,#rs))) →
    ⟦
      {⟨Instructions E(#1)↓vt{:#v}↓unused(MoRs(#r1,#rs))⟩}
      {⟨Instructions E(#2)↓vt{:#v}↓unused(MoRs(#r2,#rs))⟩}
      ORR ⟨Reg #r1⟩, ⟨Reg #r1⟩, ⟨Reg #r2⟩
    ⟧;
  E(#) → 
    error⟦Whoops at translation of expressions.⟧;

  sort Instructions | scheme ECond(Expression) ↓ft ↓vt ↓true ↓false ↓unused ↓offset ↓num;
  ECond(⟦ ⟨Integer#1⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ ⟨Integer#1⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨String#1⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ ⟨String#1⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Identifier#1⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ ⟨Identifier#1⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Identifier#1⟩ ( ⟨ExpressionList#2⟩ )⟧)↓true(#t)↓false(#f) →  
    ⟦
      { ⟨Instructions E(⟦ ⟨Identifier#1⟩ ( ⟨ExpressionList#2⟩ )⟧)⟩ }
      CMP R0, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ null ( ⟨Type#1⟩ ) ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ null ( ⟨Type#1⟩ ) ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  /*ECond(⟦ sizeof ( char )⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ null ( ⟨Type⟩ ) ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;*/
  ECond(⟦ sizeof ( ⟨Type#1⟩ )⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ null ( ⟨Type#1⟩ ) ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ! ⟨Expression#1⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ ! ⟨Expression#1⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ - ⟨Expression#1⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ - ⟨Expression#1⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ + ⟨Expression#1⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ + ⟨Expression#1⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ * ⟨Expression#1⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ * ⟨Expression#1⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ & ⟨Identifier#1⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ & ⟨Identifier#1⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Expression#1⟩ * ⟨Expression#2⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ ⟨Expression#1⟩ * ⟨Expression#2⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Expression#1⟩ + ⟨Expression#2⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ ⟨Expression#1⟩ + ⟨Expression#2⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Expression#1⟩ - ⟨Expression#2⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ ⟨Expression#1⟩ - ⟨Expression#2⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Expression#1⟩ < ⟨Expression#2⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦ 
      {⟨Instructions E(⟦ ⟨Expression#1⟩ < ⟨Expression#2⟩ ⟧)⟩}
      CMP R4, R5
      BLT ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Expression#1⟩ > ⟨Expression#2⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ ⟨Expression#1⟩ > ⟨Expression#2⟩ ⟧)⟩}
      CMP R4, R5
      BGT ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Expression#1⟩ <= ⟨Expression#2⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ ⟨Expression#1⟩ <= ⟨Expression#2⟩ ⟧)⟩}
      CMP R4, R5
      BLE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Expression#1⟩ >= ⟨Expression#2⟩ ⟧)↓true(#t)↓false(#f) → 
    ⟦
      {⟨Instructions E(⟦ ⟨Expression#1⟩ >= ⟨Expression#2⟩ ⟧)⟩}
      CMP R4, R5
      BGE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Expression#1⟩ == ⟨Expression#2⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ ⟨Expression#1⟩ == ⟨Expression#2⟩ ⟧)⟩}
      CMP R4, R5
      BEQ ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Expression#1⟩ != ⟨Expression#2⟩ ⟧)↓true(#t)↓false(#f) →
    ⟦
      {⟨Instructions E(⟦ ⟨Expression#1⟩ != ⟨Expression#2⟩ ⟧)⟩}
      CMP R4, R5
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Expression#1⟩ && ⟨Expression#2⟩ ⟧)↓true(#t)↓false(#f) → 
    ⟦ 
      {⟨Instructions E(⟦ ⟨Expression#1⟩ && ⟨Expression#2⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(⟦ ⟨Expression#1⟩ || ⟨Expression#2⟩ ⟧)↓true(#t)↓false(#f) → 
    ⟦ 
      {⟨Instructions E(⟦ ⟨Expression#1⟩ || ⟨Expression#2⟩ ⟧)⟩}
      CMP R4, #0
      BNE ⟨Label #t⟩
      B ⟨Label #f⟩
    ⟧;
  ECond(#) → 
    error⟦Tests should be valid conditional statements.⟧;

}
