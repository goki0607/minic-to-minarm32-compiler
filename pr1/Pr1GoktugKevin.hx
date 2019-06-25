module org.crsx.hacs.Pr1GoktugKevin {



  // 1.1 TOKENS (implementation given below)

  space [ \t\n\r]                        // white space
    |   "//" [^\n]*                      // single line comments
    |   "/*" ( [^*] | "*" [^//] )* "*/"  // multi-line comments
    ;

  token MainFunc  // creating a token for the main function at the top level,
                  // check companion document for reasoning
    | "function int main"  // the main function can only be of this form
                           // (can have different params though)
    ;

  token Identifier
    | ⟨ValidStart⟩ ( ⟨ValidStart⟩ | ⟨Digit⟩ )*   // identifiers
    ;
  
  token Integer
    | ⟨Digit⟩+  // integers
    ;

  token String
    | \" ( [^\"\\] | \\ ⟨Escaped⟩ )* \"  // strings
    ;

  token fragment Letter
    | [a-zA-Z]  // all letters in the english alphabet
    ;

  token fragment Digit
    | [0-9]  // digits from 0 to 9
    ;

  token fragment ValidStart  // all acceptable ways to start an identifier
    | ⟨Letter⟩  // letter is ok
    | "_"       // _ is also ok
    | "$"       // $ is also ok
    ;

  token fragment Escaped                         // all acceptable sequences after an escape
    | \n                                         // line continuation
    | [\"\\]                                     // escaped " or \
    | [nt]                                       // newline or tabulation
    | ⟨OctalDigit⟩ ⟨OctalDigit⟩? ⟨OctalDigit⟩?   // octal byte encoding
    | x ⟨HexaDecimalDigit⟩ ⟨HexaDecimalDigit⟩ ?  //  hexadecimal byte encoding 
    ;

  token fragment OctalDigit
    | [0-7]  // a valid octal digit
    ;

  token fragment HexaDecimalDigit
    | [0-9a-fA-F]  // a valid hexadecimal Digit
    ;



  // 1.2 EXPRESSIONS (implementation given below)

  sort Expr                            // expressions
    | ⟦ ⟨Identifier⟩ ⟧@9               // idenfitifers
    | ⟦ ⟨String⟩ ⟧@9                   // strings
    | ⟦ ⟨Integer⟩ ⟧@9                  // integers
    | sugar ⟦ ( ⟨Expr#⟩ ) ⟧@9 → Expr#  // parenthesis is just sugar
    | ⟦ ⟨Expr@8⟩ ( ⟨Exprs⟩ ) ⟧@8       // expression lists
    | ⟦ null ( ⟨Type⟩ ) ⟧@8            // null(Type)
    | ⟦ sizeof ( ⟨Type⟩ ) ⟧@8          // sizeof(Type)
    | ⟦ ! ⟨Expr@7⟩ ⟧@7                 // not
    | ⟦ - ⟨Expr@7⟩ ⟧@7                 // negative
    | ⟦ + ⟨Expr@7⟩ ⟧@7                 // positive
    | ⟦ * ⟨Expr@7⟩ ⟧@7                 // pointer dereference
    | ⟦ & ⟨Expr@7⟩ ⟧@7                 // address retrieval
    | ⟦ ⟨Expr@6⟩ * ⟨Expr@7⟩ ⟧@6        // multiplication
    | ⟦ ⟨Expr@6⟩ / ⟨Expr@7⟩ ⟧@6        // division
    | ⟦ ⟨Expr@6⟩ % ⟨Expr@7⟩ ⟧@6        // modulo
    | ⟦ ⟨Expr@5⟩ + ⟨Expr@6⟩ ⟧@5        // addition
    | ⟦ ⟨Expr@5⟩ - ⟨Expr@6⟩ ⟧@5        // subtraction
    | ⟦ ⟨Expr@5⟩ < ⟨Expr@5⟩ ⟧@4        // less than
    | ⟦ ⟨Expr@5⟩ > ⟨Expr@5⟩ ⟧@4        // greater than
    | ⟦ ⟨Expr@5⟩ <= ⟨Expr@5⟩ ⟧@4       // less than or equal
    | ⟦ ⟨Expr@5⟩ >= ⟨Expr@5⟩ ⟧@4       // greater than or equal
    | ⟦ ⟨Expr@4⟩ == ⟨Expr@4⟩ ⟧@3       // is equal
    | ⟦ ⟨Expr@4⟩ != ⟨Expr@4⟩ ⟧@3       // is not equal
    | ⟦ ⟨Expr@3⟩ && ⟨Expr@2⟩ ⟧@2       // and 
    | ⟦ ⟨Expr@2⟩ || ⟨Expr@1⟩ ⟧@1       // or
    ;

  sort Exprs                  // list of expressions
    | ⟦ ⟨Expr⟩ ⟨ExprsMore⟩ ⟧  // first expression, commas resolved using helper
    | ⟦ ⟧                     // no expressions
    ;

  sort ExprsMore                // helper function to get correct comma behavior for 
                                // list of expressions
    | ⟦ , ⟨Expr⟩ ⟨ExprsMore⟩ ⟧  // add more expressions with correct comma placement
    | ⟦ ⟧                       // stop adding expressions
    ;



  // 1.3 TYPES (implementation given below)

  sort Type                            // types
    | ⟦ int ⟧@3                        // int type
    | ⟦ char ⟧@3                       // int type
    | sugar ⟦ ( ⟨Type#⟩ ) ⟧@3 → Type#  // parenthesis is just sugar
    | ⟦ ⟨Type@2⟩ ( ⟨Types⟩ ) ⟧@2       // type lists
    | ⟦ * ⟨Type@1⟩ ⟧@1                 // pointer type
    ;

  sort Types                  // list of types
    | ⟦ ⟨Type⟩ ⟨TypesMore⟩ ⟧  // first type, commas resolved using helper
    | ⟦ ⟧                     // no types
    ;

  sort TypesMore                // helper function to get correct comma behavior
                                // for list of types
    | ⟦ , ⟨Type⟩ ⟨TypesMore⟩ ⟧  // add more types with correct comma placement
    | ⟦ ⟧                       // stop adding types
    ;



  // 1.4 STATEMENTS (implementation given below)

  sort Stmt                          // statements
    | ⟦ var ⟨Type⟩ ⟨Identifier⟩ ; ⟧  // variable declaration
    | ⟦ ⟨Lval⟩ = ⟨Expr⟩ ; ⟧          // variable assignment, left side must be an l-value
    | ⟦ if ( ⟨Expr⟩ ) ⟨IfElse⟩ ⟧     // if statement that uses a helper function to eagerly
                                     // evaluate else 
    | ⟦ while ( ⟨Expr⟩ ) ⟨Stmt⟩ ⟧    // while loop
    | ⟦ return ⟨Expr⟩ ; ⟧            // return satement
    | ⟦ { ⟨Stmts⟩ } ⟧                // statement list 
    ;

  sort Lval             // helper function to get valid l-values
    | ⟦ ⟨Identifier⟩ ⟧  // can either be an identifier
    | ⟦ * ⟨Deref⟩ ⟧     // or a pointer dereference
    ;

  sort Deref             // helper function to get correct pointer dereference behavior
    | ⟦ * ⟨Deref⟩ ⟧     // pointer of pointers
    | ⟦ ⟨Identifier⟩ ⟧  // stop dereferencing
  ;

  sort IfElse                 // handling the dangling else problem by using a helper function
    | ⟦ ⟨Stmt⟩ ⟧              // no more else                 
    | ⟦ ⟨Stmt⟩ else ⟨Stmt⟩ ⟧  // eagerly consume an else
    ;

  sort Stmts              // helper function to get zero or more statements
    | ⟦ ⟨Stmt⟩ ⟨Stmts⟩ ⟧  // add more statements
    | ⟦ ⟧                 // stop adding statements to a statement list
    ;



  // 1.5 DECLARATIONS (implementation given below)

  sort Decl                                                      // declarations
    | ⟦ function ⟨Type⟩ ⟨Identifier⟩ ( ⟨Params⟩ ) { ⟨Stmts⟩ } ⟧  // function declaration
    ;

  sort Params                               // list of formal parameters
    | ⟦ ⟨Type⟩ ⟨Identifier⟩ ⟨ParamsMore⟩ ⟧  // first parameter, commas resolved using helper
    | ⟦ ⟧                                   // no parameters
    ;

  sort ParamsMore                             // helper function to get correct comma
                                              // behavior for list of formal parameters
    | ⟦ , ⟨Type⟩ ⟨Identifier⟩ ⟨ParamsMore⟩ ⟧  // add more parameters with correct comma
    | ⟦ ⟧                                     // placement stop adding parameters
    ;



  // 1.6 PROGRAM (implementation given below)

  sort MainDecl                                // main function a.k.a the main declaration 
    | ⟦ ⟨MainFunc⟩ ( ⟨Params⟩ ) { ⟨Stmts⟩ } ⟧  // the main function must be of the form:
                                               // function int main ( [optional param list] )
    ;

  main sort Program           // definition of a program
    | ⟦ ⟨Decls⟩ ⟨MainDecl⟩ ⟨Decls⟩ ⟧  // a program must have a main function and
                              // an optional amount of declarations
    ;

  sort Decls              // helper function to have zero or more declarations
    | ⟦ ⟨Decl⟩ ⟨Decls⟩ ⟧  // add one more declaration
    | ⟦ ⟧                 // no more declarations
    ;

}