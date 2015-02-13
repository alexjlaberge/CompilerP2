/* File: parser.y
 * --------------
 * Yacc input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should 
 *      accept the language as described in specification, and as augmented 
 *      in the pp2 handout.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine

%}

/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */
 
/* yylval 
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser. 
 *
 * pp2: You will need to add new fields to this union as you add different 
 *      attributes to your non-terminal symbols.
 */
%union {
    int integerConstant;
    bool boolConstant;
    char *stringConstant;
    double doubleConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    Decl *decl;
    List<Decl*> *declList;
    Type *type;
    VarDecl *varDecl;
    List<VarDecl*> *varList;
    ClassDecl *classDecl;
    InterfaceDecl *interfaceDecl;
    FnDecl *fnDecl;
    StmtBlock *stmtBlck;
    Stmt *stmt;
    List<Stmt*> *stmtList;
    Expr *expr;
    List<Expr*> *exprList;
    List<NamedType*> *namedTypeList;
    List<FnDecl*> *fnDeclList;
    Identifier *iden;
    StmtBlock *stmtBlock;
    NamedType *namedType;
    PrintStmt *printStmt;
    List<Case*> *caseList;
    Case *c;
    Default *def;
    SwitchStmt *switchStmt;
    IntConstant *intC;
    PostfixExpr *postExpr;
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Yacc will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */
%token   T_Void T_Bool T_Int T_Double T_String T_Class 
%token   T_LessEqual T_GreaterEqual T_Equal T_NotEqual T_Dims
%token   T_And T_Or T_Null T_Extends T_This T_Interface T_Implements
%token   T_While T_For T_If T_Else T_Return T_Break
%token   T_New T_NewArray T_Print T_ReadInteger T_ReadLine

%token   <identifier> T_Identifier
%token   <stringConstant> T_StringConstant 
%token   <integerConstant> T_IntConstant
%token   <doubleConstant> T_DoubleConstant
%token   <boolConstant> T_BoolConstant

%token   T_Increm T_Decrem T_Switch T_Case T_Default


/* Non-terminal types/
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */
%nonassoc '='
%left T_Or 
%left T_And
%nonassoc T_Equal T_NotEqual
%nonassoc '<' '>' T_GreaterEqual T_LessEqual
%left '+' '-'
%left '*' '/' '%'
%left '!' T_Increm T_Decrem //Should T_Increm and T_Decrem be in here?
%nonassoc '[' '.'
%nonassoc No_Else
%nonassoc T_Else

%type <declList>  DeclList
%type <varList>      VarList 
%type <decl>      Decl
%type <type>      Type
%type <varDecl>      VarDecl
%type <varDecl>      Var
%type <interfaceDecl> InterfaceDecl
%type <classDecl> ClassDecl
%type <fnDecl> FnDecl
%type <stmtBlck> StmtBlock
%type <stmtList> StmtList
%type <stmt> Stmt
%type <stmt> ConditionalStmt
%type <stmt> LoopStmt
%type <stmt> ForStmt
%type <stmt> WhileStmt
%type <stmt> IfStmt
%type <stmt> BreakStmt
%type <stmt> ReturnStmt
%type <printStmt> PrintStmt
%type <expr> Expr
%type <exprList> ExprList
%type <exprList> Actuals
%type <namedType> NamedType
%type <type> ArrayType
%type <iden> Identifier
%type <declList> FieldList
%type <decl> Field
%type <namedTypeList> NamedTypeList
%type <decl> Prototype
%type <varList> Formals
%type <varList> VarDeclList
%type <expr> CompoundExpr
%type <expr> LValue
%type <expr> Call
%type <expr> ArithmeticExpr
%type <expr> EqualityExpr
%type <expr> RelationalExpr
%type <expr> LogicalExpr
%type <expr> AssignExpr
%type <declList> PrototypeList
%type <caseList> CaseList
%type <def> Default
%type <c> Case
%type <switchStmt> SwitchStmt
%type <caseList> CaseBlock
%type <intC> IntConstant
%type <postExpr> PostfixExpr
%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.
     
 */
Program:    
    DeclList    
    { 
      @1;
      Program *program = new Program($1);
      if (ReportError::NumErrors() == 0) 
          program->Print(0);             
    };

DeclList:    
    DeclList Decl 
    {
        ($$=$1)->Append($2);
    } |

    Decl 
    {
        ($$ = new List<Decl*>)->Append($1);
    };

Decl:    
    VarDecl 
    {
        $$=$1;
    } |

    FnDecl  
    {
        $$=$1;
    } |

    ClassDecl 
    {
        $$=$1;
    } |

    InterfaceDecl 
    {
        $$=$1;
    };    

VarDecl: 
    Var ';' 
    {
        $$=$1;
    };

VarList:
    VarList ',' Var 
    {
        ($$=$1)->Append($3);
    } |

    Var 
    {
        ($$ = new List<VarDecl*>)->Append($1);
    };   

Var:       
    Type Identifier 
    {
        $$ = new VarDecl($2,$1);
    };

Type:
    T_Bool 
    {
        $$ = new Type("bool");
    } |

    T_Int 
    {
        $$ = new Type("int");
    } |

    T_Double 
    {
        $$ = new Type("double");
    } |

    T_String 
    {
        $$ = new Type("string");
    } |

    T_Null 
    {
        $$ = new Type("null");
    } |

    T_Void 
    {
        $$ = new Type("void");
    } |

    NamedType 
    {
        $$ = $1;
    } |

    ArrayType 
    {
        $$ = $1;
    };

NamedType:     
    Identifier 
    {
        $$ = new NamedType($1);
    };

ArrayType:     
    Type T_Dims 
    {
        $$ = new ArrayType(@1, $1);
    };

ClassDecl :    
    T_Class Identifier T_Extends NamedType '{' FieldList '}'    
    {
        $$ = new ClassDecl($2, $4, new List<NamedType*>, $6);
    } |

    T_Class Identifier T_Implements NamedTypeList '{' FieldList '}'    
    {
        $$ = new ClassDecl($2, NULL, $4, $6);
    } |

    T_Class Identifier T_Extends NamedType T_Implements NamedTypeList '{' FieldList '}'    
    {
        $$ = new ClassDecl($2, $4, $6, $8);
    } |

    T_Class Identifier '{' FieldList '}'    
    {
        $$ = new ClassDecl($2, NULL, new List<NamedType*>, $4);
    } |

    T_Class Identifier T_Extends NamedType '{' '}'    
    {
        $$ = new ClassDecl($2, $4, new List<NamedType*>, new List<Decl*>);
    } |

    T_Class Identifier T_Implements NamedTypeList '{' '}'    
    {
        $$ = new ClassDecl($2, NULL, $4, new List<Decl*>);
    } |

    T_Class Identifier T_Extends NamedType T_Implements NamedTypeList '{''}'    
    {
        $$ = new ClassDecl($2, $4, $6, new List<Decl*>);
    } |

    T_Class Identifier '{' '}'    
    {
        $$ = new ClassDecl($2, NULL, new List<NamedType*>, new List<Decl*>);
    };


FieldList :    
    FieldList Field 
    {
        ($$=$1)->Append($2);
    } |

    Field 
    {
        ($$ = new List<Decl*>)->Append($1);
    };

Field:    
    VarDecl 
    {
        $$=$1;
    } |

    FnDecl 
    {
        $$=$1;
    };


NamedTypeList: 
    NamedTypeList ',' NamedType   
    {
        ($$=$1)->Append($3);
    } |

    NamedType
    {
        ($$ = new List<NamedType*>)->Append($1);
    };

Identifier: 
    T_Identifier 
    {
        $$ = new Identifier(@1, $1);
    };

InterfaceDecl: 
    T_Interface Identifier '{' PrototypeList '}' 
    {
        $$ = new InterfaceDecl($2, $4);
    };

PrototypeList: 
    PrototypeList Prototype 
    {
        ($$=$1)->Append($2);
    } |

    Prototype 
    {
        ($$ = new List<Decl*>)->Append($1);
    };

Prototype: 
    Type Identifier '(' Formals ')' ';' 
    {
        $$ = new FnDecl($2, $1, $4);
    };

FnDecl:    
    Type T_Identifier '(' Formals ')' StmtBlock        
    { 
        $$ = new FnDecl(new Identifier(@2,$2), $1, $4); 
        $$->SetFunctionBody($6);
    };

Formals: 
    VarList                 
    {
        $$ = $1;
    } |

    /*Empty*/                        
    {
        $$ = new List<VarDecl*>;
    };

StmtBlock: 
    '{' VarDeclList StmtList '}'                 
    {
        $$= new StmtBlock($2, $3);
    } |

    '{' VarDeclList '}'                 
    {
        $$= new StmtBlock($2, new List<Stmt*>);
    } |

    '{' StmtList '}'                 
    {
        $$= new StmtBlock(new List<VarDecl*>, $2);
    } |

    '{' '}'                 
    {
        $$= new StmtBlock(new List<VarDecl*>, new List<Stmt*>);
    };


VarDeclList: 
    VarDeclList VarDecl 
    {
        ($$=$1)->Append($2);
    } |

    VarDecl            
    {
        ($$ = new List<VarDecl*>)->Append($1);
    };

StmtList: 
    StmtList Stmt             
    {
        ($$=$1)->Append($2);
    } |

    Stmt                     
    {
        ($$ = new List<Stmt*>)->Append($1);
    };

Stmt: 
    ConditionalStmt
    {
        $$=$1;
    } |

    BreakStmt
    {
        $$=$1;
    } |

    SwitchStmt
    {
        $$=$1;
    } |

    ReturnStmt               
    {
        $$=$1;
    } |

    PrintStmt           
    {
        $$=$1;
    } |

    Expr ';'                
    {
        $$=$1;
    } |

    StmtBlock               
    {
        $$ = $1;
    };

SwitchStmt: 
    T_Switch '(' Expr ')' CaseBlock 
    {
        $$ = new SwitchStmt($3, $5);
    };

CaseBlock: 
    '{' CaseList Default '}'   
    {
        ($$=$2)->Append($3);
    };

CaseList: 
    CaseList Case 
    {
        ($$=$1)->Append($2);
    } |

    Case 
    {
        ($$ = new List<Case*>)->Append($1);
    };

Case: 
    T_Case IntConstant ':' StmtList 
    {
        $$ = new Case($2, $4);
    };

Default: 
    T_Default ':' StmtList 
    {
        $$ = new Default($3);
    };

PrintStmt: 
    T_Print '(' ExprList ')' ';' 
    {
        $$= new PrintStmt($3);
    };

BreakStmt: 
    T_Break ';'             
    {
        $$= new BreakStmt(@1);
    };

ReturnStmt: 
    T_Return Expr ';'         
    {
        $$= new ReturnStmt(@1, $2);
    } |

    T_Return ';'      
    {
        $$= new ReturnStmt(@1, new EmptyExpr());
    };


ConditionalStmt: 
    IfStmt             
    {
        $$=$1;
    } |

    LoopStmt            
    {
        $$=$1;
    };

IfStmt: 
    T_If '(' Expr ')' Stmt T_Else Stmt
    {
        $$ = new IfStmt($3, $5, $7);
    } |

    T_If '(' Expr ')' Stmt %prec No_Else
    {
        $$ = new IfStmt($3, $5, NULL);
    };

LoopStmt: 
    WhileStmt 
    {
        $$=$1;
    } |

    ForStmt 
    {
        $$=$1;
    };

WhileStmt:
    T_While '(' Expr ')' Stmt 
    {
        $$= new WhileStmt($3, $5);
    };

ForStmt : 
    T_For '(' Expr ';' Expr ';' Expr ')' Stmt         
    {
        $$= new ForStmt($3, $5, $7, $9);
    } |

    T_For '(' ';' Expr ';' Expr ')' Stmt         
    {
        $$= new ForStmt(new EmptyExpr(), $4, $6, $8);
    } |

    T_For '(' Expr ';' Expr ';' ')' Stmt         
    {
        $$= new ForStmt($3, $5, new EmptyExpr(), $8);
    } |

    T_For '(' ';' Expr ';' ')' Stmt         
    {
        $$= new ForStmt(new EmptyExpr(), $4, new EmptyExpr(), $7);
    };

Expr: 
    IntConstant 
    {
        $$=$1;
    } |

    T_DoubleConstant 
    {
        $$= new DoubleConstant(@1,$1);
    } |

    T_BoolConstant 
    {
        $$= new BoolConstant(@1,$1);
    } |

    T_StringConstant 
    {
        $$= new StringConstant(@1, $1);
    } |

    T_Null 
    {
        $$= new NullConstant(@1);
    } |

    CompoundExpr 
    {
        $$=$1;
    } |

    LValue 
    {
        $$=$1;
    } |

    T_This 
    {
        $$= new This(@1);
    } |

    Call 
    {
        $$=$1;
    } |

    T_ReadInteger '(' ')' 
    {
        $$= new ReadIntegerExpr(@1);
    } |

    T_ReadLine '(' ')' 
    {
        $$= new ReadLineExpr(@1);
    } |

    '(' Expr ')' 
    {
        $$ = $2;
    } |

    T_NewArray '(' Expr ',' Type ')' 
    {
        $$ = new NewArrayExpr(@1, $3, $5);
    } |

    T_New '(' NamedType ')' 
    {
        $$ = new NewExpr(@1, $3);
    };

IntConstant: 
    T_IntConstant 
    {
        $$= new IntConstant(@1,$1);
    };

ExprList: 
    ExprList ',' Expr 
    {
        ($$=$1)->Append($3);
    } |

    Expr 
    {
        ($$ = new List<Expr*>)->Append($1);
    };

CompoundExpr: 
    EqualityExpr 
    {
        $$=$1;
    } |

    RelationalExpr 
    {
        $$=$1;
    } | 

    ArithmeticExpr 
    {
        $$=$1;
    } |

    LogicalExpr 
    {
        $$=$1;
    } |

    AssignExpr 
    {
        $$=$1;
    } |

    PostfixExpr 
    {
        $$=$1;
    };

AssignExpr: 
    LValue '=' Expr        
    {
        Operator *op = new Operator(@2, "=");
        $$ = new AssignExpr($1, op, $3);
    };

ArithmeticExpr: 
    Expr '*' Expr         
    {
        Operator *op = new Operator(@2, "*");
        $$ = new ArithmeticExpr($1, op, $3);
    } |

    Expr '/' Expr         
    {
        Operator *op = new Operator(@2, "/");
        $$ = new ArithmeticExpr($1, op, $3);
    } |

    Expr '%' Expr      
    {
        Operator *op = new Operator(@2, "%");
        $$ = new ArithmeticExpr($1, op, $3);
    } |

    Expr '+' Expr         
    {
        Operator *op = new Operator(@2, "+");
        $$ = new ArithmeticExpr($1, op, $3);
    } |

    Expr '-' Expr      
    {
        Operator *op = new Operator(@2, "-");
        $$ = new ArithmeticExpr($1, op, $3);
    } |

    '-' Expr   %prec '!'          
    {
        Operator *op = new Operator(@1, "-");
        $$ = new ArithmeticExpr(op, $2);
    };

PostfixExpr: 
    Expr T_Increm             
    {
        Operator *op = new Operator(@2, "++");
        $$ = new PostfixExpr(op, $1);
    } |

    Expr T_Decrem             
    {
        Operator *op = new Operator(@2, "--");
        $$ = new PostfixExpr(op, $1);
    };

RelationalExpr: 
    Expr '>' Expr                 
    {
        Operator *op = new Operator(@2, ">");
        $$ = new RelationalExpr($1, op, $3);
    } |

    Expr '<' Expr                 
    {
        Operator *op = new Operator(@2, "<");
        $$ = new RelationalExpr($1, op, $3);
    } |

    Expr T_GreaterEqual Expr     
    {
        Operator *op = new Operator(@2, ">=");
        $$ = new RelationalExpr($1, op, $3);
    } |

    Expr T_LessEqual Expr         
    {
        Operator *op = new Operator(@2, "<=");
        $$ = new RelationalExpr($1, op, $3);
    };

EqualityExpr: 
    Expr T_Equal Expr             
    {
        Operator *op = new Operator(@2, "==");
        $$ = new EqualityExpr($1, op, $3);
    } |

    Expr T_NotEqual Expr         
    {
        Operator *op = new Operator(@2, "!=");
        $$ = new EqualityExpr($1, op, $3);
    };

LogicalExpr: 
    Expr T_And Expr                 
    {
        Operator *op = new Operator(@2, "&&");
        $$ = new LogicalExpr($1, op, $3);
    } |

    Expr T_Or Expr                 
    {
        Operator *op = new Operator(@2, "||");
        $$ = new LogicalExpr($1, op, $3);
    } |

    '!' Expr                         
    {
        Operator *op = new Operator(@1, "!");
        $$ = new LogicalExpr(op, $2);
    };

LValue: 
    Identifier 
    {
        $$= new FieldAccess(NULL, $1);
    } |

    Expr '.' Identifier 
    {
        $$ = new FieldAccess($1, $3);
    } |

    Expr '[' Expr ']' 
    {
        $$ = new ArrayAccess(@1, $1, $3);
    };

Call: 
    Identifier '(' Actuals ')' 
    {
        $$ = new Call(@1, NULL, $1, $3);
    } |

    Expr '.' Identifier '(' Actuals ')'   
    {
        $$ = new Call(@1, $1, $3, $5);
    };

Actuals: 
    ExprList 
    {
        $$=$1;
    } |

    /*Empty*/ 
    {
        $$ = new List<Expr*>;
    };

%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/* Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = false;
}
