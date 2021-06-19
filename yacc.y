%{
#include "lex.yy.c"
#include "symbolTable.h"

#define Trace(t) printf(t)

void check();
void yyerror(string);

SymbolTable *scope;
Symbol *symbol;
vector<int> parameter;
%}

/* tokens */
%token BOOLEAN FLOAT INTEGER STRING
%token CONSTANT
%token PROGRAM DECLARE BEG END ENDIF ENDLOOP
%token PROCEDURE RETURN
%token IF THEN ELSE
%token WHILE LOOP
%token FOR IN
%token BREAK CASE CONTINUE DO
%token FALSE TRUE
%token PRINT PRINTLN READ
%token AND OR NOT
%token ADD SUB MUL DIV
%token LE LE_EQ EQUA GR_EQ GE N_EQUA
%token SEMI ASSI COLO COMM RANG L_PARE R_PARE L_SQUA_BRAC R_SQUA_BRAC

%union {
    bool boolVal;
    int intVal;
    float floatVal;
    char* stringVal;
}

%token <boolVal> BOOL_VAL
%token <intVal> INT_VAL
%token <floatVal> FLOAT_VAL
%token <stringVal> STRING_VAL
%token <stringVal> ID

%type <intVal> assignment_type type expression array_reference function_invocation statement

/* operators */
%left OR
%left AND
%left NOT
%left LE LE_EQ EQUA GR_EQ GE N_EQUA
%left ADD SUB
%left MUL DIV
%nonassoc UMINUS

%start program

%%
program:            PROGRAM ID {
                        scope = new SymbolTable($2, NULL);
                    } block END ID {
                        delete scope;
                    }
                ;

declare:            /* zero declarations */
                |   DECLARE declarations
                ;

declarations:       variables_declarations procedure_declarations
                ;

variables_declarations:
                    /* zero declarations */
                |   variables_declarations variables
                |   variables
                ;

variables:
                    variable
                |   constant
                |   array
                ;

procedure_declarations:
                    /* zero declarations */
                |   procedure_declarations procedure
                |   procedure
                ;

procedure:          PROCEDURE ID {
                        if (scope->localLookUp($2) == NULL) { 
                            // new a table
                            scope->insert($2, NONE_TYPE, FUNC_KIND);
                            symbol = scope->localLookUp($2);
                            SymbolTable *child = new SymbolTable($2, scope);
                            scope = child;
                        } else {
                            yyerror("\'" + string($2) + "\' is already declared.");
                        }
                    } optional_formal_arguments optional_return block SEMI END ID SEMI {
                        // drop table when leave
                        SymbolTable *dropScope = scope;
                        scope = scope->parent;
                        delete dropScope;
                    }
                ;

type:               BOOLEAN { $$ = BOOL_TYPE; }
                |   INTEGER { $$ = INT_TYPE; }
                |   FLOAT   { $$ = FLOAT_TYPE; }
                |   STRING  { $$ = STRING_TYPE; }
                ;

optional_return:    
                |   RETURN type {
                    symbol->type = $2;
                    scope->returnType = $2;
                }
                ;

optional_formal_arguments:
                |   formal_arguments
                ;

formal_arguments:   L_PARE arguments R_PARE
                ;

arguments:          arguments SEMI arguments
                |   ID assignment_type {
                        // append argument
                        symbol->argumentType.push_back($2);
                        scope->insert($1, $2, VARIABLE_KIND);
                    }
                ;

assignment_type:    COLO type {
                        $$ = $2;
                    }
                ;

block:              declare BEG statements END
                ;

conditional:        IF boolean_expression THEN block_or_simple ENDIF SEMI
                |   IF boolean_expression THEN block_or_simple ELSE block_or_simple ENDIF SEMI
                ;

block_or_simple:    {
                        SymbolTable *child = new SymbolTable("anonymous", scope);
                        scope = child;
                    } block {
                        // drop table when leave
                        SymbolTable *dropScope = scope;
                        scope = scope->parent;
                        delete dropScope;
                    }
                |   statement
                ;

loop:               WHILE boolean_expression LOOP block_or_simple ENDLOOP SEMI
                |   FOR L_PARE ID {
                        symbol = scope->lookUp($3);
                        if (symbol == NULL) {
                            yyerror("\'" + string($3) + "\' does not declared.");
                        } else if (symbol->type != INT_TYPE) {
                            yyerror("\'" + string($3) + "\' is not integer.");
                        }
                    } IN integer_expression RANG integer_expression R_PARE LOOP block_or_simple ENDLOOP SEMI
                ;

statements:         /* zero statement */
                |   statement statements
                ;

statement:          ID ASSI expression SEMI {
                        symbol = scope->lookUp($1);
                        if (symbol != NULL) {
                            if (symbol->kind == VARIABLE_KIND) {
                                if (symbol->type == NONE_TYPE || symbol->type == $3) {
                                    // if not default type or types are same
                                    $$ = $3;
                                } else {
                                    yyerror("Types are not equal.");
                                }
                            } else {
                                yyerror("\'" + string($1) + "\' is not variable.");
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("\'" + string($1) + "\' does not declared.");
                        }
                    }
                |   ID L_SQUA_BRAC integer_expression R_SQUA_BRAC ASSI expression SEMI {
                        symbol = scope->lookUp($1);
                        if (symbol != NULL) {
                            if (symbol->kind == ARRAY_KIND) {
                                if (symbol->type == NONE_TYPE || symbol->type == $6) {
                                    // if not default type or types are same
                                    $$ = $6;
                                } else {
                                    yyerror("Types are not equal.");
                                }
                            } else {
                                yyerror("\'" + string($1) + "\' is not array.");
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("\'" + string($1) + "\' does not declared.");
                        }
                    }
                |   PRINT expression SEMI {
                        if ($2 == ERROR) {
                            yyerror("Can't print.");
                        }
                    }
                |   PRINTLN expression SEMI {
                        if ($2 == ERROR) {
                            yyerror("Can't print.");
                        }
                    }
                |   READ ID SEMI {
                        symbol = scope->lookUp($2);
                        if (symbol == NULL) {
                            yyerror("\'" + string($2) + "\' does not declared.");
                        }
                    }
                |   RETURN SEMI {
                        if (scope->returnType != NONE_TYPE) {
                            yyerror(scope->name + " needs return value.");
                        }
                    }
                |   RETURN expression SEMI {
                        if (scope->returnType != NONE_TYPE) {
                            if (scope->returnType != $2) {
                                yyerror("Function type and Return type are not same.");
                            }
                        } else {
                            yyerror(scope->name + " doesn't need return value.");
                        }
                    }
                |   procedure_invocation
                |   block SEMI
                |   conditional
                |   loop
                ;

expression:         SUB expression %prec UMINUS {
                        if ($2 == INT_TYPE || $2 == FLOAT_TYPE) {
                            $$ = $2;
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use UMINUS operator on this type.");
                        }
                    }
                |   expression MUL expression {
                        if (($1 == INT_TYPE || $1 == FLOAT_TYPE) && ($3 == INT_TYPE || $3 == FLOAT_TYPE)) {
                            if ($1 == $3) {
                                $$ = $1;
                            } else {
                                $$ = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use MUL operator on this type.");
                        }
                    }
                |   expression DIV expression {
                        if (($1 == INT_TYPE || $1 == FLOAT_TYPE) && ($3 == INT_TYPE || $3 == FLOAT_TYPE)) {
                            if ($1 == $3) {
                                $$ = $1;
                            } else {
                                $$ = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use DIV operator on this type.");
                        }
                    }
                |   expression ADD expression {
                        if (($1 == INT_TYPE || $1 == FLOAT_TYPE) && ($3 == INT_TYPE || $3 == FLOAT_TYPE)) {
                            if ($1 == $3) {
                                $$ = $1;
                            } else {
                                $$ = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use ADD operator on this type.");
                        }
                    }
                |   expression SUB expression {
                        if (($1 == INT_TYPE || $1 == FLOAT_TYPE) && ($3 == INT_TYPE || $3 == FLOAT_TYPE)) {
                            if ($1 == $3) {
                                $$ = $1;
                            } else {
                                $$ = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use SUB operator on this type.");
                        }
                    }
                |   expression LE expression {
                        if (($1 == INT_TYPE || $1 == FLOAT_TYPE) && ($3 == INT_TYPE || $3 == FLOAT_TYPE)) {
                            if ($1 == $3) {
                                $$ = BOOL_TYPE;
                            } else {
                                $$ = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use LE operator on this type.");
                        }
                    }
                |   expression LE_EQ expression {
                        if (($1 == INT_TYPE || $1 == FLOAT_TYPE) && ($3 == INT_TYPE || $3 == FLOAT_TYPE)) {
                            if ($1 == $3) {
                                $$ = BOOL_TYPE;
                            } else {
                                $$ = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use LE_EQ operator on this type.");
                        }
                    }
                |   expression GE expression {
                        if (($1 == INT_TYPE || $1 == FLOAT_TYPE) && ($3 == INT_TYPE || $3 == FLOAT_TYPE)) {
                            if ($1 == $3) {
                                $$ = BOOL_TYPE;
                            } else {
                                $$ = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use GE operator on this type.");
                        }
                    }
                |   expression GR_EQ expression {
                        if (($1 == INT_TYPE || $1 == FLOAT_TYPE) && ($3 == INT_TYPE || $3 == FLOAT_TYPE)) {
                            if ($1 == $3) {
                                $$ = BOOL_TYPE;
                            } else {
                                $$ = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use GR_EQ operator on this type.");
                        }
                    }
                |   expression EQUA expression {
                        if ($1 != NONE_TYPE && $1 != ERROR && $3 != NONE_TYPE && $3 != ERROR) {
                            if ($1 == $3) {
                                $$ = BOOL_TYPE;
                            } else {
                                $$ = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use EUQA operator on this type.");
                        }
                    }
                |   expression N_EQUA expression {
                        if ($1 != NONE_TYPE && $1 != ERROR && $3 != NONE_TYPE && $3 != ERROR) {
                            if ($1 == $3) {
                                $$ = BOOL_TYPE;
                            } else {
                                $$ = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use N_EQUA operator on this type.");
                        }
                    }
                |   expression AND expression {
                        if ($1 == BOOL_TYPE && $3 == BOOL_TYPE) {
                            $$ = $1;
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use AND operator on this type.");
                        }
                    }
                |   expression OR expression {
                        if ($1 == BOOL_TYPE && $3 == BOOL_TYPE) {
                            $$ = $1;
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use OR operator on this type.");
                        }
                    }
                |   NOT expression {
                        if ($2 == BOOL_TYPE) {
                            $$ = $2;
                        } else {
                            $$ = ERROR;
                            yyerror("Can't use NOT operator on this type.");
                        }
                    }
                |   L_PARE expression R_PARE    { $$ = $2; }
                |   INT_VAL                     { $$ = INT_TYPE; }
                |   STRING_VAL                  { $$ = STRING_TYPE; }
                |   FLOAT_VAL                   { $$ = FLOAT_TYPE; }
                |   BOOL_VAL                    { $$ = BOOL_TYPE; }
                |   FALSE                       { $$ = BOOL_TYPE; }
                |   TRUE                        { $$ = BOOL_TYPE; }
                |   array_reference             { $$ = $1; }
                |   function_invocation         { $$ = $1; }
                |   ID {
                        symbol = scope->lookUp($1);
                        if (symbol != NULL) {
                            if (symbol->kind == VARIABLE_KIND || symbol->kind == CONST_KIND) {
                                // only variable or constant can be expression as ID
                                $$ = symbol->type;
                            } else {
                                $$ = ERROR;
                                yyerror("\'" + string($1) + "\' is not variable or constant.");    
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("\'" + string($1) + "\' does not declared.");
                        }
                    }
                ;

integer_expression:
                    expression {
                        if ($1 != INT_TYPE) {
                            yyerror("This type isn't integer.");
                        }
                    }
                ;

boolean_expression:
                    expression {
                        if ($1 != BOOL_TYPE) {
                            yyerror("This type isn't boolean.");
                        }
                    }
                ;

array_reference:    ID L_SQUA_BRAC integer_expression R_SQUA_BRAC {
                        symbol = scope->lookUp($1);
                        if (symbol != NULL) {
                            if (symbol->kind == ARRAY_KIND) {
                                $$ = symbol->type;
                            } else {
                                $$ = ERROR;
                                yyerror("\'" + string($1) + "\' is not array.");
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("\'" + string($1) + "\' does not declared.");
                        }
                    }
                ;

function_invocation:
                    ID L_PARE R_PARE {
                        symbol = scope->lookUp($1);
                        if (symbol == NULL) {
                            $$ = ERROR;
                            yyerror("\'" + string($1) + "\' does not declared.");
                        } else {
                            if (symbol->type == NONE_TYPE) {
                                $$ = ERROR;
                                yyerror("\'" + string($1) + "\' is a non-return method.");
                            } else {
                                $$ = symbol->type;
                            }
                        }
                    }
                |   ID L_PARE {
                        parameter.clear();
                    } comma_separated_expressions R_PARE {
                        symbol = scope->lookUp($1);
                        if (symbol != NULL) {
                            if (symbol->type == NONE_TYPE) {
                                $$ = ERROR;
                                yyerror("\'" + string($1) + "\' is a non-return method.");
                            } else {
                                $$ = symbol->type;

                                // check if arguments are match (if there have arguments)
                                if (symbol->argumentType.size() == parameter.size()) {
                                    bool anyMismatch = false;
                                    for (int i = 0; i < symbol->argumentType.size(); i++) {
                                        if (symbol->argumentType[i] != parameter[i]) {
                                            anyMismatch = true;
                                            break;
                                        }
                                    }
                                    if (anyMismatch) {
                                        $$ = ERROR;
                                        yyerror("\'" + string($1) + "\' parameter is not match.");
                                    }
                                } else {
                                    $$ = ERROR;
                                    yyerror("\'" + string($1) + "\' parameter is not match.");
                                }
                            }
                        } else {
                            $$ = ERROR;
                            yyerror("\'" + string($1) + "\' does not declared.");
                        }
                    }
                ;

procedure_invocation:
                    ID SEMI {
                        symbol = scope->lookUp($1);
                        if (symbol == NULL) {
                            yyerror("\'" + string($1) + "\' does not declared.");
                        } else {
                            if (symbol->type != NONE_TYPE) {
                                yyerror("\'" + string($1) + "\' is a return method.");
                            }
                            if (symbol->argumentType.size() != 0) {
                                yyerror("\'" + string($1) + "\' needs argument.");
                            } 
                        }
                    }
                |   ID L_PARE comma_separated_expressions R_PARE SEMI {
                        symbol = scope->lookUp($1);
                        if (symbol == NULL) {
                            yyerror("\'" + string($1) + "\' does not declared.");
                        } else {
                            if (symbol->type != NONE_TYPE) {
                                yyerror("\'" + string($1) + "\' is a return method.");
                            }
                            
                            // check if arguments are match (if there have arguments)
                            if (symbol->argumentType.size() != parameter.size()) {
                                yyerror("\'" + string($1) + "\' parameter is not match.");
                            } else {
                                bool anyMismatch = false;
                                for (int i = 0; i < symbol->argumentType.size(); i++) {
                                    if (symbol->argumentType[i] != parameter[i]) {
                                        anyMismatch = true;
                                        break;
                                    }
                                }
                                if (anyMismatch) {
                                    yyerror("\'" + string($1) + "\' parameter is not match.");
                                }
                            }
                        }
                    }
                ;

comma_separated_expressions:
                    expression {
                        parameter.push_back($1);
                    }
                |   expression {
                    parameter.push_back($1);
                } COMM comma_separated_expressions
                ;

constant:           ID COLO CONSTANT ASSI expression SEMI {
                        if (scope->localLookUp($1) != NULL) {
                            yyerror("\'" + string($1) + "\' is already declared.");
                        } else {
                            scope->insert($1, $5, CONST_KIND);
                        }
                    }
                |   ID COLO CONSTANT assignment_type ASSI expression SEMI {
                        if (scope->localLookUp($1) != NULL) {
                            yyerror("\'" + string($1) + "\' is already declared.");
                        } else {
                            if ($4 != $6) {
                                yyerror("Types are not equal");
                            } else {
                                scope->insert($1, $4, CONST_KIND);
                            }
                        }
                    };

variable:           ID SEMI {
                        if (scope->localLookUp($1) != NULL) {
                            yyerror("\'" + string($1) + "\' is already declared.");
                        } else {
                            scope->insert($1, NONE_TYPE, VARIABLE_KIND);
                        }
                    }
                |   ID assignment_type SEMI {
                        if (scope->localLookUp($1) != NULL) {
                            yyerror("\'" + string($1) + "\' is already declared.");
                        } else {
                            scope->insert($1, $2, VARIABLE_KIND);
                        }
                    }
                |   ID ASSI expression SEMI {
                        if (scope->localLookUp($1) != NULL) {
                            yyerror("\'" + string($1) + "\' is already declared.");
                        } else {
                            scope->insert($1, $3, VARIABLE_KIND);
                        }
                    }
                |   ID assignment_type ASSI expression SEMI {
                        if (scope->localLookUp($1) != NULL) {
                            yyerror("\'" + string($1) + "\' is already declared.");
                        } else {
                            if ($2 != $4) {
                                yyerror("Types are not equal");
                            } else {
                                scope->insert($1, $2, VARIABLE_KIND);
                            }
                        }
                    }
                ;

array:              ID COLO type L_SQUA_BRAC INT_VAL R_SQUA_BRAC SEMI {
                        if (scope->localLookUp($1) != NULL) {
                            yyerror("\'" + string($1) + "\' is already declared.");
                        } else {
                            scope->insert($1, $3, ARRAY_KIND);
                        }
                    }
                ;
%%

void yyerror(string msg) {
    cout << msg << endl;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf ("Usage: sc filename\n");
        exit(1);
    }
    yyin = fopen(argv[1], "r");

    yyparse();
}
