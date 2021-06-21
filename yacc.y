%{
#include "lex.yy.c"
#include "symbolTable.h"

#include <fstream>
#include <stack>
#include <map>

#define Trace(t) printf(t)

void check();
void yyerror(string);
void appendLine(string);
string typeMapping(int);

SymbolTable *scope;
Symbol *symbol;
vector<int> parameter;

string programName;
string lastId;

stack<int> routeStack;
bool elseRoute;

string fileData;
ofstream ofs;

map<string, int> globalVariables;
vector<int> localVariable;
int localVariableCounter = 0;
int routeCounter = 0;
bool isConstant = false;
%}

/* tokens */
%token BOOLEAN INTEGER STRING
%token CONSTANT
%token PROGRAM DECLARE BEG END ENDIF ENDLOOP
%token PROCEDURE RETURN
%token IF THEN ELSE
%token WHILE LOOP
%token FOR IN
%token BREAK CASE CONTINUE DO
%token FALSE TRUE
%token PRINT PRINTLN
%token AND OR NOT
%token ADD SUB MUL DIV
%token LE LE_EQ EQUA GR_EQ GE N_EQUA
%token SEMI ASSI COLO COMM RANG L_PARE R_PARE

%union {
    int intVal;
    char* stringVal;
    returnNode* node;
}

%token <intVal> INT_VAL
%token <stringVal> STRING_VAL
%token <stringVal> ID

%type <intVal> assignment_type type integer_expression boolean_expression
%type <node> expression statement function_invocation

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
                        programName = $2;

                        appendLine("class " + string($2) + " {");
                        appendLine("method public static void main(java.lang.String[])");
                        appendLine("max_stack 15");
                        appendLine("max_locals 15");
                        appendLine("{");
                    } block END ID {
                        delete scope;

                        appendLine("return");
                        appendLine("}");
                        appendLine("}");
                        ofs << fileData;
                        ofs.close();
                    }
                ;

declare:            /* zero declarations */
                |   DECLARE variables_declarations
                |   procedures
                |   DECLARE variables_declarations procedures
                ;

variables_declarations:
                    /* zero declarations */
                |   variables_declarations variables
                |   variables
                ;

variables:
                    variable
                |   constant
                ;

procedures:
                    /* zero declarations */
                |   procedures procedure
                |   procedure
                ;

procedure:          PROCEDURE ID {
                        if (scope->localLookUp($2) == NULL) { 
                            // new a table
                            scope->insert($2, NONE_TYPE, FUNC_KIND, GLOBAL);
                            symbol = scope->localLookUp($2);
                            SymbolTable *child = new SymbolTable($2, scope);
                            scope = child;
                            
                            
                            appendLine("method public static TYPE " + scope->name + "(PARMS)");
                            appendLine("max_stack 15");
                            appendLine("max_locals 15");
                            appendLine("{");
                        } else {
                            yyerror("\'" + string($2) + "\' is already declared.");
                        }
                    } optional_formal_arguments optional_return block END ID SEMI {
                        // drop table when leave
                        SymbolTable *dropScope = scope;
                        scope = scope->parent;
                        delete dropScope;

                        if (scope->returnType == NONE_TYPE) {
                            appendLine("return");
                        } else {
                            appendLine("TYPEreturn");
                        }
                        appendLine("}");
                    }
                ;

type:               BOOLEAN { $$ = BOOL_TYPE; }
                |   INTEGER { $$ = INT_TYPE; }
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
                        scope->insert($1, $2, VARIABLE_KIND, LOCAL, "", -1, localVariableCounter++);
                    }
                ;

assignment_type:    COLO type {
                        $$ = $2;
                    }
                ;

block:              declare BEG statements END SEMI
                ;

conditional:        IF boolean_expression  {
                        elseRoute = false;

                        routeStack.push(routeCounter + 1);
                        routeStack.push(routeCounter);
                        routeStack.push(routeCounter + 1);
                        routeStack.push(routeCounter);

                        appendLine("ifeq R" + to_string(routeStack.top()));
                        routeStack.pop();

                        routeCounter += 2;
                    } THEN block_or_simple optional_else ENDIF SEMI {
                        if (elseRoute) {
                            appendLine("R" + to_string(routeStack.top()) + ":");
                        }
                        routeStack.pop();
                    }
                ;

optional_else:  {
                    routeStack.pop();
                    appendLine("R" + to_string(routeStack.top()) + ":");
                    routeStack.pop();
                }
                |   ELSE {
                    elseRoute = true;

                    appendLine("goto R" + to_string(routeStack.top()) + ":");
                    routeStack.pop();
                    appendLine("R" + to_string(routeStack.top()) + ":");
                    routeStack.pop();
                } block_or_simple
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

loop:               WHILE {
                        routeStack.push(routeCounter + 1);
                        routeStack.push(routeCounter);
                        routeStack.push(routeCounter + 1);
                        routeStack.push(routeCounter);

                        appendLine("R" + to_string(routeStack.top()) + ":");
                        routeStack.pop();

                        routeCounter += 2;
                    } boolean_expression LOOP {
                        appendLine("ifeq R" + to_string(routeStack.top()));
                        routeStack.pop();
                    } block_or_simple ENDLOOP SEMI {
                        appendLine("goto R" + to_string(routeStack.top()));
                        routeStack.pop();
                        appendLine("R" + to_string(routeStack.top()) + ":");
                        routeStack.pop();
                    }
                |   FOR L_PARE ID {
                        symbol = scope->lookUp($3);
                        if (symbol == NULL) {
                            yyerror("\'" + string($3) + "\' does not declared.");
                        } else if (symbol->type != INT_TYPE) {
                            yyerror("\'" + string($3) + "\' is not integer.");
                        }
                    } IN integer_expression {
                        symbol = scope->lookUp($3);
                        if (symbol->scope == GLOBAL) {
                            appendLine("putstatic " + typeMapping(symbol->type) + " " + programName + " " + symbol->name);
                        } else {
                            appendLine("istore " + to_string(symbol->index));
                        }
                        
                        routeStack.push(routeCounter + 1);
                        routeStack.push(routeCounter);
                        routeStack.push(routeCounter + 1);
                        routeStack.push(routeCounter);
                        
                        appendLine("R" + to_string(routeStack.top()) + ":");
                        routeStack.pop();

                        routeCounter += 2;
                        
                        symbol = scope->lookUp($3);
                        if (symbol->scope == GLOBAL) {
                            appendLine("getstatic " + typeMapping(symbol->type) + " " + programName + " " + symbol->name);
                        } else {
                            appendLine("iload " + to_string(symbol->index));
                        }
                    } RANG integer_expression R_PARE LOOP {
                        int routeId = routeCounter;
                        appendLine("isub");
                        appendLine("ifle R" + to_string(routeId));
                        appendLine("iconst_0");
                        appendLine("goto R" + to_string(routeId + 1));
                        appendLine("R" + to_string(routeId) + ": iconst_1");
                        appendLine("R" + to_string(routeId + 1) + ":");
                        routeCounter += 2;

                        appendLine("ifeq R" + to_string(routeStack.top()));
                        routeStack.pop();
                    } block_or_simple {
                        appendLine("goto R" + to_string(routeStack.top()));
                        routeStack.pop();
                        appendLine("R" + to_string(routeStack.top()) + ":");
                        routeStack.pop();
                    } ENDLOOP SEMI
                ;

statements:         /* zero statement */
                |   statement statements
                ;

statement:          ID ASSI {
                        lastId = $1;
                    } expression SEMI {
                        symbol = scope->lookUp(lastId);
                        if (symbol != NULL) {
                            if (symbol->kind == VARIABLE_KIND) {
                                if (symbol->type == NONE_TYPE || symbol->type == $4->type) {
                                    // if not default type or types are same
                                    $$ = $4;
                                    symbol->value = $4->value;
                                    if (symbol->scope == GLOBAL) {
                                        appendLine("putstatic " + typeMapping(symbol->type) + " " + programName + "." + symbol->name);
                                    } else {
                                        appendLine("istore " + to_string(symbol->index));
                                    }
                                } else {
                                    yyerror("Types are not equal.");
                                }
                            } else {
                                yyerror("\'" + lastId + "\' is not variable.");
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("\'" + lastId + "\' does not declared.");
                        }
                    }
                |   PRINT {
                        appendLine("getstatic java.io.PrintStream java.lang.System.out");
                    } expression SEMI {
                        if ($3->type == ERROR) {
                            yyerror("Can't print.");
                        } else {
                            if ($3->type == INT_TYPE) {
                                appendLine("invokevirtual void java.io.PrintStream.print(int)");
                            } else if ($3->type == BOOL_TYPE) {
                                appendLine("invokevirtual void java.io.PrintStream.print(boolean)");
                            } else {
                                appendLine("invokevirtual void java.io.PrintStream.print(java.lang.String)");
                            }
                        }
                    }
                |   PRINTLN {
                        appendLine("getstatic java.io.PrintStream java.lang.System.out");
                    } expression SEMI {
                        if ($3->type == ERROR) {
                            yyerror("Can't print.");
                        } else {
                            if ($3->type == INT_TYPE) {
                                appendLine("invokevirtual void java.io.PrintStream.println(int)");
                            } else if ($3->type == BOOL_TYPE) {
                                appendLine("invokevirtual void java.io.PrintStream.println(boolean)");
                            } else {
                                appendLine("invokevirtual void java.io.PrintStream.println(java.lang.String)");
                            }
                        }
                    }
                |   RETURN SEMI {
                        if (scope->returnType != NONE_TYPE) {
                            yyerror(scope->name + " needs return value.");
                        }
                    }
                |   RETURN expression SEMI {
                        if (scope->returnType != NONE_TYPE) {
                            if (scope->returnType != $2->type) {
                                yyerror("Function type and Return type are not same.");
                            }
                        } else {
                            yyerror(scope->name + " doesn't need return value.");
                        }
                    }
                |   procedure_invocation
                |   block
                |   conditional
                |   loop
                ;

expression:         SUB expression %prec UMINUS {
                        $$ = $2;
                        if ($2->type == INT_TYPE) {
                            $$->type = INT_TYPE;
                            $$->value = -$2->value;
                            appendLine("ineg");
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use UMINUS operator on this type.");
                        }
                    }
                |   expression MUL expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type == INT_TYPE && $3->type == INT_TYPE) {
                            if ($1->type == $3->type) {
                                $$->value = $1->value * $3->value;
                                appendLine("imul");
                            } else {
                                $$->type = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use MUL operator on this type.");
                        }
                    }
                |   expression DIV expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type == INT_TYPE && $3->type == INT_TYPE) {
                            if ($1->type == $3->type && $3->value != 0) {
                                $$->value = $1->value / $3->value;
                                appendLine("idiv");
                            } else {
                                $$->type = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use DIV operator on this type.");
                        }
                    }
                |   expression ADD expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type == INT_TYPE && $3->type == INT_TYPE) {
                            if ($1->type == $3->type) {
                                $$->value = $1->value + $3->value;
                                appendLine("iadd");
                            } else {
                                $$->type = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use ADD operator on this type.");
                        }
                    }
                |   expression SUB expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type == INT_TYPE && $3->type == INT_TYPE) {
                            if ($1->type == $3->type) {
                                $$->value = $1->value - $3->value;
                                appendLine("isub");
                            } else {
                                $$->type = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use SUB operator on this type.");
                        }
                    }
                |   expression LE expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type == INT_TYPE && $3->type == INT_TYPE) {
                            if ($1->type == $3->type) {
                                $$->type = BOOL_TYPE;
                                $$->value = $1->value < $3->value;

                                int routeId = routeCounter;
                                appendLine("isub");
                                appendLine("iflt R" + to_string(routeId));
                                appendLine("iconst_0");
                                appendLine("goto R" + to_string(routeId + 1));
                                appendLine("R" + to_string(routeId) + ": iconst_1");
                                appendLine("R" + to_string(routeId + 1) + ":");
                                routeCounter += 2;
                            } else {
                                $$->type = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use LE operator on this type.");
                        }
                    }
                |   expression LE_EQ expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type == INT_TYPE && $3->type == INT_TYPE) {
                            if ($1->type == $3->type) {
                                $$->type = BOOL_TYPE;
                                $$->value = $1->value <= $3->value;

                                int routeId = routeCounter;
                                appendLine("isub");
                                appendLine("ifle R" + to_string(routeId));
                                appendLine("iconst_0");
                                appendLine("goto R" + to_string(routeId + 1));
                                appendLine("R" + to_string(routeId) + ": iconst_1");
                                appendLine("R" + to_string(routeId + 1) + ":");
                                routeCounter += 2;
                            } else {
                                $$->type = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use LE_EQ operator on this type.");
                        }
                    }
                |   expression GE expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type == INT_TYPE && $3->type == INT_TYPE) {
                            if ($1->type == $3->type) {
                                $$->type = BOOL_TYPE;
                                $$->value = int($1->value > $3->value);

                                int routeId = routeCounter;
                                appendLine("isub");
                                appendLine("ifgt R" + to_string(routeId));
                                appendLine("iconst_0");
                                appendLine("goto R" + to_string(routeId + 1));
                                appendLine("R" + to_string(routeId) + ": iconst_1");
                                appendLine("R" + to_string(routeId + 1) + ":");
                                routeCounter += 2;
                            } else {
                                $$->type = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use GE operator on this type.");
                        }
                    }
                |   expression GR_EQ expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type == INT_TYPE && $3->type == INT_TYPE) {
                            if ($1->type == $3->type) {
                                $$->type = BOOL_TYPE;
                                $$->value = $1->value >= $3->value;

                                int routeId = routeCounter;
                                appendLine("isub");
                                appendLine("ifge R" + to_string(routeId));
                                appendLine("iconst_0");
                                appendLine("goto R" + to_string(routeId + 1));
                                appendLine("R" + to_string(routeId) + ": iconst_1");
                                appendLine("R" + to_string(routeId + 1) + ":");
                                routeCounter += 2;
                            } else {
                                $$->type = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use GR_EQ operator on this type.");
                        }
                    }
                |   expression EQUA expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type != NONE_TYPE && $1->type != ERROR && $3->type != NONE_TYPE && $3->type != ERROR) {
                            if ($1->type == $3->type) {
                                $$->type = BOOL_TYPE;
                                $$->value = $1->value == $3->value;

                                int routeId = routeCounter;
                                appendLine("isub");
                                appendLine("ifeq R" + to_string(routeId));
                                appendLine("iconst_0");
                                appendLine("goto R" + to_string(routeId + 1));
                                appendLine("R" + to_string(routeId) + ": iconst_1");
                                appendLine("R" + to_string(routeId + 1) + ":");
                                routeCounter += 2;
                            } else {
                                $$->type = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use EUQA operator on this type.");
                        }
                    }
                |   expression N_EQUA expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type != NONE_TYPE && $1->type != ERROR && $3->type != NONE_TYPE && $3->type != ERROR) {
                            if ($1->type == $3->type) {
                                $$->type = BOOL_TYPE;
                                $$->value = $1->value != $3->value;
                                
                                int routeId = routeCounter;
                                appendLine("isub");
                                appendLine("ifne R" + to_string(routeId));
                                appendLine("iconst_0");
                                appendLine("goto R" + to_string(routeId + 1));
                                appendLine("R" + to_string(routeId) + ": iconst_1");
                                appendLine("R" + to_string(routeId + 1) + ":");
                                routeCounter += 2;
                            } else {
                                $$->type = ERROR;
                                yyerror("Types are not equal.");    
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use N_EQUA operator on this type.");
                        }
                    }
                |   expression AND expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type == BOOL_TYPE && $3->type == BOOL_TYPE) {
                            $$->type = BOOL_TYPE;
                            $$->value = $1->value && $3->value;
                            appendLine("iand");
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use AND operator on this type.");
                        }
                    }
                |   expression OR expression {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        if ($1->type == BOOL_TYPE && $3->type == BOOL_TYPE) {
                            $$->type = BOOL_TYPE;
                            $$->value = $1->value && $3->value;
                            appendLine("ior");
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use OR operator on this type.");
                        }
                    }
                |   NOT expression {
                        $$ = $2;
                        if ($2->type == BOOL_TYPE) {
                            $$->type = BOOL_TYPE;
                            $$->value = !$2->value;
                            appendLine("ixor");
                        } else {
                            $$->type = ERROR;
                            yyerror("Can't use NOT operator on this type.");
                        }
                    }
                |   L_PARE expression R_PARE    { $$ = $2; }
                |   INT_VAL {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        $$->type = INT_TYPE;
                        $$->value = $1;
                        $$->str = "";
                        if (!isConstant) {
                            appendLine("sipush " + to_string($1));
                        }
                    }
                |   STRING_VAL {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        $$->type = STRING_TYPE;
                        $$->str = $1;
                        if (!isConstant) {
                            appendLine("idc \"" + string($1) + "\"");
                        }
                    }
                |   FALSE {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        $$->type = BOOL_TYPE;
                        $$->value = 0;
                        $$->str = "";
                        if (!isConstant) {
                            appendLine("sipush 0");
                        }
                    }
                |   TRUE {
                        $$ = (returnNode*)malloc(sizeof(returnNode*));
                        $$->type = BOOL_TYPE;
                        $$->value = 1;
                        $$->str = "";
                        if (!isConstant) {
                            appendLine("sipush 1");
                        }
                    }
                |   function_invocation         { $$ = $1; }
                |   ID {
                        symbol = scope->lookUp($1);
                        if (symbol != NULL) {
                            $$ = (returnNode*)malloc(sizeof(returnNode*));
                            $$->type = symbol->type;
                            $$->value = symbol->value;
                            $$->str = &symbol->str[0];

                            if (symbol->kind == VARIABLE_KIND) {
                                // only variable or constant can be expression as ID
                                if (symbol->scope == GLOBAL) {
                                    appendLine("getstatic int " + programName + "." + symbol->name);
                                } else {
                                    appendLine("iload " + to_string(symbol->index));
                                }
                            } else if (symbol->kind == CONST_KIND) {
                                if (symbol->type == INT_TYPE) {
                                    appendLine("sipush " + to_string(symbol->value));
                                } else if (symbol->type == BOOL_TYPE) {
                                    appendLine("iconst_" + to_string(symbol->value));
                                } else if (symbol->type == STRING_TYPE) {
                                    appendLine("idc \"" + string(symbol->str) + "\"");
                                }
                            } else {
                                $$->type = ERROR;
                                yyerror("\'" + string($1) + "\' is not variable or constant.");    
                            }
                        } else {
                            $$->type = ERROR;
                            yyerror("\'" + string($1) + "\' does not declared.");
                        }
                    }
                ;

integer_expression:
                    expression {
                        if ($1->type != INT_TYPE) {
                            yyerror("This type isn't integer.");
                        }
                        $$ = $1->value;
                    }
                ;

boolean_expression:
                    expression {
                        if ($1->type != BOOL_TYPE) {
                            yyerror("This type isn't boolean.");
                        }
                        $$ = $1->value;
                    }
                ;

function_invocation:
                    ID L_PARE R_PARE {
                        symbol = scope->lookUp($1);
                        if (symbol == NULL) {
                            $$->type = ERROR;
                            yyerror("\'" + string($1) + "\' does not declared.");
                        } else {
                            if (symbol->type == NONE_TYPE) {
                                $$->type = ERROR;
                                yyerror("\'" + string($1) + "\' is a non-return method.");
                            } else {
                                $$->type = symbol->type;
                            }
                        }
                    }
                |   ID L_PARE {
                        parameter.clear();
                    } comma_separated_expressions R_PARE {
                        symbol = scope->lookUp($1);
                        if (symbol != NULL) {
                            if (symbol->type == NONE_TYPE) {
                                $$->type = ERROR;
                                yyerror("\'" + string($1) + "\' is a non-return method.");
                            } else {
                                $$->type = symbol->type;

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
                                        $$->type = ERROR;
                                        yyerror("\'" + string($1) + "\' parameter is not match.");
                                    }
                                } else {
                                    $$->type = ERROR;
                                    yyerror("\'" + string($1) + "\' parameter is not match.");
                                }
                            }
                        } else {
                            $$->type = ERROR;
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
                        parameter.push_back($1->type);
                    }
                |   expression {
                    parameter.push_back($1->type);
                } COMM comma_separated_expressions
                ;

constant:           ID COLO CONSTANT ASSI {
                        lastId = $1;
                        isConstant = true;
                    } expression SEMI {
                        isConstant = false;
                        if (scope->localLookUp(lastId) != NULL) {
                            yyerror("\'" + string(lastId) + "\' is already declared.");
                        } else {
                            scope->insert(lastId, $6->type, CONST_KIND, scope->parent != NULL, $6->str, $6->value);
                        }
                    }
                |   ID COLO CONSTANT assignment_type ASSI {
                        lastId = $1;
                        isConstant = true;
                    } expression SEMI {
                        isConstant = false;
                        if (scope->localLookUp(lastId) != NULL) {
                            yyerror("\'" + lastId + "\' is already declared.");
                        } else {
                            if ($4 != $7->type) {
                                yyerror("Types are not equal");
                            } else {
                                scope->insert(lastId, $4, CONST_KIND, scope->parent != NULL, $7->str, $7->value);
                            }
                        }
                    };

variable:           ID SEMI {
                        if (scope->localLookUp($1) != NULL) {
                            yyerror("\'" + string($1) + "\' is already declared.");
                        } else {
                            int index = -1;
                            if (scope->parent == NULL) {
                                appendLine("field static int " + string($1));
                                globalVariables[$1] = -1;
                            } else {
                                index = localVariableCounter++;
                            }
                            scope->insert($1, INT_TYPE, VARIABLE_KIND, scope->parent != NULL, "", -1, index);
                        }
                    }
                |   ID assignment_type SEMI {
                        if (scope->localLookUp($1) != NULL) {
                            yyerror("\'" + string($1) + "\' is already declared.");
                        } else {
                            int index = -1;
                            if (scope->parent == NULL) {
                                appendLine("field static " + typeMapping($2) + " " + string($1));
                                globalVariables[$1] = -1;
                            } else {
                                index = localVariableCounter++;
                            }
                            scope->insert($1, $2, VARIABLE_KIND, scope->parent != NULL, "", -1, index);
                        }
                    }
                |   ID ASSI {
                        lastId = $1;
                    } expression SEMI {
                        if (scope->localLookUp(lastId) != NULL) {
                            yyerror("\'" + string(lastId) + "\' is already declared.");
                        } else {
                            int index = -1;
                            if (scope->parent == NULL) {
                                appendLine("field static " + typeMapping($4->type) + " " + lastId + " = " + to_string($4->value));
                                globalVariables[lastId] = $4->value;
                            } else {
                                // appendLine("sipush " + to_string($3->value));
                                appendLine("istore " + to_string(localVariableCounter));
                                index = localVariableCounter++;
                            }
                            scope->insert(lastId, $4->type, VARIABLE_KIND, scope->parent != NULL, "", $4->value, index);
                        }
                    }
                |   ID assignment_type ASSI {
                        lastId = $1;
                    } expression SEMI {
                        if (scope->localLookUp(lastId) != NULL) {
                            yyerror("\'" + string(lastId) + "\' is already declared.");
                        } else {
                            if ($2 != $5->type) {
                                yyerror("Types are not equal");
                            } else {
                                int index = -1;
                                if (scope->parent == NULL) {
                                    appendLine("field static " + typeMapping($2) + " " + lastId + " = " + to_string($5->value));
                                    globalVariables[lastId] = $5->value;
                                } else {
                                    // appendLine("sipush " + to_string($4->value));
                                    appendLine("istore " + to_string(localVariableCounter));
                                    index = localVariableCounter++;
                                }
                                scope->insert(lastId, $2, VARIABLE_KIND, scope->parent != NULL, "", $5->value, index);
                            }
                        }
                    }
                ;
%%

string typeMapping(int type) {
    if (type == INT_TYPE) {
        return "int";
    } else if (type == BOOL_TYPE) {
        return "boolean";
    } else {
        return "";
    }
}

void appendLine(string line) {
    fileData += line + "\n";
}

void yyerror(string msg) {
    cout << msg << endl;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf ("Usage: sc filename\n");
        exit(1);
    }
    yyin = fopen(argv[1], "r");

    string fileName = string(argv[1]).substr(0, string(argv[1]).find_last_of("."));
    ofs.open(fileName + ".jasm");

    fileData = "";

    yyparse();
    return 0;
}
