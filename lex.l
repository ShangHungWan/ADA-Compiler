%{
#include "symbolTable.h"
#include "y.tab.h"

#define LIST strcat(buf, yytext)
#define token(t) {LIST; return(t);}
#define MAX_LINE_LENG 256

int lineNum = 0;
char buf[MAX_LINE_LENG];
%}

digs        [0-9]+
integer     {digs}
plain_real  {digs}"."{digs}
expreal     {digs}"."{digs}[Ee][+-]?{digs}
real        {plain_real}|{expreal}
id          [a-zA-Z][0-9a-zA-Z]*

%x COMMENT

%%
"{"             { LIST; BEGIN(COMMENT); }
<COMMENT>"}"    { LIST; BEGIN(INITIAL); }
<COMMENT>\n     {
                    LIST; 
                    printf("%d: %s", lineNum++, buf);
                    buf[0] = '\0';
                }
<COMMENT>.      { LIST; }

boolean         { token(BOOLEAN); }
integer         { token(INTEGER); }
string          { token(STRING); }

constant        { token(CONSTANT); }

program         { token(PROGRAM); }
declare         { token(DECLARE); }
begin           { token(BEG); }
end             { token(END); }
"end if"        { token(ENDIF); }
"end loop"      { token(ENDLOOP); }

procedure       { token(PROCEDURE); }
return          { token(RETURN); }

if              { token(IF); }
then            { token(THEN); }
else            { token(ELSE); }

while           { token(WHILE); }
loop            { token(LOOP); }
for             { token(FOR); }
in              { token(IN); }

break           { token(BREAK); }
case            { token(CASE); }
continue        { token(CONTINUE); }
do              { token(DO); }

false           { token(FALSE); }
true            { token(TRUE); }

print           { token(PRINT); }
println         { token(PRINTLN); }

and             { token(AND); }
or              { token(OR); }
not             { token(NOT); }

"+"             { token(ADD); }
"-"             { token(SUB); }
"*"             { token(MUL); }
"/"             { token(DIV); }

"<"             { token(LE); }
"<="            { token(LE_EQ); }
"=>"            { token(GR_EQ); }
">"             { token(GE); }
"/="            { token(N_EQUA); }

","             { token(COMM); }
";"             { token(SEMI); }
"="             { token(EQUA); }
":="            { token(ASSI); }
":"             { token(COLO); }
".."            { token(RANG); }
"("             { token(L_PARE); }
")"             { token(R_PARE); }

{id}            {
                    yylval.stringVal = strdup(yytext);
                    token(ID);
                }

{integer}       {
                    sscanf(yytext, "%d", &yylval.intVal);
                    token(INT_VAL);
                }

\".*\"          {
                    char tmp[MAX_LINE_LENG] = "";
                    for (int i = 1, idx = 0; i < yyleng - 1; i++) {
                        if (yytext[i] == '"' && yytext[i+1] == '"') {
                            tmp[idx++] = '"';
                            i++;
                        } else {
                            tmp[idx++] = yytext[i];
                        }
                    }
                    strcpy(yytext, tmp);
                    yylval.stringVal = strdup(yytext);
                    token(STRING_VAL);
                }

<<EOF>>         {
                    LIST;
                    printf("%d: %s", lineNum++, buf);
                    yyterminate();
                }
--.*            { LIST; }
\n				{
                    LIST;
                    printf("%d: %s", lineNum++, buf);
                    buf[0] = '\0';
                }
.				{ LIST; }
%%