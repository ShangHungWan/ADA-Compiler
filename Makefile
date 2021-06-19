all: main

main: lex.l yacc.y
	lex lex.l
	bison -d yacc.y -b y
	g++ y.tab.c symbolTable.cpp -ll -ly -o main

.PHONY: clean
clean:
	rm lex.yy.c y.tab.* main