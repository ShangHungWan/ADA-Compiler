all: main

main: project1.l project2.y
	lex project1.l
	bison -d project2.y -b y
	g++ y.tab.c symbolTable.cpp -ll -ly -o main

.PHONY: clean
clean:
	rm lex.yy.c y.tab.* main