#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include <iostream>
#include <string>
#include <vector>

using namespace std;

#define ERROR -1

// TYPE
#define NONE_TYPE 0
#define INT_TYPE 1
#define FLOAT_TYPE 2
#define BOOL_TYPE 3
#define STRING_TYPE 4

// KIND
#define NONE_KIND 0
#define VARIABLE_KIND 1
#define CONST_KIND 2
#define ARRAY_KIND 3
#define FUNC_KIND 4

#define GLOBAL 0
#define LOCAL 1

struct returnNode
{
    int value;
    int type;
    char *str;
};

typedef struct returnNode returnNode;

class Symbol
{
public:
    Symbol();
    Symbol(string, int, int, int, string, int, int);
    void insertArg(int);

    string name;
    int type; // type of variable (e.g. integer, float...)
    int kind; // kind of symbol (e.g. variable, constant, array...)
    int value;
    string str;
    int scope;
    int index;
    vector<int> argumentType; // vector of argument (only store types)
};

class SymbolTable
{
public:
    SymbolTable();
    SymbolTable(string, SymbolTable *);

    Symbol *lookUp(string);
    Symbol *localLookUp(string);
    void insert(string, int, int, int, string = "", int = -1, int = -1);

    vector<Symbol *> table;
    SymbolTable *parent;
    string name;
    int returnType;
};
#endif