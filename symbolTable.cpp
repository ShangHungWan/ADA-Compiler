#include "symbolTable.h"

Symbol::Symbol() {}

Symbol::Symbol(string name, int type, int kind)
{
    this->name = name;
    this->type = type;
    this->kind = kind;
}

void Symbol::insertArg(int type)
{
    this->argumentType.push_back(type);
}

SymbolTable::SymbolTable()
{
    this->name = "";
    this->parent = NULL;
    this->returnType = NONE_TYPE;
}

SymbolTable::SymbolTable(string name, SymbolTable *parent)
{
    this->name = name;
    this->parent = parent;
    this->returnType = NONE_TYPE;
}

Symbol *SymbolTable::lookUp(string name)
{
    // find specific entity by name from "all" tables
    SymbolTable *currentTable = this;
    while (currentTable != NULL)
    {
        for (int i = 0; i < currentTable->table.size(); ++i)
        {
            if (currentTable->table[i]->name == name)
            {
                return currentTable->table[i];
            }
        }
        currentTable = currentTable->parent;
    }
    return NULL;
}

Symbol *SymbolTable::localLookUp(string name)
{
    // find specific entity by name from only "this" table
    for (int i = 0; i < this->table.size(); ++i)
    {
        if (this->table[i]->name == name)
        {
            return this->table[i];
        }
    }
    return NULL;
}

void SymbolTable::insert(string name, int type, int kind)
{
    // insert a new symbol to this table
    this->table.push_back(new Symbol(name, type, kind));
}