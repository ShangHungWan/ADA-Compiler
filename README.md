# Programming Assignment 3

## Changes to project

- 新增 symbol table 以及 symbol 的紀錄內容（etc: value, scope, index, string）
- 將 yacc 的 return 改為 Node（type and value），而非原本單純的 return type
- 由於上一點，有些非動態產生的 expression（例如：c := 1 + 5），可預先得出值，而非在執行階段才得知
- 修正 lex 無回傳 STRING 的問題
- 修正 yacc syntax error 的問題
- 實作 stack 以便紀錄 block （if, while, loop...）的路徑指向
- 刪除此 Project 不必要的功能（READ, arrays, float, string variables...）

## Usage

``` bash
make
./main examples/HelloWorld.ada
./javaaProtable/javaa examples/HelloWorld.jasm
java HelloWorld
```
