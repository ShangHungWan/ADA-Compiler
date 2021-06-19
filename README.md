# Programming Assignment 2

## Changes to scanner

- 將 Symbol Table 改為用 Linked List 實作，並從 lex 中拆出去
- 引入 y.tab.h 的 token 並傳送給 yacc parser
- 刪除 main()，改至 yacc 中實做
- 註解部份改用 %x 實作
- 刪除大部分的輸出

## Usage

``` bash
make
./main examples/HelloWorld.ada
```
