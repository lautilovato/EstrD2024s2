#include <iostream>
using namespace std;

#ifndef TREEHEADER
#include "Tree.h"
#endif

struct  TListHeaderSt;
typedef TListHeaderSt* TList; // INV.REP.: el puntero NO es NULL

TList emptyTL();
void ConsTL(Tree n, TList xs);  
void SnocTL(TList xs, Tree n);  
bool isEmptyTList(TList xs);
Tree headTL(TList xs); // PRECOND: lista no vacía
void TailTL(TList xs); // PRECOND: lista no vacía
int  lengthTL(TList xs);
void LiberarTL(TList xs);
