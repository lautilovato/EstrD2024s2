#include <iostream>
using namespace std;

#ifndef TREENODEST
#define TREENODEST
struct TreeNodeSt {
  int value;
  TreeNodeSt* left;
  TreeNodeSt* right;
};
#endif

typedef TreeNodeSt* Tree;

Tree emptyT();
bool isEmptyT(Tree t);
Tree nodeT(int x, Tree ti, Tree td);

