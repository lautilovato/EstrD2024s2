#include <iostream>
#include <iomanip>
using namespace std;

#include "Tree.h"

Tree emptyT()         { return NULL;      }

bool isEmptyT(Tree t) { return (t==NULL); }

Tree nodeT(int x, Tree ti, Tree td) {
  TreeNodeSt* t = new TreeNodeSt;
  t->value = x;
  t->left  = ti;
  t->right = td;
  return t;
}
