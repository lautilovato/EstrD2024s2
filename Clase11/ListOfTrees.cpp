#include <iostream>
#include <iomanip>
using namespace std;

#include "ListOfTrees.h"
#include "Tree.h"

struct NodeTL {
  Tree    value;        // INV. REP.:
  NodeTL* next;         //  * los punteros internos NO son compartidos
};
struct TListHeaderSt {  // INV. REP.:
  NodeTL* first;        //  * first = NULL sii last  = NULL
  int     size;         //  * size es la cant. de nodos a recorrer
  NodeTL* last;         //  *   hasta llegar a un NULL desde first
};                      //  * si last != NULL, last->next = NULL

TList emptyTL() {    // O(1)  
  TListHeaderSt* xs = new TListHeaderSt;
  xs->first = NULL;
  xs->last  = NULL;
  xs->size  = 0;
  return xs;
}

bool isEmptyTList(TList xs) {  // O(1)
  return (xs->size==0);
}

Tree  headTL(TList xs) {  // O(1)
  // PRECOND: lista no vacía
  return (xs->first->value);
}

void ConsTL(Tree t, TList xs) {  // O(1)
  NodeTL* node = new NodeTL;
  node->value = t;
  node->next  = xs->first;
  xs->first   = node;
  if (xs->last == NULL) { xs->last = node; }
  xs->size++;
}

void SnocTL(TList xs, Tree t) {  // O(1)
  NodeTL* node = new NodeTL;
  node->value = t;
  node->next  = NULL;
  if (xs->last == NULL) { xs->first = node;      }
  else                  { xs->last->next = node; }
  xs->last = node;
  xs->size++;
}

void TailTL(TList xs) {  // O(1)
  // PRECOND: lista no vacía
  NodeTL* temp = xs->first;
  xs->first = xs->first->next;
  if(xs->first==NULL) { xs->last==NULL; }
  xs->size--;
  delete temp;
}

int lengthTL(TList xs) {
  return xs->size;
}

void LiberarTL(TList xs) {  // O(n)
  NodeTL* temp = xs->first;
  while (xs->first!=NULL) {  
    xs->first = xs->first->next;
    delete temp;
  }
  delete xs;
}

