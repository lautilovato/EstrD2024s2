#include <iostream>
#include <iomanip>
#include "LinkedListIterables.h"
using namespace std;

struct NodeL {
  int    value;        // INV. REP.:
  NodeL* next;         //  * los punteros internos NO son compartidos
};
struct ListHeaderSt {  // INV. REP.:
  NodeL* first;        //  * first = NULL sii last  = NULL
  int    size;         //  * size es la cant. de nodos a recorrer
  NodeL* last;         //  *   hasta llegar a un NULL desde first
};                     //  * si last != NULL, last->next = NULL

struct ListIteratorSt {
  NodeL* current;
};


List emptyList() {    // O(1)  
  ListHeaderSt* xs = new ListHeaderSt;
  xs->first = NULL;
  xs->last  = NULL;
  xs->size  = 0;
  return xs;
}

bool isEmptyList(List xs) {  // O(1)
  return (xs->size==0);
}

int  head(List xs) {  // O(1)
  // PRECOND: lista no vacía
  return (xs->first->value);
}

int  length(List xs) {  // O(1)
  return (xs->size);
}

void Cons(int n, List xs) {  // O(1)
  NodeL* node = new NodeL;
  node->value = n;
  node->next  = xs->first;
  xs->first   = node;
  if (xs->last == NULL) { xs->last = node; }
  xs->size++;
}

void Snoc(List xs, int n) {  // O(1)
  NodeL* node = new NodeL;
  node->value = n;
  node->next  = NULL;
  if (xs->last == NULL) { xs->first = node;      }
  else                  { xs->last->next = node; }
  xs->last = node;
  xs->size++;
}

void Tail(List xs) {  // O(1)
  // PRECOND: lista no vacía
  NodeL* temp = xs->first;
  xs->first = xs->first->next;
  if(xs->first==NULL) { xs->last==NULL; }
  xs->size--;
  delete temp;
}

void Liberar(List xs) {  // O(n)
  NodeL* temp = xs->first;
  while (xs->first!=NULL) {  
    xs->first = xs->first->next;
    delete temp;
  }
  delete xs;
}

void ShowList(List xs) {
  NodeL* current = xs->first;
  cout << "[";
  if (current!=NULL) {
    cout << " " << current->value;
    current = current->next;
  }
  while (current!=NULL) {  
    cout << ", " << current->value;
    current = current->next;
  }
  cout << " ]";
}

ListIterator iniciarRecorrido(List xs) {
  ListIteratorSt* ixs = new ListIteratorSt;
  ixs->current = xs->first;
}

bool estaAlFinalDelRecorrido(ListIterator ixs) {
  return (ixs->current == NULL);
}

int elementoActual(ListIterator ixs) {
  // PRECOND: no está al fin del recorrido
  return(ixs->current->value);
}

void PasarAlSiguienteElemento(ListIterator ixs){
  // PRECOND: no está al fin del recorrido
  ixs->current = ixs->current->next;
}

void LiberarIterador(ListIterator ixs) {
  delete ixs;
}
