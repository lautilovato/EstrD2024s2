#include <iostream>
#include "LinkedList.h"
using namespace std;

struct NodoL {
    int elem; // valor del nodo
    NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt {
    // INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
    // desde primero por siguiente hasta alcanzar a NULL
    int cantidad; // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
};


struct IteratorSt {
    NodoL* current;
};


//Costo O(1)
LinkedList nil(){
    LinkedListSt* l = new LinkedListSt;
    l->cantidad = 0;
    l->primero = NULL;
    return l;
}

//Costo O(1)
bool isEmpty(LinkedList xs){
    return xs->cantidad == 0; 
}

//Costo O(1)
int head(LinkedList xs){
    return (xs->primero)->elem;
}

//Costo O(1)
void Cons(int x, LinkedList xs){
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente = xs->primero;
    xs->primero = nodo;
    xs->cantidad++;
}

//Costo O(1)
void Tail(LinkedList xs){
    NodoL* fst = xs->primero;
    xs->primero = fst->siguiente;
    delete fst;
    xs->cantidad--; 
}

//Costo O(1)
int length(LinkedList xs){
    return xs->cantidad;
}

//Agrega un elemento al final de la lista.
//Costo O(N), siendo N la cantidad de nodos.
void Snoc(int x, LinkedList xs){
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente = NULL;
    if(isEmpty(xs)){
        xs->primero = nodo;
    }else{
        ultimoNodo(xs)->siguiente = nodo;
    }
    xs->cantidad++;
}

//precond la lista no esta vacia
//Costo O(N), siendo N la cantidad de nodos.
NodoL* ultimoNodo(LinkedList xs){
    NodoL* current = xs->primero;
    while(current->siguiente != NULL){
        current = current->siguiente;
    }
    return current;
}

//Costo O(1)
ListIterator getIterator(LinkedList xs){
    IteratorSt* i = new IteratorSt;
    i->current = xs->primero;
    return i;
}

//Costo O(1)
int current(ListIterator ixs){
    return ixs->current->elem;
}

//Costo O(1)
void SetCurrent(int x, ListIterator ixs){
    ixs->current->elem = x;
}

//Costo O(1)
void Next(ListIterator ixs){
    ixs->current = ixs->current->siguiente;
}

//Costo O(1)
bool atEnd(ListIterator ixs){
    return ixs->current == NULL;
}

//Costo O(1)
void DisposeIterator(ListIterator ixs){
    delete ixs;
}

//Costo O(1)
void DestroyL(LinkedList xs){
    NodoL* temp = xs->primero;
    while(xs->primero != NULL){
        xs->primero = xs->primero->siguiente;
        delete temp;
        temp = xs->primero;
    }
    delete xs;
}

void Append(LinkedList xs, LinkedList ys){
    while(! isEmpty(ys)){
        Snoc(head(ys), xs);
        Tail(ys);
    }
    DestroyL(ys);
}