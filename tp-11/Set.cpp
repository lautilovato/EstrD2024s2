#include <iostream>
#include "Set.h"
using namespace std;

struct SetSt {
    int cantidad; // cantidad de elementos diferentes
    NodoS* primero; // puntero al primer nodo
};

// Costo: O(1)
Set emptyS(){
    SetSt* s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
}

// Costo: O(1)
bool isEmptyS(Set s){
    return s->cantidad == 0;
}

// Costo: O(N), siendo n la cantidad de Nodos
bool belongsS(int x, Set s){
    NodoS* current = s->primero;
    for(int i = 1; i <= s->cantidad && current->elem != x; i++){
        current = current->siguiente;
    }
    return current != NULL;
}

// Costo: O(N) por el belongsS
void AddS(int x, Set s){
    if(belongsS(x,s)){
        return;
    }
    NodoS* n = new NodoS;
    n->elem = x;
    n->siguiente = s->primero;
    s->primero = n;
    s->cantidad = s->cantidad + 1;
}

//Quita un elemento dado.
// Costo: O()??
void RemoveS(int x, Set s){
    if(! belongsS(x,s)){
        return;
    }
    NodoS* current = s->primero;
    NodoS* next = current->siguiente;
    if(s->primero == current){
        s->primero = next;
        delete current;
        s->cantidad--;
    }else{
        while(next->elem != x){
            current = current->siguiente;
            next = next->siguiente;
        }
        current->siguiente = next->siguiente;
        delete next;
        s->cantidad--;
    }   
}

// Costo: O(1)
int sizeS(Set s){
    return s->cantidad;
}

// Costo: O(N)
LinkedList setToList(Set s){
    LinkedList l = nil();
    NodoS* current = s->primero;
    for(int i = 1; i <= s->cantidad; i++){
        Cons(current->elem, l);
        current = current->siguiente;
    }
    return l;
}

// Costo: O(N)
void DestroyS(Set s){
    NodoS* temp = s->primero;
    while(s->primero != NULL){
        s->primero = s->primero->siguiente;
        delete temp;
        temp = s->primero;
    }
    delete s;
}