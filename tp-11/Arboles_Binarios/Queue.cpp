#include <iostream>
#include "Queue.h"
#include "Tree.h"
using namespace std;

struct NodoQ {
    Tree elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};

struct QueueSt {
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo; // puntero al ultimo nodo
};

//Crea una cola vacía.
//Costo: O(1).
Queue emptyQ(){
    QueueSt* q = new QueueSt;
    q->primero = NULL;
    q->ultimo = NULL;
    q->cantidad = 0;
}

//Indica si la cola está vacía.
//Costo: O(1).
bool isEmptyQ(Queue q){
    return q->cantidad == 0;
}

//Devuelve el primer elemento.
//Costo: O(1).
Tree firstQ(Queue q){
    return q->primero->elem;
}

//Agrega un elemento al final de la cola.
//Costo: O(1).
void Enqueue(Tree x, Queue q){
    NodoQ* n = new NodoQ;
    n->elem = x;
    n->siguiente = NULL;
    if(isEmptyQ(q)){
        q->primero = n;
    }else{
        NodoQ* ultimo = q->ultimo;
        ultimo->siguiente = n;
    }
    q->ultimo = n;
    q->cantidad = q->cantidad + 1;
}

//Quita el primer elemento de la cola.
//Costo: O(1).
void Dequeue(Queue q){
    if(q->primero == q->ultimo){
        delete q->primero;
        q->primero = NULL;
        q->ultimo = NULL;
    }else{
        NodoQ* fst = q->primero;
        q->primero = fst->siguiente;
        delete fst;
    }
    q->cantidad = q->cantidad - 1;
}

//Devuelve la cantidad de elementos de la cola.
//Costo: O(1).
int lengthQ(Queue q){
    return q->cantidad;
}


//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).
void MergeQ(Queue q1, Queue q2){
    q1->ultimo->siguiente =  q2->primero;
    delete q2;
}

//Libera la memoria ocupada por la cola.
//Costo: O(n).
void DestroyQ(Queue q){
    while(! isEmptyQ(q)){
        NodoQ* fst = q->primero;
        Dequeue(q);
        delete fst;
    }
    delete q;
}





