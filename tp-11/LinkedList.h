#include <iostream>
using namespace std;

struct LinkedListSt;
typedef LinkedListSt* LinkedList; // INV.REP.: el puntero NO es NULL

struct IteratorSt;
typedef IteratorSt* ListIterator; // INV.REP.: el puntero NO es NULL

struct NodoL;

//Crea una lista vacía.
LinkedList nil();

//Indica si la lista está vacía.
bool isEmpty(LinkedList xs);                    

//Devuelve el primer elemento.
int head(LinkedList xs);

//Agrega un elemento al principio de la lista.
void Cons(int x, LinkedList xs);

//Quita el primer elemento.
void Tail(LinkedList xs);

//Devuelve la cantidad de elementos.
int length(LinkedList xs);

//Agrega un elemento al final de la lista.
void Snoc(int x, LinkedList xs);

//Apunta el recorrido al primer elemento.
ListIterator getIterator(LinkedList xs);

//Devuelve el elemento actual en el recorrido.
int current(ListIterator ixs);

//Reemplaza el elemento actual por otro elemento.
void SetCurrent(int x, ListIterator ixs);

//Pasa al siguiente elemento.
void Next(ListIterator ixs);

//Indica si el recorrido ha terminado.
bool atEnd(ListIterator ixs);

//Libera la memoria ocupada por el iterador.
void DisposeIterator(ListIterator ixs);

//Libera la memoria ocupada por la lista
void DestroyL(LinkedList xs);

// retorna al ultimo nodo 
NodoL* ultimoNodo(LinkedList xs);

//Agrega todos los elementos de la segunda lista al final de los de la primera.
//La segunda lista se destruye
void Append(LinkedList xs, LinkedList ys);
