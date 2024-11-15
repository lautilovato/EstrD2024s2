#include <iostream>
using namespace std;

struct  ListHeaderSt; 
typedef ListHeaderSt*   List;           // INV.REP.: el puntero NO es NULL

struct  ListIteratorSt;
typedef ListIteratorSt* ListIterator; // INV.REP.: el puntero NO es NULL

List emptyList();
void Cons(int n, List xs);  
void Snoc(List xs, int n);  
bool isEmptyList(List xs);
int  head(List xs); // PRECOND: lista no vacía
void Tail(List xs); // PRECOND: lista no vacía
int  length(List xs);
void Append(List xs, List ys); // Modifica la 1era lista, libera la 2da lista
void Liberar(List xs);

void ShowList(List xs);

ListIterator iniciarRecorrido(List xs);
bool estaAlFinalDelRecorrido(ListIterator ixs);
int  elementoActual(ListIterator ixs);           // PRECOND: no está al 
void PasarAlSiguienteElemento(ListIterator ixs); //   fin del recorrido
void LiberarIterador(ListIterator ixs);
