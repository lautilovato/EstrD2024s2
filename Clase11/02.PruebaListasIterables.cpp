#include <iostream>
using namespace std;
#include "LinkedListIterables.h"

int sum(List xs) {  // O(n)
  int total = 0;
  ListIterator ixs = iniciarRecorrido(xs);
  while (!estaAlFinalDelRecorrido(ixs)) {
    total += elementoActual(ixs);
    PasarAlSiguienteElemento(ixs);
  }
  LiberarIterador(ixs);  // Una vez utilizado, se libera
  return total;
}

int main() {
  List xs = emptyList();
  Cons(40, xs);
  Cons(30, xs);
  Cons(20, xs);
  Cons(10, xs);  // Se agregan al revés... ¿Por qué?
  ShowList(xs); cout << endl;
  cout << "length(xs) = " << length(xs) << endl;
  cout << "sum(xs) = " << sum(xs) << endl;
  ShowList(xs); cout << endl;
  cout << "length(xs) = " << length(xs) << endl;
}
