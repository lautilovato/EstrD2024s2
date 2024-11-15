#include <iostream>
using namespace std;
#include "LinkedList.h"

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
  List ys = emptyList();
  Cons(40, ys);
  Cons(30, ys);

  List xs = emptyList();
  Cons(20, xs);
  Cons(10, xs);

  ShowList(xs); cout << endl;
  cout << "length(xs) = " << length(xs) << endl;
  cout << "sum(xs) = " << sum(xs) << endl;
  ShowList(ys); cout << endl;
  cout << "length(ys) = " << length(ys) << endl;
  cout << "sum(ys) = " << sum(ys) << endl;
  Append(xs,ys);
  ShowList(xs); cout << endl;
  cout << "length(xs) = " << length(xs) << endl;

  List zs = emptyList();
  Append(xs, zs);
  ShowList(xs); cout << endl;
  cout << "length(xs) = " << length(xs) << endl;

  List ws = emptyList();
  Append(ws, xs);
  ShowList(ws); cout << endl;
  cout << "length(ws) = " << length(ws) << endl;
}
