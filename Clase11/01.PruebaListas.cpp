#include <iostream>
using namespace std;
#include "LinkedListBasicas.h"

int sumDestructiva(List xs) {  // O(n)
  int total = 0;
  while (!isEmptyList(xs)) {
    total += head(xs);
    Tail(xs);
  }
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
  cout << "sum(xs) = " << sumDeRepresentacion(xs) << endl;
  cout << "sum(xs) = " << sumDestructiva(xs) << endl;  // Se destruye xs
  ShowList(xs); cout << endl;
  cout << "length(xs) = " << length(xs) << endl;
}
