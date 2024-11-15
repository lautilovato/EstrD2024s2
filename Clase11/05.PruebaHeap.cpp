#include <iostream>
using namespace std;

#include "BinHeap.h"

int main() {
  // Creación del ejemplo de prueba
  int values[20] = { 24, 3, 8, 13, 10, 17, 42, 92, 15, 22, 53, 25, 33, 56, 50, 99, 88, 12, 90, 23 };
  BinHeap h2 = crearHeap(values, 20, 32);
  cout << "h2                = "; ShowHeap(h2);
  BinHeap h = emptyHeap();
  InsertH(3, h);
  InsertH(10, h); InsertH(8, h);
  InsertH(12, h); InsertH(22, h); InsertH(17, h); InsertH(42, h);
  InsertH(88, h); InsertH(13, h); InsertH(23, h); InsertH(53, h); InsertH(25, h); InsertH(33, h); InsertH(56, h); InsertH(50, h);
  InsertH(99, h); InsertH(92, h); InsertH(15, h); InsertH(90, h); InsertH(24, h);
  cout << "h                 = "; ShowHeap(h);
  // Caso común de inserción
  InsertH(5, h);
  cout << "h  = Insert(5, h) = "; ShowHeap(h);
  // Caso de borde
  InsertH(1, h);
  cout << "h  = Insert(1, h) = "; ShowHeap(h);
  DeleteMin(h);
  cout << "h  = DeleteMin(h) = "; ShowHeap(h);
  DeleteMin(h);
  cout << "h  = DeleteMin(h) = "; ShowHeap(h);
  DeleteMin(h);
  cout << "h  = DeleteMin(h) = "; ShowHeap(h);
}
