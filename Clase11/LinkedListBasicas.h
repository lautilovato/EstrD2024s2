#include <iostream>
using namespace std;

struct  ListHeaderSt;
typedef ListHeaderSt* List; // INV.REP.: el puntero NO es NULL

List emptyList();
void Cons(int n, List xs);  
void Snoc(List xs, int n);  
bool isEmptyList(List xs);
int  head(List xs); // PRECOND: lista no vacía
void Tail(List xs); // PRECOND: lista no vacía
int  length(List xs);
void Liberar(List xs);

void ShowList(List xs);
int sumDeRepresentacion(List xs);
