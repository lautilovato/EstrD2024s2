#include <iostream>
using namespace std;

struct BinHeapHeaderSt;
typedef BinHeapHeaderSt* BinHeap;

BinHeap emptyHeap();
void InsertH(int x, BinHeap h);
bool isEmptyHeap(BinHeap h);
int  findMin(BinHeap h);
void DeleteMin(BinHeap h);

BinHeap crearHeap(int* elems, int cant, int max);
void ShowHeap(BinHeap h);
