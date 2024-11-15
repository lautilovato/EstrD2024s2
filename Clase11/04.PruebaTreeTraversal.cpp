#include <iostream>
using namespace std;

#include "Tree.h"
#include "ListOfTrees.h"

void AgregarTSiNoEmptyT(Tree t, TList ts) {
  if (!isEmptyT(t)) {
    ConsTL(t, ts);
  }
}

int iSumT(Tree t) {
  TList faltanProcesar = emptyTL(); // INV.REP.: nunca habrá emptyT en la lista
  Tree actual;
  int totalVisto = 0;
  AgregarTSiNoEmptyT(t, faltanProcesar);
  while(!isEmptyTList(faltanProcesar)) {
    actual = headTL(faltanProcesar); // actual NO es emptyT
    TailTL(faltanProcesar);
    totalVisto += actual->value;
    AgregarTSiNoEmptyT(actual->right, faltanProcesar);
    AgregarTSiNoEmptyT(actual->left , faltanProcesar);
  }
  LiberarTL(faltanProcesar);
  return(totalVisto);
}

int main() {
  Tree t1 = nodeT(11, nodeT(21, emptyT()
                              , emptyT())
                    , nodeT(31, emptyT()
                              , emptyT())
                 );
  Tree t2 = nodeT(1, nodeT(2, nodeT(4, emptyT()
                                     , emptyT())
                            , nodeT(5, emptyT()
                                     , emptyT()))
                   , nodeT(3, nodeT(6, emptyT()
                                     , emptyT())
                            , nodeT(7, emptyT()
                                     , emptyT()))
                 );
  cout << "iSumT(t1) = " << iSumT(t1) << endl;
  cout << endl;
  cout << "iSumT(t2) = " << iSumT(t2) << endl;
  cout << endl;
}
