#include <iostream>
using namespace std;

#include "Tree.h"
#include "LinkedList.h"

int sumTRec(Tree t) {
  if (isEmptyT(t)) { 
    return 0; 
  } else { 
    return (t->value + sumTRec(t->left) + sumTRec(t->right)); 
  }
}

void SuccTRec(Tree t) {
  if (!isEmptyT(t)) { 
    t->value++;
    SuccTRec(t->left);
    SuccTRec(t->right); 
  }
}

int heightTRec(Tree t) {
  if (isEmptyT(t)) { 
    return 0; 
  } else { 
    return (1 + max(heightTRec(t->left), heightTRec(t->right))); 
  }
}

List preorderTSlow(Tree t) {
  if (isEmptyT(t)) { return emptyList(); } 
  else { 
    List xs = preorderTSlow(t->left);
    List ys = preorderTSlow(t->right); 
    Append(xs, ys);     // Modifica xs, libera ys
    Cons(t->value, xs); // Modifica xs
    return xs;
  }
}

void AgregarPreorderEn(Tree t, List xs) {
  if (!isEmptyT(t)) { 
    AgregarPreorderEn(t->right, xs);   // Modifica xs
    AgregarPreorderEn(t->left, xs);    // Modifica xs
    Cons(t->value, xs);                // Modifica xs
  }
}

List preorderT(Tree t) {
  List vistosHastaAhora = emptyList();
  AgregarPreorderEn(t, vistosHastaAhora);
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
  cout << "sumTRec(t1) = " << sumTRec(t1) << endl;
  cout << "heightTRec(t1) = " << heightTRec(t1) << endl;
  cout << "preorderT(t1) = "; ShowList(preorderTSlow(t1)); cout << endl;
  cout << "preorderT(t1) = "; ShowList(preorderT(t1)); cout << endl;
  cout << endl;
  cout << "sumTRec(t2) = " << sumTRec(t2) << endl;
  cout << "heightTRec(t2) = " << heightTRec(t2) << endl;
  cout << "preorderT(t2) = "; ShowList(preorderTSlow(t2)); cout << endl;
  cout << "preorderT(t2) = "; ShowList(preorderT(t2)); cout << endl;
  cout << endl;
  SuccTRec(t1);
  cout << "sumTRec(t1) = " << sumTRec(t1) << endl;
  cout << "heightTRec(t1) = " << heightTRec(t1) << endl;
  cout << "preorderT(t1) = "; ShowList(preorderTSlow(t1)); cout << endl;
  cout << "preorderT(t1) = "; ShowList(preorderT(t1)); cout << endl;
  cout << endl;
  SuccTRec(t2);
  cout << "sumTRec(t2) = " << sumTRec(t2) << endl;
  cout << "heightTRec(t2) = " << heightTRec(t2) << endl;
  cout << "preorderT(t2) = "; ShowList(preorderTSlow(t2)); cout << endl;
  cout << "preorderT(t2) = "; ShowList(preorderT(t2)); cout << endl;
  cout << endl;
}
