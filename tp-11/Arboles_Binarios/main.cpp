#include <iostream>
using namespace std;
#include "Tree.h"
#include "Arraylist.h"

//Dado un árbol binario de enteros devuelve la suma entre sus elementos
int sumarT(Tree t){
    if (isEmptyT(t)){
        return 0;
    }else{
        return (rootT(t) + sumarT(left(t)) + sumarT(right(t)) );
    }
}

//Dado un árbol binario devuelve su cantidad de elementos, 
//es decir, el tamaño del árbol (size en inglés).
int sizeT(Tree t){
    if (isEmptyT(t)){
        return 0;
    }else{
        return (1 + sizeT(left(t)) + sizeT(right(t)) );
    }
}

//Dados un elemento y un árbol binario devuelve True 
//si existe un elemento igual a ese en el árbol
bool perteneceT(int e, Tree t){
    if (isEmptyT(t)){
        return false;
    }else{
        return rootT(t) == e || + perteneceT(e, left(t)) || + perteneceT(e, right(t));
    }
}

int unoSi(bool b){
    if(b){
        return 1;
    }else{
        return 0;
    }
}

//Dados un elemento e y un árbol binario devuelve 
//la cantidad de elementos del árbol que son iguales a e
int aparicionesT(int e, Tree t){
    if (isEmptyT(t)){
        return 0;
    }else{
        return ( unoSi(rootT(t) == e) + aparicionesT(e, left(t)) + aparicionesT(e, right(t)) );
    }
}



//Dado un árbol devuelve su altura.
int heightT(Tree t){
    if (isEmptyT(t)){
        return 0;
    }else{
        return 1 + max( heightT(left(t)), heightT(right(t))) ;
    }
}

ArrayList append(ArrayList xs, ArrayList ys){
    for(int i=1; i<= lengthAL(ys); i++ ){
        add(get(i,ys) , xs);
    }
    return xs;
}

//Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t){
    ArrayList xs = newArrayListWith(sizeT(t));
    if (isEmptyT(t)){
        return xs;
    }else{
        add(rootT(t), xs);
        return append( xs, append(toList(left(t)), toList(right(t))) );
    }
}

// Costo: O(n)
ArrayList AgregarLeaves(Tree t, ArrayList xs) {
    if(!isEmptyT(t)) {                      
        if(isEmptyT(left(t)) && isEmptyT(right(t))) {
            add(rootT(t), xs);
        } else {
            AgregarLeaves(left(t), xs);
            AgregarLeaves(right(t), xs);
        }
    }
}

// Dado un árbol devuelve los elementos que se encuentran en sus hojas. NOTA: en este tipo se define como hoja a un nodo con dos hijos vacíos.
ArrayList leaves(Tree t) {                  // Costo: O(n)
    ArrayList vistosHastaAhora = newArrayList(); // Creo una lista vacia. 
    AgregarLeaves(t, vistosHastaAhora);     // Voy guardando cada elemento del arbol.
    return vistosHastaAhora;
}

// Costo: O(n)
void AgregarLevenN(int n, Tree t, ArrayList xs) {
    if(!isEmptyT(t)) {                      
        if(n == 1) {
            add(rootT(t), xs);
        } else {
            AgregarLevenN(n-1, left(t), xs);
            AgregarLevenN(n-1, right(t), xs);
        }
    }
}

// Costo: O(n)
//  Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
ArrayList levelN(int n, Tree t) {          
    ArrayList xs = newArrayList();
    AgregarLevenN(n,t,xs);
    return xs;
}

int main(){
    //Tree t = nodeT( 2 , nodeT(7, emptyT(), emptyT()) , emptyT() );
    Tree t1 = nodeT(11, nodeT(11, emptyT()
                              , emptyT())
                    , nodeT(30, emptyT()
                              , emptyT())
                 );
    cout << "Este es un arbol vacio: " << isEmptyT(t1) << endl;
    cout << "La raiz de este arbol es: " << rootT(t1) << endl;
    cout << "La suma del arbol da: " << sumarT(t1) << endl;
    cout << "El size del arbol es: " << sizeT(t1) << endl;
    cout << "El elemento 11 pertenece: " << perteneceT(11,t1) << endl;
    cout << "El elemento 11 aparece: " << aparicionesT(11,t1) << " veces" << endl;
    cout << "La altura del arbol es: " << heightT(t1) << endl;
    ArrayList l = toList(t1);
    cout << "El tamano de la lista es: " << lengthAL(l) << endl;
    ArrayList lvs = leaves(t1);
    cout << "El tamano de la lista es: " << lengthAL(lvs) << endl;
    ArrayList lvln = levelN(1,t1);
    cout << "El tamano de la lista es: " << lengthAL(lvln) << endl; 
}

