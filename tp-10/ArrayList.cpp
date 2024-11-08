#include <iostream>
#include "ArrayList.h"
using namespace std;

struct ArrayListSt {
    int cantidad; // cantidad de elementos
    int* elementos; // array de elementos
    int capacidad; // tamaño del array
};

//Crea una lista con 0 elementos.
//Nota: empezar el array list con capacidad 16.
ArrayList newArrayList(){
    ArrayListSt* a = new ArrayListSt;
    a->cantidad = 0;
    a->elementos = new int[16];
    a->capacidad = 16;
}

//Crea una lista con 0 elementos y una capacidad dada por parámetro.
ArrayList newArrayListWith(int capacidad){
    ArrayListSt* a = new ArrayListSt;
    a->cantidad = 0;
    a->elementos = new int[capacidad];
    a->capacidad = capacidad;
}

int lengthAL(ArrayList xs){
    return xs->cantidad;
}

//Devuelve el iésimo elemento de la lista.
int get(int i, ArrayList xs){
    if(i > xs->cantidad){
        return 777;
    }else{
        return xs->elementos[i-1];
    }
    
}

//Reemplaza el iésimo elemento por otro dado.
void set(int i, int x, ArrayList xs){
    if(i > xs->cantidad){
        cout << "El elemento no existe" << endl;
    }
    xs->elementos[i-1] = x;
}

// Decrementa o aumenta la capacidad del array.
// Nota: en caso de decrementarla, se pierden los elementos del final de la lista.
void resize(int capacidad, ArrayList xs){
    if(capacidad >= xs->capacidad){
        int* temp = new int[capacidad];
        for(int i=0; i < xs->cantidad; i++){
            temp[i] = xs->elementos[i];
        }
        delete xs->elementos;
        xs->capacidad = capacidad;
        xs->elementos = temp;

    }else if(capacidad < xs->capacidad && xs->cantidad < capacidad){
        int* temp = new int[capacidad];
        for(int i=0; i <= xs->cantidad; i++){
            temp[i] = xs->elementos[i];
        }
        delete xs->elementos;
        xs->capacidad = capacidad;
        xs->elementos = temp;

    }else{
        int* temp = new int[capacidad];
        for(int i=0; i < capacidad; i++){
            temp[i] = xs->elementos[i];
        }
        delete xs->elementos;
        xs->cantidad = capacidad;
        xs->capacidad = capacidad;
        xs->elementos = temp;
    }
}


//Agrega un elemento al final de la lista.
void add(int x, ArrayList xs){
    if (xs->capacidad = xs->cantidad){
        xs->capacidad = xs->capacidad *2;
        int* temp = new int[xs->capacidad];
        for(int i=0; i <= xs->cantidad; i++){
            temp[i] = xs->elementos[i];
        }
        temp[xs->cantidad] = x;
        xs->cantidad++;
        delete xs->elementos;
        xs->elementos = temp;
    }else{
        xs->elementos[xs->cantidad] = x;
        xs->cantidad++;    
    }
}

void remove(ArrayList xs){
    if (xs->cantidad == 0){
        return;
    }else{
        xs->cantidad = xs->cantidad - 1;
    }
}

