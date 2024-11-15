#include <iostream>
using namespace std;
#include "Set.h"
#include "Queue.h" 

//Devuelve la suma de todos los elementos.
// Costo O(N), siendo N la cantidad de elementos del LinkedList
int sumatoria(LinkedList xs){
    ListIterator i = getIterator(xs);
    int suma = 0;
    while(! atEnd(i)){
        suma = suma + current(i);
        Next(i);
    }
    DisposeIterator(i);
    return suma;
}

//Incrementa en uno todos los elementos.
// Costo O(N), siendo N la cantidad de elementos de la lista
void Sucesores(LinkedList xs){
    ListIterator i = getIterator(xs);
    while(! atEnd(i)){
        SetCurrent(current(i)+1, i);
        Next(i);
    }
    DisposeIterator(i);
}

//Indica si el elemento pertenece a la lista.
// Costo O(N), siendo N la cantidad de elementos de la lista
bool pertenece(int x, LinkedList xs){
    ListIterator i = getIterator(xs);
    while(! atEnd(i)){
        if(current(i) == x){
            return true;
        }
        Next(i);
    }
    DisposeIterator(i);
    return false;
}

//Indica la cantidad de elementos iguales a x
// Costo O(N), siendo N la cantidad de elementos de la lista
int apariciones(int x, LinkedList xs){
    int apariciones = 0;
    ListIterator i = getIterator(xs); 
    while(! atEnd(i)){
        if(current(i) == x){
            apariciones++;
        }
        Next(i);
    }
    DisposeIterator(i);
    return apariciones;
}

//Devuelve el elemento más chico de la lista.
// Costo O(N), siendo N la cantidad de elementos de la lista
int minimo(LinkedList xs){
    ListIterator i = getIterator(xs);
    int min = current(i);
    while(! atEnd(i)){
        if(current(i) < min){
            min = current(i);
        }
        Next(i);
    }
    DisposeIterator(i);
    return min;
}

//Dada una lista genera otra con los mismos elementos, en el mismo orden.
//Nota: notar que el costo mejoraría si Snoc fuese O(1), ¾cómo podría serlo
// Costo O(N*2), snoc por cada elemento de la lista
LinkedList copy(LinkedList xs){
    LinkedList copia = nil();
    ListIterator i = getIterator(xs);
    while(! atEnd(i)){
        Snoc(current(i), copia);
        Next(i);
    }
    DisposeIterator(i);
    return copia;
}

//Agrega todos los elementos de la segunda lista al final de los de la primera.
//La segunda lista se destruye.
//Nota: notar que el costo mejoraría si Snoc fuese O(1), cómo podría serlo?
// Costo O(N*2), snoc por cada elemento de la lista
void AppendV2(LinkedList xs, LinkedList ys){
    while(! isEmpty(ys)){
        Snoc(head(ys), xs);
        Tail(ys);
    }
    DestroyL(ys);
}



int main(){
    /*LinkedList l = nil();
    cout << "La lista esta vacia (1 SI / 0 NO): " << isEmpty(l) << endl;
    Cons(20,l);
    Cons(11,l);
    cout << "La lista esta vacia (1 SI / 0 NO): " << isEmpty(l) << endl;
    cout << "El primer elemento es: " << head(l) << endl;
    cout << "La cantidad de elementos es: " << length(l) << endl;
    Tail(l);
    cout << "El head de la cola es: " << head(l) << endl; 
    Snoc(30, l);
    cout << length(l) << endl;
    cout << head(l) << endl;
    // PRUEA ITERATOR
    ListIterator i = getIterator(l);
    cout << "El elemento actual del List Iterator es: " << current(i) << endl;
    SetCurrent(30,i);
    cout << "Ahora el elemento actual del List Iterator es: " << current(i) << endl; 
    Next(i);
    cout << "El recorrido termino: (1 SI / 0 NO): " << atEnd(i) << endl;
    Next(i);
    DisposeIterator(i);
    DestroyL(l);
    cout << head(l) << endl;
    cout << current(i) << endl;*/
    /*
    LinkedList l = nil();
    Cons(20,l);
    Cons(11,l);
    Cons(3,l);
    cout << "El resulado de la sumatoria es: " << sumatoria(l) << endl;
    Sucesores(l);
    cout << "El resulado de la sumatoria es: " << sumatoria(l) << endl;
    cout << "El elemento pertenece en la lita: " << pertenece(11,l) << endl;
    cout << "Elementos iguales a: " << apariciones(12, l) << endl;
    cout << "El elemento minimo es: " << minimo(l) << endl;
    LinkedList copia = copy(l);
    cout << "El resulado de la sumatoria de la copia es: " << sumatoria(l) << endl;
    AppendV2(l, copia);
    cout << "El resulado de la sumatoria de la oiginal y la copia es: " << sumatoria(l) << endl;
    cout << length(copia) << endl;*/
    


    /*
    Set s = emptyS();
    cout << "El conjunto esta vacio: " << isEmptyS(s) << endl;
    AddS(1,s);
    AddS(2,s);
    AddS(3,s);
    cout << "El conjunto esta vacio: " << isEmptyS(s) << endl;
    cout << "El elemento 1 petenece al conjunto: " << belongsS(1,s) << endl;
    cout << "El elemento 10 petenece al conjunto: " << belongsS(10,s) << endl;
    RemoveS(3,s);
    cout << "El elemento 3 petenece al conjunto: " << belongsS(3,s) << endl;
    cout << "El conjunto tiene " << sizeS(s) << " elementos" << endl;
    LinkedList l = setToList(s);
    cout << "La suma de los elementos da: " << sumatoria(l) << endl;*/ 
    //Preguntar por destroy*/

    
    Queue q = emptyQ();
    cout << isEmptyQ(q) << endl;
    Enqueue(10,q);
    Enqueue(20,q);
    Enqueue(30,q);
    cout << "El primer elemento es: " << firstQ(q) << endl;
    cout << "La cantidad de elementos es: " << lengthQ(q) << endl;
    Dequeue(q);
    cout << "El primer elemento es: " << firstQ(q) << endl;
    cout << "La cantidad de elementos es: " << lengthQ(q) << endl;
    return 0;
    
}