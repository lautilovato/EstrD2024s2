#include <iostream>
using namespace std;
#include "Entrenador.h"
#include "Pokemon.h"
#include "ArrayList.h"

//Ejercicio 4

//Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs){
    int resultado = 0;
    for(int i=1; i<= lengthAL(xs); i++){
        resultado = resultado + get(i, xs);
    }
    return resultado;
}

//Incrementa en uno todos los elementos
void sucesores(ArrayList xs){
    for (int i=1; i <= lengthAL(xs); i++){
        set(i, get(i,xs)+1, xs);
    }
}

//Indica si el elemento pertenece a la lista
bool pertenece(int x, ArrayList xs){
    for(int i=1; i<= lengthAL(xs); i++){
        if (get(i,xs) == x){
            return true;
        }
    }
    return false;
}

//Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs){
    int apariciones = 0;
    for(int i=1; i<= lengthAL(xs); i++){
        if(get(i, xs) == x){
            apariciones++;
        }
    }
    return apariciones;
}

//Crea una nueva lista a partir de la primera y la segunda (en ese orden).
ArrayList append(ArrayList xs, ArrayList ys){
    for(int i=1; i<= lengthAL(ys); i++ ){
        add(get(i,ys) , xs);
    }
    return xs;
}

//Devuelve el elemento más chico de la lista.
int minimo(ArrayList xs){
    int min = get(1,xs);
    for(int i = 1; i <= lengthAL(xs); i++){
        if(get(i,xs) < min){
            min = get(i,xs);
        }
    }
    return min;
}


int main(){
    /*Pokemon p1 = consPokemon("fuego");
    Pokemon p2 = consPokemon("agua");
    Pokemon p3 = consPokemon("planta");
    Pokemon* ps = new Pokemon[3];
    ps[0] = p1;
    ps[1] = p2;
    ps[2] = p3;
    Pokemon* ps2 = new Pokemon[2];
    ps2[0] = p1;
    ps2[1] = p2;
    ps2[2] = p3;

    Entrenador entrenador = consEntrenador("lautaro", 3, ps);
    cout << "El nombre del entrenador es: "<< nombreDeEntrenador(entrenador) << endl;
    cout << "La cantidad de pokemons es: " << cantidadDePokemon(entrenador) << endl;
    cout << "La cantidad de pokemons de fuego es: " << cantidadDePokemonDe("fuego", entrenador) << endl;
    cout << "El tipo del pokemon 3 es: "<<tipoDePokemon(pokemonNro(3, entrenador)) << endl;
    Entrenador entrenador2 = consEntrenador("manu", 3, ps2);
    cout << leGanaATodos(entrenador, entrenador2);*/
    
    /*
    ArrayList a = newArrayList();
    add(10,a);
    add(20,a);
    add(30,a);
    cout << "El tercer elemento es: " << get(3,a) << endl;
    cout << "El cuarto elemento es: " << get(4,a) << endl;
    set(5,40,a);
    set(3,40,a);
    cout << "Ahora el tercer elemento es: " << get(3,a) << endl; 
    resize(2,a);
    cout << "Ahora la capacidad es menor a la cantidad que yo tenia" << endl; 
    cout << "El segundo elemento es: " << get(1,a) << endl;
    cout << "El segundo elemento es: " << get(2,a) << endl;
    cout << "El tercer elemento es: " << get(3,a) << endl;
    add(50,a);
    cout << "AHORA El tercer elemento es: " << get(3,a) << endl;
    cout << "AHORA El segundo elemento es: " << get(2,a) << endl;
    remove(a);
    cout << "post REMOVE" << endl; 
    cout << "AHORA El tercer elemento es: " << get(3,a) << endl;
    cout << "AHORA El segundo elemento es: " << get(2,a) << endl;
    cout << "AHORA El primer elemento es: " << get(1,a) << endl;*/
    ArrayList a = newArrayList();
    add(10,a);
    add(7,a);
    add(30,a);
    cout << "La sumatoria del array es: "<<sumatoria(a) << endl;
    cout << "Prueba de suceroes" << endl;
    sucesores(a);
    cout << "Ahora el primer elemento es: " << get(1,a) << endl;
    cout << "Ahora el segundo elemento es: " << get(2,a) << endl;
    cout << "Ahora el tercer elemento es: " << get(3,a) << endl;
    cout << "Prueba de pertenece" << endl;
    cout << "El elemento pertenece (1 SI / 0 NO): " << pertenece(31, a) << endl;
    cout << "Prueba de apariciones" << endl;
    cout << "El elemento 31 aparece: " << apariciones(31,a)<< " veces" << endl;
    cout << "Prueba de append" << endl;
    ArrayList b = newArrayList();
    add(40,b);
    add(1,b);
    ArrayList c = append(a,b);
    cout << "Ahora el primer elemento es: " << get(1,c) << endl;
    cout << "Ahora el segundo elemento es: " << get(2,c) << endl;
    cout << "Ahora el tercer elemento es: " << get(3,c) << endl;
    cout << "Ahora el cuarto elemento es: " << get(4,c) << endl;
    cout << "Ahora el quinto elemento es: " << get(5,c) << endl;
    cout << "Ahora el sexto elemento es: " << get(6,c) << endl;
    cout << "El menor elemento de c es: " << minimo(c) << endl;
    return 0;
}

