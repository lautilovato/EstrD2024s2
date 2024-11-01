#include <iostream>
using namespace std;
#include "Entrenador.h"
#include "Pokemon.h"
#include "ArrayList.h"

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
    cout << "AHORA El primer elemento es: " << get(1,a) << endl;
    return 0;
}

