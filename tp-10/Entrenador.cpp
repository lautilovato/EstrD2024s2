#include <iostream>
#include "Entrenador.h"
#include "Pokemon.h"
using namespace std;

struct EntrenadorSt{
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantPokemon = cantidad;
    e->pokemon = new Pokemon[e->cantPokemon];
    e->pokemon = pokemon;
}

string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}

int cantidadDePokemon(Entrenador e){
    return e->cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){ 
    int contador = 0;
    for (int i = 0; i < e->cantPokemon; i++){
        if(tipoDePokemon(e->pokemon[i]) == tipo){
            contador++;
        }
    }
    return contador;

}

Pokemon pokemonNro(int i, Entrenador e){
    if(i > 0 && i <= e->cantPokemon){
        return e->pokemon[i-1];
    }
    return pokemonNulo();
}

//Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
//posee al menos un pokémon que le gane.
bool leGanaATodos(Entrenador e1, Entrenador e2){
    int pokemonsVencidos = 0;
    for (int i = 0; i < e2->cantPokemon; i++){
        if(algunoLeGana(e1, e2->pokemon[i])){
           pokemonsVencidos++; 
        }
    }
    return pokemonsVencidos == e2->cantPokemon;
}

bool algunoLeGana(Entrenador e, Pokemon pokemon){
    
    for(int i = 0; i < e->cantPokemon  ; i++ ){
        if(superaA(e->pokemon[i], pokemon)){
            return true;
        }
    }
    return false;
}

