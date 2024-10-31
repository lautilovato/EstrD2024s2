#include <iostream>
using namespace std;
#include "Entrenador.h"
#include "Pokemon.h"


int main(){
    Pokemon p1 = consPokemon("fuego");
    Pokemon p2 = consPokemon("agua");
    Pokemon p3 = consPokemon("planta");
    Pokemon pokemons[] = {p1,p2, p3};
    Entrenador entrenador = consEntrenador("lautaro", 3, pokemons);
    cout << nombreDeEntrenador(entrenador) << endl;
    cout << cantidadDePokemon(entrenador) << endl;
    cout << cantidadDePokemonDe("fuego", entrenador) << endl;
    cout << tipoDePokemon(pokemonNro(3, entrenador)) << endl;
    Pokemon pokemons2[] = {p1, p1, p1};
    Entrenador entrenador2 = consEntrenador("manu", 3, pokemons2);
    cout << algunoLeGana(entrenador, p1)  << endl;
    cout << leGanaATodos(entrenador, entrenador2);
    return 0;
}

