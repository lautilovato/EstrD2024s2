#include <iostream>
#include "Pokemon.h"
using namespace std;

struct PokeSt{
    TipoDePokemon tipo;
    int vida;
};

Pokemon pokemonNulo(){
    return NULL;
}

Pokemon consPokemon(TipoDePokemon tipo){
    PokeSt* p = new PokeSt;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p){
    return p->tipo;
}

int energia(Pokemon p){
    return p->vida;
}

void perderEnergia(int energia, Pokemon p){
    p->vida = p->vida - energia;
}

//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
//a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
bool superaA(Pokemon p1, Pokemon p2){
    if(p1->tipo == "agua" && p2->tipo == "fuego"){
        return true;
    }else if(p1->tipo == "fuego" && p2->tipo == "planta"){
        return true;
    }else if(p1->tipo == "planta" && p2->tipo == "agua"){
        return true;
    }else{
        return false;
    }
}