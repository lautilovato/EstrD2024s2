#include <iostream>
#include <numeric> // Para std::gcd en C++17

struct Fraccion {
    int numerador;
    int denominador;
};

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador){
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

int numerador(Fraccion f){
    return f.numerador;
}

int denominador(Fraccion f){
    return f.denominador;
}

float division(Fraccion f){
    return f.numerador / f.denominador;
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2){
    Fraccion f;
    f.numerador = f1.numerador * f2.numerador;
    f.denominador = f1.denominador * f2.denominador;
    return f;
}

 int gcd(int a, int b) {
        while (b != 0) {
            int temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p){
    Fraccion f;
    int divisor = gcd(p.numerador, p.denominador);
    f.numerador = p.numerador / divisor;
    f.denominador = p.denominador / divisor;
    return f;
}

Fraccion sumF(Fraccion f1, Fraccion f2){
    int num = f1.numerador * f2.denominador + f2.numerador * f1.denominador;
    int den = f1.denominador * f2.denominador;
    Fraccion f;
    f.numerador = num;
    f.denominador = den;
    return f;
}
