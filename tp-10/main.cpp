#include <iostream>
using namespace std;
#include "Persona.h"


int main(){
    Persona p1 = consPersona("Lautaro", 22);
    cout << edad(p1) << endl;
    cout << nombre(p1) << endl;
    crecer(p1);
    cout << (edad(p1)) << endl;
    cambioDeNombre("Tobias", p1);
    cout << nombre(p1) << endl;
    Persona p2 = consPersona("David", 39);
    cout << esMayorQueLaOtra(p2, p1) << endl;
    cout << nombre(laQueEsMayor(p1,p2));
    return 0;
}







