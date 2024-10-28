#include <iostream>
using namespace std;
#include "Par.h"


//Propósito: imprime n veces un string s
void printN(int n, string s){
    if (n > 0){
        cout << s << endl;
        printN(n-1, s);
    }
    return;
}

void printNV2(int n, string s){
    while(n>0, n--){
        cout << s << endl;
    }
}

//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n){
    if(n>=0){
        cout << n << endl;
        cuentaRegresiva(n-1);
    }
    return;
}

void cuentaRegresivaV2(int n){
    while(n>=0){
        cout << n << endl;
        n = n-1;
    }
}

//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaN(int n){
    if(n<0){
        return;  
    }
    desdeCeroHastaN(n-1);
    cout << n << endl;
}

void desdeCeroHastaNV2(int n){
    int current = 0;
    while(current <= n){
        cout << current << endl;
        current = current + 1;
    }
    return;
}

//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int mult(int n, int m){
    if(n <= 0){
        return 0;   
    }
    return m + mult(n-1, m);
}

int multV2(int n, int m){
    int result = 0;
    while (n > 0, n--){
        result = result + m;
    }
    return result;
}

//Propósito: imprime los primeros n char del string s, separados por un salto de línea.
//Precondición: el string tiene al menos n char.s
void primerosN(int n, string s){
    if (n>0){
        cout << s[0] << endl;
        primerosN(n-1, s.substr(1));
    }
    return;
}

void primerosNV2(int n, string s){
    int current = 0;
    while(current < n){
        cout << s[current] << endl;
        current = current + 1;
    }
    return;
}

// Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s){
    if (s.empty()) {
        return false;
    }else if(c == s[0]){
        return true;
    }else{
    return pertenece(c, s.substr(1));
    }
}

bool perteneceV2(char c, string s){
    for(char ch : s){
        if(ch == c){
            return true;
        }
    }
    return false;
}

//Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int apariciones(char c, string s){
    if(s.empty()){
        return 0;
    }else if(c == s[0]){
        return 1 + apariciones(c,s.substr(1));
    }else{
        return 0 + apariciones(c,s.substr(1));
    }
}

int aparicionesV2(char c, string s){
    int apariciones = 0;
    for(char ch : s){
        if(ch == c){
            apariciones = apariciones + 1;
        }
    }
    return apariciones;
}

int main(){
    cout << aparicionesV2('a',"Holaaa");
    return 0;
}

