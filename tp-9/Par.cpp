struct Par {
    int x;
    int y;
};

Par consPar(int k, int z){
    Par p;
    p.x = k;
    p.y = z;
    return(p);
}

int fst(Par p){
    return(p.x);
}

int snd(Par p){
    return(p.y);
}


int maxDelPar(Par p){
    int x = p.x;
    int y = p.y; 
    if (x > y){
        return(x);
    }else {
        return(y);
    }
}

Par swap(Par p){
    Par q;
    q.x = p.y;
    q.y = p.x;
    return(q);
}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m){
    Par p;
    p.x = n / m;
    p.y = n % m;
    return(p);
}

