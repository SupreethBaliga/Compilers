int max(int a,int b){
    if(a >= b){
        return a;
    }else{
        return b;
    }
}

int main(){
    int a = 4;
    int b = 6;
    int c = 9;
    int final = max(max(a,b),c);
}