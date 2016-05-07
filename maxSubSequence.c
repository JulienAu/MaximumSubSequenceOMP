#include <stdlib.h>
#include <stdio.h>
#include <omp.h>
#include <math.h>
#include <ctype.h>
#include <limits.h>

struct tablo {
  int * tab;
  int size;
};

///////////// Creation du Tableau ///////////////////////////

///Permet de doubler la taille du tableau //////
void arrayDoubleCapacity(struct tablo * source) {
    source->size *= 2;
    source->tab = realloc(source->tab, sizeof(int) * source->size);
}

///Parse le buffer dans le Tableau ////////
void generateArray(struct tablo * s , void *buffer, int buffer_size) {
  s->size=1;
  s->tab=malloc(s->size*sizeof(int));
  int i;
  int x = 0;
  int j = 0;
  for(i = 0;i < buffer_size;++i){
    if(isspace(((char *)buffer)[i])){
      if(j == s->size){

          arrayDoubleCapacity(s);
      } 
        
        s->tab[j] = x;
        j++;
        x=0;
    }else{
        if(((char *)buffer)[i] == '-'){
            x = - (((char *)buffer)[i+1] - '0');
            i++;
        } else {
          if(x<0){
            x = x * 10 - (((char *)buffer)[i] - '0');
            
          }else{
             x = x * 10 + ((char *)buffer)[i] - '0';
             
          }
       }
    }
  }
  if(j <= s->size-1){
    s->tab[s->size-1] = x;
  }
}

///// Recupere les Data du fichier binaire ///////
void generateArrayFile(char * argv , struct tablo * s){
    FILE *file;
    char *buffer;
    unsigned long fileSize;

    
    file = fopen(argv, "rb");
    if (!file)
    {
        fprintf(stderr, "Error file %s", argv);
        return;
    }
  

    fseek(file, 0, SEEK_END);
    fileSize=ftell(file);
    fseek(file, 0, SEEK_SET);

    buffer=(char *)malloc(fileSize+1);

    if (!buffer)
    {
        fprintf(stderr, "Memory error!");
        fclose(file);
        return;
    }

    fread(buffer, fileSize, 1, file);
    fclose(file);
    
    generateArray(s,buffer, fileSize);

    free(buffer);
}
/////////////////////////////////////////////////////////////////


///// Print les tableaux intermediaire ///// 
void printArray(struct tablo * tmp , int x , int y , int Max) {
  int i;
  printf("%i ",Max);
  for (i = x-1; i < y-1; ++i) {
    printf("%i ", tmp->tab[i]);
  }

  printf("%i\n" , tmp->tab[y-1]);
}

//// Permet de print le resultat //////////
void printArray2(struct tablo * tmp) {
  printf("---- Array of size %i ---- \n", tmp->size);
  int size = tmp->size;
  int i;
  for (i = 0; i < size; ++i) {
    printf("%i ", tmp->tab[i]);
  }
  printf("\n");
}

/////Permet d'allouer les structures contenant les tableaux et leurs tailles //////
struct tablo * allocateTablo(int size) {
  struct tablo * tmp = malloc(sizeof(struct tablo));
  tmp->size = size;
  tmp->tab = malloc(size*sizeof(int));
  #pragma omp parallel for
  for(int i= 0 ; i < size ; i++){
    tmp->tab[i] = 0;
  }
  return tmp;
}

/////Libere la memoire des structs /////////
void freeTablo(struct tablo * source) {
  free(source->tab);
  free(source);
}


//////// montee de la somme////////
void montee(struct tablo * source, struct tablo * destination) {
	int l ;
  #pragma omp parallel for
	for (int i = 0; i < source->size; i++) {
   		destination->tab[destination->size / 2 + i] = source->tab[i];
    }

    for ( l=log2(source->size)-1; l >= 0; l-- ){
    	int k = pow(2.0 , l);
    	#pragma omp parallel for
    	for (int j = k ; j <=(k*2)-1; j++) {
    		destination->tab[j] = destination->tab[2*j] + destination->tab[2*j+1];
    	}
    }

}


//////// descente de la somme////////
void descente(struct tablo * a, struct tablo * b) {
	int l;
  b->tab[1] = 0;
    for (l = 2; l <= log2(a->size) ; l++){
    	int k = pow(2.0 , l-1);
    	#pragma omp parallel for
    	for (int j = k ; j <= (k*2)-1; j++) {
    		if((j%2) == 0){
    			b->tab[j]=b->tab[j/2];
    		}else{
    			b->tab[j]=b->tab[j/2]+a->tab[j-1];
    		}
    	}
    }
}


//////// final de la somme////////
void final(struct tablo * a, struct tablo *b , struct tablo *PSUM) {
  #pragma omp parallel for
	for(int j = a->size/2 ; j < a->size;j++){
		PSUM->tab[j-(a->size/2)]=b->tab[j]+a->tab[j];
	}
}

//////// Permet d'obtenir la sumsuffix  a l'aide de la SumPrefix////////
void sumSuffix(struct tablo *a , struct tablo *b){
  int max = a->tab[a->size-1];
  b->tab[0] = max;
  #pragma omp parallel for
  for(int j =0; j < a->size-1;j++){
    b->tab[j+1]=max - a->tab[j];
  }
}

//////// montee du Max Suffix////////
void monteeMaxSuffix(struct tablo * source, struct tablo * destination) {
  int l ;
  for (int i = 0; i < source->size; i++) {
      destination->tab[destination->size - (i + 1)] = source->tab[i];
    }

    for ( l=log2(source->size)-1; l >= 0; l-- ){
      int k = pow(2.0 , l);
      #pragma omp parallel for
      for (int j = k ; j <=(k*2)-1; j++) {
        destination->tab[j] = fmax(destination->tab[2*j],destination->tab[2*j+1]);
      }
    }
}

//////// montee du Max Prefix////////
void monteeMax(struct tablo * source, struct tablo * destination) {
  int l ;
  #pragma omp parallel for
  for (int i = 0; i < source->size; i++) {
      destination->tab[destination->size / 2 + i] = source->tab[i];
    }

    for ( l=log2(source->size)-1; l >= 0; l-- ){
      int k = pow(2.0 , l);
      #pragma omp parallel for
      for (int j = k ; j <=(k*2)-1; j++) {
        destination->tab[j] = fmax(destination->tab[2*j],destination->tab[2*j+1]);
      }
    }
}

//////// Decsente du Max////////
void descenteMax(struct tablo * a, struct tablo * b) {
  int l;
  b->tab[1] = INT_MIN;
    for (l = 2; l <= log2(a->size) ; l++){
      int k = pow(2.0 , l-1);
      #pragma omp parallel for
      for (int j = k ; j <= (k*2)-1; j++) {
        if((j%2) == 0){
          b->tab[j]=b->tab[j/2];
        }else{
          b->tab[j]=fmax(b->tab[j/2],a->tab[j-1]);
        }
      }
    }
}

//////// Final du Max////////
void finalMax(struct tablo * a, struct tablo *b , struct tablo *PMAX) {
  #pragma omp parallel for
  for(int j = a->size/2 ; j < a->size;j++){
    PMAX->tab[j-(a->size/2)]=fmax(b->tab[j],a->tab[j]);
  }
}


//// Calcul le tableau Resultat (M) //////
void maximumSub(struct tablo * Q, struct tablo *PMAX , struct tablo *SSUM , struct tablo *M , struct tablo *SMAX , struct tablo *PSUM) {
  for(int j = 0 ; j < Q->size;j++){
    M->tab[j] = PMAX->tab[j] + SMAX->tab[(Q->size) - (j + 1)] + Q->tab[j] - (SSUM->tab[j] + PSUM->tab[j]);
  }
}


///// Permet d'obtenir le Max ainsi que les indices correspondants////
void Maximum(struct tablo * source, struct tablo * destination , struct tablo * destinationMin , struct tablo * destinationMax) {
  int l ;
  #pragma omp parallel for
  for (int i = 0; i < source->size; i++) {
      destination->tab[destination->size / 2 + i] = source->tab[i];
      destinationMin->tab[destination->size / 2 + i] = i+1;
      destinationMax->tab[destination->size / 2 + i] = i+1;
    }

    for ( l=log2(source->size)-1; l >= 0; l-- ){
      int k = pow(2.0 , l);
      #pragma omp parallel for
      for (int j = k ; j <=(k*2)-1; j++) {
        if(destination->tab[2*j+1] - destination->tab[2*j] >= 0 ){
          if(destination->tab[2*j+1] == destination->tab[2*j]){
            destination->tab[j] = destination->tab[2*j+1];
            if(destinationMax->tab[2*j] - destinationMin->tab[2*j+1] < -1){
              destinationMin->tab[j] = destinationMin->tab[2*j];
              destinationMax->tab[j] =destinationMax->tab[2*j] ; 
            } else{
            destinationMin->tab[j] = fmin(destinationMin->tab[2*j+1],destinationMin->tab[2*j]);
            destinationMax->tab[j] = fmax(destinationMax->tab[2*j+1],destinationMax->tab[2*j]);
            }
          }else{
          destination->tab[j] = destination->tab[2*j+1];
          destinationMin->tab[j] = destinationMin->tab[2*j+1];
          destinationMax->tab[j] = destinationMax->tab[2*j+1];
          }
        } else {
          destination->tab[j] = destination->tab[2*j];
          destinationMin->tab[j] = destinationMin->tab[2*j];
          destinationMax->tab[j] = destinationMax->tab[2*j];
        }
      }
    }
}





int main(int argc, char **argv) {
  struct tablo source;

  generateArrayFile(argv[1] , &source);
  struct tablo * a = allocateTablo(source.size*2);
  montee(&source, a);
  

  struct tablo * b = allocateTablo(source.size*2);
  descente(a, b);
 
  struct tablo * PSUM = allocateTablo(source.size);
  final(a,b,PSUM);
  
  freeTablo(b);
  freeTablo(a);

  struct tablo * SSUM = allocateTablo(source.size);
  sumSuffix(PSUM,SSUM);

  struct tablo * d = allocateTablo(source.size*2);
  monteeMax(SSUM, d);

  struct tablo * e = allocateTablo(source.size*2);
  descenteMax(d, e);

  struct tablo * PMAX = allocateTablo(source.size);
  finalMax(d,e,PMAX);

  freeTablo(d);
  freeTablo(e);

  struct tablo * f = allocateTablo(source.size*2);
  monteeMaxSuffix(PSUM, f);

  struct tablo * g = allocateTablo(source.size*2);
  descenteMax(f, g);

  struct tablo * SMAX = allocateTablo(source.size);
  finalMax(f,g,SMAX);

  freeTablo(f);
  freeTablo(g);


  struct tablo * M = allocateTablo(source.size);
  maximumSub(&source,PMAX,SSUM,M,SMAX,PSUM);
 
  struct tablo * Max = allocateTablo(source.size*2);
  struct tablo * MinI = allocateTablo(source.size*2);
  struct tablo * MaxI = allocateTablo(source.size*2);
  Maximum(M , Max , MinI , MaxI);

  printArray(&source, MinI->tab[1], MaxI->tab[1] , Max->tab[1]);

  freeTablo(SSUM);
  freeTablo(PSUM);
  freeTablo(PMAX);
  freeTablo(SMAX);
  freeTablo(M);
  freeTablo(Max);
  freeTablo(MinI);
  freeTablo(MaxI);
  free(source.tab);
}





