#include "Game.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int tomar_compara(char * caracter){
  char c = getchar();
  *caracter = c;
  return c != EOF && c != '\n' && c != '\0';
}

//Logra tomar de la entrada estandar la linea completa
int get_line(char ** destino) {
  char caracter;
  unsigned largo = 80;
  char * buffer = malloc(sizeof(char) * largo);

  unsigned int i;

  for (i = 0;
       tomar_compara(&caracter);
       i++) {

    if (largo == i)
      buffer = realloc(buffer, (largo += 20) * sizeof(char));
    buffer[i] = caracter;
  }

  buffer = realloc(buffer, sizeof(char) * (i + 1));
  buffer[i] = '\0';

  *destino = buffer;

  return i + 1;
}

int main(){
  char * linea;
  printf("$>./simulador ");
  int long_linea = get_line(&linea);

  if (strcmp("game", &(linea[long_linea-5]))){
    free(linea);
    perror("El archivo dado como entrada no es un .game");
    exit(EXIT_FAILURE);
  }

  game_t* juego = loadGame(linea);
  linea = realloc(linea, sizeof(char) * (long_linea + 2));


  //Cambio de la extencion del archivo
  linea[long_linea-5] = 'f';
  linea[long_linea-4] = 'i';
  linea[long_linea-3] = 'n';
  linea[long_linea-2] = 'a';
  linea[long_linea-1] = 'l';
  linea[long_linea] = '\0';
  
  //A JUGAR! y depues a guardar
  writeBoard(congwayGoL(juego),linea);
  printf("Salida generada en: %s\n", linea);
  game_destroy(juego);

  //Libera la memoria pedida para la linea
  free(linea);
  return EXIT_SUCCESS;
}