#include <stdio.h>
#include <stdlib.h>
#include <semaphore.h>
#include <string.h>
#include <math.h>
#include <sys/sysinfo.h>
#include <assert.h>
#include <unistd.h>

//#include <unistd.h>
#include <pthread.h>

/* Librerias propias */
#include "Game.h"   //Implementacion del tablero
#include "Board.h"  //El juego propiamente dicho
#include "Barreras.h"

//Lee un tablero codificado en RLE y lo carga en un tablero
void cargar_tablero(board_t * tablero, FILE * archivo){
  char line[tablero->fila *2], c;
  unsigned long contador,cant;
  unsigned int offsset = 0;
  size_t row = tablero->fila;
  size_t col = tablero->columna;

  for (size_t i = 0; i < row; i++){
    fscanf(archivo, "%s", line);
    contador = 0;
    while (contador < col){
      sscanf(&(line[offsset]),"%lu%c", &cant, &c);
      memset(&(tablero->board[i][contador]),c,cant);
      contador += cant;
      offsset = offsset + (int) log10(cant) + 2;
    }
    offsset = 0;
  }
}

//Decide la cantidad de hilos a utilizar para ejecutar el programa
unsigned int hilos(game_t * juego){
  size_t cantProsUnit = get_nprocs();
  size_t filas = juego->tableros->fila;
  return cantProsUnit <= filas ?  (unsigned int) cantProsUnit : (unsigned) filas;
}

//Lee un archivo .game cargando una partida del juego de la vida de conway
game_t *loadGame(const char *filename){
  game_t * juego = malloc(sizeof(game_t));

  FILE * archivo = fopen(filename,"r");
  if (!archivo){
    perror("Archivo no encontrado");
    free(juego);
    exit(EXIT_FAILURE);
  }
  
  size_t row, col;
  
  int lectura; 
  lectura = fscanf(archivo, "%u %lu %lu", &(juego->ciclos), &row, &col); 
                //Cantidad de cilos, columnas, filas.

  if (lectura != 3){
    perror("El archivo no cumple con los estandares de.game");
    free(juego);
    exit(EXIT_FAILURE);
  }

  /* Tableros */
  board_init(&(juego->tableros[0]),col, row); //En este se guarda el estado inicial
  board_init(&(juego->tableros[1]),col, row);

  //Carga del tablero inical.
  cargar_tablero(juego->tableros, archivo);
  
  fclose(archivo);

  /* TEMPORAL PARA TESTs*/
  for (size_t i = 0; i < row; i++)
    printf("%s\n", juego->tableros->board[i]);
  printf("Ciclos: %u Dim: %lu %lu\n", 
    juego->ciclos, row, col);
  /* TEMPORAL PARA TESTs*/

  return juego;
}

//Libera a los dos tableros que contiene el juego.
void game_destroy(game_t * game){
  board_destroy(game->tableros);
  board_destroy(&(game->tableros[1]));
  free(game);
}

void writeBoard(board_t * board, const char *filename){
  FILE * archivo = fopen(filename, "w+");

  if (!archivo){
    perror("Preoblemas en el archivo de salida\n");
    board_destroy(board);
    exit(EXIT_FAILURE);
  }

  unsigned int col = 0;
  unsigned int cont = 0;
  //unsigned int total = 0;

  char c;

  for (unsigned row = 0; row < board->fila; row++){
    col = 0;     
    while (col < board->columna){
      c = board_get(*board, col,row);
      cont = 0;
      while (col < board->columna && board_get(*board,col,row) == c ) {
        cont++;
        col++;
      }
      fprintf(archivo, "%u%c", cont, c);
    }
    fprintf(archivo, "\n");
  }
  fclose(archivo);
}

//Copia el juego dandole el valor del hilo al que se le dara esta clon del juego
//y la barrea que se utilizara para sincronizar.
game_t game_clone(game_t * game, unsigned int num_clon, barrier_t * bar){
  game_t clon;
  clon.tableros[0] = game->tableros[0];
  clon.tableros[1] = game->tableros[1];
  clon.ciclos = game->ciclos;
  clon.hilos = game->hilos;
  clon.barrier = bar;
  clon.id = num_clon;
  return clon;
}

//Nos indica si una celula esta viva o muerta.
char DeadLive(board_t tablero, size_t col, size_t row){
  int live = 0;
  for (int i = -1; i <= 1; i++){
    for (int j = -1; j <= 1; j++){
      if (j != 0 || i != 0){
        if (board_get_round(tablero, col+i, row+j) == ALIVE)
          live++;
      }
    }
  }
  if (live == 3)
    return ALIVE; //NAce o tiene companieras
  if (board_get(tablero, col, row) == ALIVE &&  live == 2)
    return ALIVE; //Tiene companieras
  return DEAD;    //Soledad o sobrepoblacion
}

//Intercambia dos tableros
//Esta funcion es necesaria para respetar la decicion de tener un tablero
//de lectura y otro de escritura
void boar_swap(board_t * destino, board_t * origen){
  board_t temp = *origen;
  *origen = *destino;
  *destino = temp;
}

/* IMPLEMENTACION DEL HILO */
//SE juega al juego de la vida sobre filas.
//Estas filas son solo trabajadas por un hilo espesifico.
void * game_conway(void * game_){
  game_t * game = (game_t *) game_;
  for (unsigned int i = 0; i < game->ciclos; i++){
    for (size_t j = 0; j*game->hilos + game->id < game->tableros->fila; j++){
      for (size_t k = 0; k < game->tableros->columna; k++){
        board_set(game->tableros[1], 
          k,
          j*game->hilos + game->id, 
          DeadLive(game->tableros[0], k, j*game->hilos + game->id));
      }
    }
    boar_swap(&game->tableros[0],&game->tableros[1]);    
    //Barrera de sinclonizacion
    barrier_wait(game->barrier);
  }
  pthread_exit(EXIT_SUCCESS);
}



board_t *congwayGoL(game_t *game){
  game->hilos = hilos(game);
  barrier_t bar;

  game_t clones[game->hilos];
  pthread_t th[game->hilos];
  
  barrier_init(&bar, game->hilos); //Barrera de sincronizacion

  for (unsigned int i = 0; i < game->hilos; i++) {
    clones[i] = game_clone(game, i,&bar);
    if (pthread_create(&th[i], NULL, &game_conway, &(clones[i])) != 0) {
      perror("Failed to create thread");
    }
  }
  for (unsigned int i = 0; i < game->hilos; i++) {
    if (pthread_join(th[i], NULL) != 0) {
      perror("Failed to join thread");
    }
  }
  
  barrier_destroy(&bar);
  
  return &(game->tableros)[game->ciclos % 2];
}
