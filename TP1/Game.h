#ifndef GAME_TYPES
#define GAME_TYPES
#include "Board.h"
#include "Barreras.h"
#include <pthread.h>

/******************************************************************************/
/* Representamos las células vivas como 'O' y las muertas como 'X' */

enum State {ALIVE = 79, DEAD = 88};
/******************************************************************************/
struct _game{
  board_t tableros[2];
  unsigned int ciclos, hilos,id;
  barrier_t * barrier;
};


typedef struct _game game_t;
/******************************************************************************/

/* Cargamos el juego desde un archivo */
game_t *loadGame(const char *filename);

/* Guardamos el tablero 'board' en el archivo 'filename' */
void writeBoard(board_t * board, const char *filename);

/* Simulamos el Juego de la Vida de Conway con tablero 'board' la cantidad de
ciclos indicados en 'cycles' en 'nuprocs' unidades de procesamiento*/
board_t *congwayGoL(game_t * game);


void game_destroy(game_t * game);


#endif
