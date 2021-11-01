/* Librerias propias*/
#include "Board.h"  //Cabeceras para las funciones del tablero

/* Librerias provistas por c*/
#include <stdlib.h> //Entrada y salida
#include <stdio.h>  //Manejo de memoria
#include <math.h>   //Uso de la funcion 'log10'
#include <string.h> //Uso de 'memset'

/* Librerias auxiliares */
#include <assert.h> //Tests en el codigo

//Esta Funcion pide la memoria necesaria para un tablero y ademas inicializa los 
//que vinen sujetos a ella. Todos estos son almacenados en la estructura que es
//recibida como argumento.
int board_init(board_t *board, size_t col, size_t row){
  board->columna = col;
  board->fila = row;
  board->board = malloc(sizeof(char *) * row); //Pide memoria para alojar las filas
  if (!board->board)
    return EXIT_FAILURE; //No se pudo obtener memoria.
  for (size_t i = 0; i < row; i++){
    board->board[i] = malloc(sizeof(char) * (col + 1)); //Aloja las filas
    board->board[i][col] = '\0';  //Este caracter es util para imprimir las filas.
  }
  return EXIT_SUCCESS;
}

//El comportamiento es igual al de la funcion anterior, con la diferencia de que
//esta coloca valores por defecto en las casillas
int board_init_def(board_t *board, size_t col, size_t row, char def){
  board->columna = col;
  board->fila = row;
  board->board = malloc(sizeof(char *) * row);
  if (!board->board)
    return EXIT_FAILURE; //No se pudo obtener memoria.
  for (size_t i = 0; i < row; i++){
    board->board[i] = malloc(sizeof(char) * (col + 1));
    memset(board->board[i],def,col);  //Seteo eficiente de la fila
    board->board[i][col] = '\0';
  }
  return EXIT_SUCCESS;
}

//Dado un tablro, una columan y una fila nos devuelve el valor de dicha casilla.
//Esta funcion solo toma como valida a casillas dentro del tablro.
char board_get(board_t board, unsigned int col, unsigned int row){
  if(board.columna <= col){
    perror("la columana solisitada esta fuera de la matriz");
    exit(EXIT_FAILURE);
  }
  if (board.fila <= row){
    perror("la fila solisitada esta fuera de la matriz");
    exit(EXIT_FAILURE);
  }
  return board.board[row][col];
}

//Dado un tablro, una columan y una fila nos devuelve el valor de dicha casilla.
//Esta funcion toma como valida a casillas dentro del tablro y si exeden los
//limites de este.
char board_get_round(board_t board, int col, int row){
  if (col < 0)
    col = board.columna - col;
  if (row < 0)
    row = board.fila - row;
  return board.board[row % board.fila][col % board.fila];
}

//Dado un tablero y una casilla en formafo columna, fila, se setea el tablero
//con el caracter dado como parametro val
int board_set(board_t board, unsigned int col, unsigned int row, char val){
  if(board.columna <= col || board.fila <= row) 
    perror("La fila o la columana esta fuera de la matriz");
  board.board[row][col] = val;
  return EXIT_SUCCESS;
}

//Esta funcion es capaz de cargar un tablero de juego desde un arrego.
//En el arreglo se encuentran cada fila del tablero codificadas en RLE y
//separadas por un caracter '\0'
int board_load(board_t *board, char *str){
  unsigned int carga_fila, carga_columnas, cant;
  carga_fila = carga_columnas = 0;

  unsigned long offset = 0;
  char c;

  while (carga_fila < board->fila){
    sscanf(&(str[offset]),"%u%c", &cant, &c);
    memset( &(board->board[carga_fila][carga_columnas]), c, cant);
    carga_columnas += cant;
    if (carga_columnas >= board->columna){
      carga_fila++;
      carga_columnas = 0;
      offset++;
    }
    offset = offset + (int) log10(cant) + 2;
  }
  return EXIT_SUCCESS;
}

//Esta funcion exporta un tablero a un arreglo de caracteres,
//separando cada fila con '\0'. Se asume que el tablero entra
//en res.
void board_show(board_t board, char *res){
  unsigned int col = 0;   //En que columna se encuntra la lectura
  unsigned int cont = 0;  //Cantidad de caracteres iguales que se leyeron
  unsigned int total = 0; //Posicion del arreglo en la que me encuentro

  char c;

  for (unsigned row = 0; row < board.fila; row++){
    col = 0;     
    while (col < board.columna){
      c = board_get(board, col,row);
      cont = 0;
      while (col < board.columna && board_get(board, col,row) == c) {
        cont++;
        col++;
      }
      total += sprintf(&(res[total]),"%u%c", cont, c);//Transcripcion al
                                                      //arreglo
    }
    res[total] = '\0';  //Separando cada linea codificada
    total++;
  }
}

//Libra la memoria que se pidiera, ademas se setean en 0 los valores
//que no seran liverados.
void board_destroy(board_t *board){
  if (!board)
    perror("No pudo liberar un NULL");
  for (size_t i = 0; i < board->fila; i++)
    free(board->board[i]);
  free(board->board);
  board->columna = 0;
  board->fila = 0;
}