#ifndef __BARRERAS_H__
#define __BARRERAS_H__ 

#include <pthread.h>
/* Definición de la estructura y sinónimo de tipo.*/
struct cond_barrier{
  unsigned int watting;
  unsigned int cant_threads;
  pthread_mutex_t mutexLock;
  pthread_cond_t watting_cond;
};

typedef struct cond_barrier barrier_t;
/************/

/************/
/* Operaciones*/

/* Creación de una barrera de condición, tomando como argumento la cantidad de
hilos que se van a esperar*/
int barrier_init(barrier_t *barr, unsigned int count);

/* Función *bloqueante* para esperar a los demás hilos */
int barrier_wait(barrier_t *barr);

/* Eliminación de la barrera */
int barrier_destroy(barrier_t *barr);
#endif