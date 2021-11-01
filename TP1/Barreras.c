#include "Barreras.h"
#include <pthread.h>
#include <stdlib.h>

//inizialisa la barrera de sincronizacion
int barrier_init(barrier_t *barr, unsigned int count){
  pthread_mutex_init (&(barr->mutexLock), NULL);
  barr->watting = 0;
  barr->cant_threads = count;
  pthread_cond_init(&(barr->watting_cond),NULL);
  return count;
}

/* Función *bloqueante* para esperar a los demás hilos */
/* Cuando todos los hilos llegan a este punto son todos liberados*/
int barrier_wait(barrier_t *barr){
  int salida = 0;
  pthread_mutex_lock(&(barr->mutexLock));
  barr->watting++;
  if (barr->watting < barr->cant_threads){
    pthread_cond_wait(&(barr->watting_cond), &(barr->mutexLock));
  }
  else{
    barr->watting = 0;
    pthread_cond_broadcast(&(barr->watting_cond));
    salida = 1;
  }

  pthread_mutex_unlock(&(barr->mutexLock));

  return salida;
}

/* Eliminación de la barrera */
int barrier_destroy(barrier_t *barr){
  pthread_mutex_destroy(&(barr->mutexLock));
  pthread_cond_destroy(&(barr->watting_cond));
  return 0;
}

