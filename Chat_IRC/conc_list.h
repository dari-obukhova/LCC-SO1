#ifndef __CONC_LIST_H__
#define __CONC_LIST_H__

#include <pthread.h>

typedef struct _conc_list_node_{
  char * nick;
  int socket;
  struct _conc_list_node_ * siguiente;
} conc_list_node;

typedef struct _conc_list_{
  pthread_mutex_t lock;
  conc_list_node * list;
} conc_list;

/* Inicializa una nueva lista*/
conc_list * conc_list_init();

/* Agrega un nuevo elemento al comienzo de la lista*/
int conc_list_add(conc_list * list, char * nick, int socket);

/* Elimina una lista */
/* OBS: solo destruye listas vacias */
int conc_list_delete(conc_list * list);

/* Elimina un elemento de la lista */
/* OBS: no destruye lo que el nodo contenga */
int conc_list_rm(conc_list * list, void * dato, int type);

/* busca un nodo dado su nick */
conc_list_node * conc_list_serch(conc_list * list, char * nick);

#endif /* __CONC_LIST_H__ */