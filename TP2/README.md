# Atomic broadcast:

Segundo trabajo práctico de **Sistemas Operativos I**

## ALUMNOS:
+ [Joaquin Caporalini](https://git.dcc.fceia.unr.edu.ar/jcaporalini)    e-mail: joaquicaporalini@gmail.com
+ [Daria Obukhova](https://git.dcc.fceia.unr.edu.ar/dobukhova)  e-mail: dari.obukhova@gmail.com

## Atomic broadcast: total order

El objetivo de este trabajo práctico es el de implementar un ledger distribuido, para esto sera necesario implementar primero un servicio de broadcast atómico y además este servicio deberá ser distribuido.

En nuestro caso se realizo utilizando **ERLANG** y toda la infraestructura planteada por este.

Como primeara iteración a la hora de solucionar el problema se puede plantear un enfoque con secuenciador al cual varios procesos accederán para pedir un orden en los mensajes. Si bien esto soluciona el problema no termina de ser distribuido, puesto que en caso de falla del proceso secuenciador la red completa fallaría.

La solución a este problema es lograr al distribuir el secuenciador, algo que se realizara mediante la implementación mediante acuerdo de los destinatarios (ISIS). En este caso los distintos proveedores del servicio de broadcast serán los encargados de llegar a un acuerdo sobre el orden de los mensajes.

### Distribución

El concepto de software distribuido es muy laxo, esto nos permitiría crear muchas veces los procesos dentro de la misma maquina virtual de erlan y así ofrecer el servicio con una sola maquina virtual, pero nosotros queríamos desligar realmente el funcionamiento de una única maquina virtual a varias. Para esto conectaremos varias maquinas virtuales de erlang creando una red mesh, sobre la cual distribuiremos el servicio, ahora si se pueden caer varias de las maquinas conectadas a la red y seguir brindando el servicio.

### cosas que se asumen porparte de la red y la ejecución.

* Solo se podrán caer los nodos que brindan el servicio y no se agregaran nuevos.
* El servicio de __ABroadcast__ funcionará en una red mesh creada con anterioridad.
* Todos los proveedores del servicio utilizan el mismo código.
* Cada proveedor del servicio se ejecutara en una maquina virtual de erlang, la cual está conectada a la red mesh.
* Los nodos que brinden el servicio proberán desconectarse cuando no estén proponiendo mensajes a la red.

### Procesos necesarios para resolver la tarea:

Para nuesta implmentación constaremos de 3 procesos, los cuales serán necesarios a la hora de brindar el servicio de __broadcast atómico__

1. Sender (sendLoop) :
    * encargado de difundir el mensaje en la red de prestadores del servicio. Se accede a él a través de `atomic:send`.
    * Además, busca el acuerdo de la prioridad de cada mensaje.

2. Dest (senderLoop):
    * Gestiona las prioridades que serán propuestas a la hora de enviar un mensaje.
    * Gestiona la recepción de peticiones de prioridad por parte de la red y prepara una respuesta.
    * Se encarga de mantener un registro de los mensajes de los cuales se esta deliverando su prioridad(orden).
    * Se encarga de mantener una un registro con los mensajes que ya tienen prioridad y esperan a ser entregados. Cuando un mensaje le llega la hora de ser entregados son derivados a __deliver__

3. Deliver (deliverLoop):
    * En este proceso se registran todos los procesos que quieren estar suscriptos a los mensajes enviados por la red de broadcast.
    * También pueden desuscribirse en cualquier momento.
    * Distribuye los mensajes entre los suscriptores.

### Utilización del servicio de Broadcast Atómico

Para comenzar es necesario elegir quienes en la red serán los designados para brindar el servicio, pueden ser todos o solo algunos de los nodos disponibles (maquinas de erlang en la red mesh). En las designadas se debe de lanzar los procesos mencionados anteriormente para poder hacer el broadcast. Para esto se utiliza `atomic:start`, con parámetro la lista de los nodos que proveerán el servicio.

```erl
atomic:start(['nodo@machine',  ...]).
```

una vez realizado este proceso en cada uno de los proveedores ya estamos en condiciones de poder enviar y recibir mensajes. Aunque todavía nos queda un pequeño detalle, los mensajes no son escuchados por nadie... esto no nos impide enviar mensajes, solo que estos no serán entregados a nadie. Un proceso que quiera recibir mensajes enviados debe suscribirse a la red, para ello nos dotamos de la función `atomic:register` de dos formas distintas:

1. Si nos queremos suscribirnos cuando estamos en el mismo nodo proveedor
```erl
atomic:register().
```

2. Si queremos suscribirnos especificando nodo por el cual lo haremos, ya sea porque estamos en un nodo que no provee el servicio o porque el nodos no dispone de este.
```erl
atomic:register('nodo@machine').
```

Ahora ya tenemos a nuestro primer oyente!
Los mensajes que se envíen tiene destino, así que enviemos con `atomic:send`. Devuelta, tenemos 2 métodos:

1. Si estamos en un nodo que provee el servicio podemos directamente hacer:
```erl
atomic:send(~MENSAJE~).
```

2. Mientras que si estamos en un nodo que no es proveedor o queremos utilizar otro nodo usaremos:
```erl
atomic:send(~MENSAJE~, 'nodo@machine').
```

Luego de hacer el envío, y que actúen todos los mecanismos del algoritmo, le llegara a los suscriptores del deliver.

Si algún suscriptor del deliver quiere dejar de recibir mensajes de la red puede hacerlo con `atomic:unRegister`. Si en un primer lugar te suscribiste sin indicar el nodo (esto porque estas en un nodo proveedor) lo haremos con:
```erl
atomic:register().
```
Y si especificamos un nodo cuando nos registramos:
```erl
atomic:register('nodo@machine').
```

## Leadger distribuido

Un Leadger es un objeto, mas particularmente, es una lista que solo crece en la cual se irán agregando los records que envíen los clientes. Esté objeto solo podrá soportar dos operaciones: `get` y `append`. En el caso de la implementación que propondremos sera distribuida, de la misma forma que se hizo en la sección anterior.

Siguiendo el paper [Formalizing and Implementing Distributed Ledger Objects](https://arxiv.org/abs/1802.07817), más precisamente los pseudocódigos 8 y 5 se dará una implementación en __erlang__ para su uso.
