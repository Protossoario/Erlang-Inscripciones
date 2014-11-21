#Programa de eventos en Erlang

## Guia de usabilidad

###Compilar

El código se encuentra en distintos archivos, los cuales cada uno tiene un namespace diferente. Para poder correr el programa se necesita compilar. En el folder del proyecto en bash correr:

```
erl -sname registro 
```

Con esto se correra la terminal de erlang para el namespace de registro. Dentro de la terminal de `Erlang` Correr

```
c(registro), c(cliente), c(organizador), c(programador), c(evento).
```
###Comandos
####Cliente

```
cliente:login(Usuario, Contrasenia).

cliente:registrar(Usuario, Contrasenia).

cliente:logoff().

cliente:inscribir_evento(Evento).

cliente:salir_evento(Evento).
```
  
####Organizador

```
organizador:login(Usuario, Contrasenia).

organizador:registrar(Usuario, Contrasenia).

organizador:logoff().

organizador:crear_evento(Evento, Capacidad).

organizador:eliminar_evento(Evento).

organizador:modificar_capacidad(Evento, Capacidad).

organizador:consultar_evento(Evento).

organizador:listar_eventos().
```
