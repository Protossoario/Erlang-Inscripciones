-module(programador).
-compile(export_all).

evento(Capacidad, Inscritos) ->
    receive
        {actualizar, NuevaCapacidad, NuevaLista} -> evento(NuevaCapacidad,NuevaLista);
        consultar -> io:format("Capacidad:~p~nInscritos:~n~p~n", [Capacidad,Inscritos]),
                     evento(Capacidad,Inscritos);
        eliminar -> io:format("Eliminando evento...")
    end.
