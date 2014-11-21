-module(evento).
-compile(export_all).

% Busca en la lista de nodos loggeados si el nodo existe.
% - existe: el nodo tiene un usuario loggeado.
% - no_existe: el nodo no esta en la lista de loggeados.
buscar_usuario(Usr, [ Usr | _ ]) -> existe;
buscar_usuario(Usr, [ _ | T ]) -> buscar_usuario(Usr, T);
buscar_usuario(_, _) -> no_existe.

eliminar_usuario(Usr, [ H | T ]) when Usr =/= H -> [H | eliminar_usuario (Usr, T)];
eliminar_usuario(Usr, [ Usr | T ]) -> eliminar_usuario (Usr, T);
eliminar_usuario(_, []) -> [].

evento(Capacidad, Inscritos) ->
    receive
        {De, {registrar, Usuario} } ->
            case buscar_usuario(Usuario, Inscritos) of
                existe ->
                    De ! {self(), ya_inscrito},
                    evento(Capacidad, Inscritos);
                no_existe ->
                    if
                        length(Inscritos) < Capacidad ->
                            De ! {self(), inscripcion_exitosa},
                            evento(Capacidad, [Inscritos|Usuario]);
                        true ->
                            De ! {self(), evento_lleno},
                            evento(Capacidad, Inscritos)
                    end
            end;

        {De, {eliminar, Usuario} } ->
            case buscar_usuario(Usuario, Inscritos) of
                existe ->
                    De ! {self(), usuario_eliminado},
                    evento(Capacidad, eliminar_usuario(Usuario, Inscritos));
                no_existe ->
                    De ! {self(), usuario_no_existe},
                    evento(Capacidad, Inscritos)
            end;

        {De, consultar} ->
            De ! {self(), {consulta_exitosa, Capacidad, Inscritos}},
            evento(Capacidad, Inscritos);

        {De, {cambiar_capacidad, N}}->
            if
                N > length(Inscritos) ->
                    De ! {self(), capacidad_cambiada},
                    evento(N, Inscritos);
                true -> 
                    De ! {self(), error_capacidad_no_valida},
                    evento(Capacidad, Inscritos)
            end
    end.

