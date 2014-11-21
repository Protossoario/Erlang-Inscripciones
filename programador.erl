-module(programador).
-compile(export_all).

buscar_evento(Evento, [{Pid, Evento} | _ ]) -> Pid;
buscar_evento(Evento, [ _ | T ]) -> buscar_evento(Evento, T);
buscar_evento(_, _) -> no_existe.

eliminar_evento(Evn, [ {Pid, Evento} | T ]) when Evn =/= Pid -> [{Evn,Evento} | eliminar_evento (Evn, T)];
eliminar_evento(Evn, [ _ | T ]) -> eliminar_evento (Evn, T);
eliminar_evento(_, []) -> [].

% Eventos:
% [ { Pid, Nombre } ]
programador(Eventos) ->
    io:format("Estos son los eventos:~p~n",[Eventos]),
    receive
        {De, {inscribir, {Evento, Usuario}}} ->
            case buscar_evento(Evento, Eventos) of
                no_existe ->
                    De ! {self(), error_evento_no_existe};
                Pid ->
                    Pid ! {self(), {registrar, Usuario}}
            end,
            programador(Eventos);

        {De, listar_eventos} ->
            De ! {self(), {listado_exitoso, Eventos}},
            programador(Eventos);

        {De, {crear_eventos, {Nombre, Capacidad}}} ->
            case buscar_evento(Nombre, Eventos) of
                no_existe ->
                    De ! {self(), evento_creado},
                    programador([Eventos | {spawn_link(evento,evento,[Capacidad,[]]), Nombre}]);
                _ ->
                    De ! {self(), error_evento_existe},
                    programador(Eventos)
            end;

        {De, {eliminar_evento, {Nombre}}} ->
            case buscar_evento(Nombre, Eventos) of
                no_existe ->
                    De ! {self(), error_evento_no_existe},
                    programador(Eventos);
                Pid ->
                    De ! {self(), evento_borrado},
                    programador(eliminar_evento(Pid, Eventos))
            end;

        {De, {modificar_capacidad, {Nombre, N}}} ->
            case buscar_evento(Nombre, Eventos) of
                no_existe ->
                    De ! {self(), error_evento_no_existe};
                Pid ->
                    Pid ! {self(), {cambiar_capacidad, Pid, N}}
            end,
            programador(Eventos);

        {De, {listar_inscritos, Nombre}} ->
            case buscar_evento(Nombre, Eventos) of
                no_existe ->
                    De ! {self(), error_evento_no_existe};
                Pid ->
                    Pid ! {De, consultar}
            end,
            programador(Eventos)
    end.

iniciarProgramador() ->
    register(servidor_programador, spawn(programador, programador, [[]])).