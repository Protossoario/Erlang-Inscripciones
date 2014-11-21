-module(evento).
-compile(export_all).

% Busca en la lista de usuarios inscritos si el usuario existe.
% - existe: el usuario esta en la lista.
% - no_existe: el nodo no esta en la lista.
buscar_usuario(Usr, [ Usr | _ ]) -> existe;
buscar_usuario(Usr, [ _ | T ]) -> buscar_usuario(Usr, T);
buscar_usuario(_, _) -> no_existe.

% Busca en la lista de usuarios inscritos si el usuario existe.
% Si se encuentra el usuario, se elimina de la lista.
% Si no se encuentra, vuelve a buscar.
% Si la lista esta vacia, regresa una lista vacia.
eliminar_usuario(Usr, [ H | T ]) when Usr =/= H -> [H | eliminar_usuario (Usr, T)];
eliminar_usuario(Usr, [ Usr | T ]) -> eliminar_usuario (Usr, T);
eliminar_usuario(_, []) -> [].

% El evento maneja el numero maximo de inscritos,
% asi como la lista de usuarios inscritos.
evento(Capacidad, Inscritos) ->
    receive
        % Registrar un usuario en un evento.
        % - ya_inscrito: el usuario ya esta en la lista de inscritos
        % - inscripcion_exitosa: el usuario no existe, y la cantidad de inscritos es menor que la capacidad maxima
        % - evento_lleno: el usuario no existe, pero ya no cabe en el evento
        {De, {registrar, {Pid,Usuario}} } ->
            case buscar_usuario(Usuario, Inscritos) of
                existe ->
                    De ! {self(), ya_inscrito},
                    evento(Capacidad, Inscritos);
                no_existe ->
                    if
                        length(Inscritos) < Capacidad ->
                        Pid ! inscripcion_exitosa,
                            evento(Capacidad, [Inscritos|Usuario]);
                        true ->
                            Pid ! evento_lleno,
                            evento(Capacidad, Inscritos)
                    end
            end;

        % Eliminar un usuario de un evento.
        % - usuario_eliminado: el usuario se encontro en la lista de inscritos y se elimina
        % - usuario_no_existe: el usuario no estaba inscrito y por ende no se puede eliminar
        {De, {eliminar, {Pid,Usuario}} } ->
            case buscar_usuario(Usuario, Inscritos) of
                existe ->
                    Pid ! usuario_eliminado,
                    evento(Capacidad, eliminar_usuario(Usuario, Inscritos));
                no_existe ->
                    Pid ! usuario_no_existe,
                    evento(Capacidad, Inscritos)
            end;

        % Consulta la informacion de un evento.
        % - consulta_exitosa: envia la capacidad y la lista de inscritos de un evento.
        {De, consultar} ->
            De ! {consulta_exitosa, Capacidad, Inscritos},
            evento(Capacidad, Inscritos);

        % Cambia la capacidad de un evento.
        % - capacidad_cambiada: cambia la capacidad si el valor N es mayor que la cantidad de usuarios inscritos.
        % - error_capacidad_no_valida: no cambia la capacidad si se intenta actualizar con una cantidad menor de la de los usuarios inscritos.
        {De, {cambiar_capacidad, Pid, N}}->
            if
                N > length(Inscritos) ->
                    Pid ! {self(), capacidad_cambiada},
                    evento(N, Inscritos);
                true -> 
                    Pid ! {self(), error_capacidad_no_valida},
                    evento(Capacidad, Inscritos)
            end
    end.

