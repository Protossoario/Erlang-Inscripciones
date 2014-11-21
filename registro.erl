-module(registro).
-export([inicio/0, servidor/2]).

% Tipos:
% - organizador
% - usuario
% Registrados:
% [{Tipo, Usuario, Contrasenia}]
%
% Loggeados:
% [{PID, Tipo, Usuario}]
%
% Busca en la lista de usuarios registrados para validar sus credenciales:
% - aceptado: el usuario existe y la contrasenia es valida
% - rechazado: el usuario pero la contrasenia es incorrecta
% - inexistente: el usuario no existe
login(Usuario, Contrasenia, Tipo, [ {Tipo, Usuario, Contrasenia} | _ ]) -> aceptado;
login(Usuario, _, Tipo, [ {Tipo, Usuario, _} | _ ]) -> rechazado;
login(Usuario, Contrasenia, Tipo, [ _ | T ]) -> login(Usuario, Contrasenia, Tipo, T);
login(_, _, _, _) -> inexistente.

% Busca en la lista el nombre de usuario para determinar si existe en la lista de registrados.
% - existe: el usuario esta en la lista de usuarios registrados.
% - no_existe: el usuario no esta registrado.
buscar_usuario(Usuario, Tipo, [ {Tipo, Usuario, _} | _ ]) -> existe;
buscar_usuario(Usuario, Tipo, [ _ | T ]) -> buscar_usuario(Usuario, Tipo, T);
buscar_usuario(_,_,_) -> no_existe.

% Busca en la lista de usuarios loggeados el nombre del usuario a partir de un pid
obtener_nombre(Pid, Tipo, [ {Pid, Tipo, Usuario} | _ ]) -> Usuario;
obtener_nombre(Pid, Tipo, [ _ | T ]) -> obtener_nombre(Pid, Tipo, T);
obtener_nombre(_,_,_) -> no_existe.

organizador_loggeado(PID, Tipo ,  [ {PID, organizador, _} | _ ]) -> existe;
organizador_loggeado(PID, Tipo, [ _ | T ]) -> organizador_loggeado(PID, Tipo, T);
organizador_loggeado(_, _, _) -> no_existe.

% Busca en la lista de nodos loggeados si el nodo existe.
% - existe: el nodo tiene un usuario loggeado.
% - no_existe: el nodo no esta en la lista de loggeados.
buscar_nodo(PID, [ {PID, _, _} | _ ]) -> existe;
buscar_nodo(PID, [ _ | T ]) -> buscar_nodo(PID, T);
buscar_nodo(_, _) -> no_existe.

% Busca en la lista de nodos loggeados para determinar el estado loggeado del nodo-usuario:
% - aceptado: el usuario ya esta loggeado en ese mismo nodo
% - nodo_loggeado: el nodo ya tiene un usuario con sesion abierta
% - usuario_loggeado: el usuario ya tiene una sesion abierta en otro nodo
% - no_loggeado: el usuario no ha iniciado sesion y el nodo esta disponible
loggeado(PID, Usuario, Tipo, [ {PID, Tipo, Usuario} | _ ]) -> aceptado;
loggeado(PID, _, _, [ {PID, _, _} |_ ]) -> nodo_loggeado;
loggeado(_, Tipo, Usuario, [ {_, Tipo, Usuario} | _ ]) -> usuario_loggeado;
loggeado(PID, Usuario, Tipo, [ _ | T ]) -> loggeado(PID, Usuario, Tipo, T);
loggeado(_, _, _, _) -> no_loggeado.

% Agrega el usuario a la cabeza de la lista de usuarios registrados.
% Regresa la nueva lista.
registrar(Tipo, Usuario, Contrasenia, Datos) -> [ {Tipo, Usuario, Contrasenia} | Datos ].

% Agrega el nodo que inicia sesion a la cabeza de la lista de loggeados.
% Regresa la nueva lista.
loggear(PID, Usuario, Tipo, Datos) -> [ {PID, Tipo, Usuario} | Datos ].

% Elimina la tupla del nodo origen de la lista de loggeados.
% Regresa la nueva lista.
logoff(PID, _, [ {PID, _, _} | Datos ]) -> Datos;
logoff(PID, Tipo, [ _ | Datos ]) -> logoff(PID, Tipo, Datos);
logoff(_, _, []) -> [].

% El servidor maneja dos listas, una para usuarios registrados,
% y otra para los nodos con sesion iniciada.
%
% Registrados: {Usuario, Contrasenia}
% Loggeados: {PID, Usuario}
%
% Cuando un nodo inicia sesion, se agrega su PID y el nombre de usuario a la lista,
% para validar que un usuario no inicie sesion en mas de un nodo,
% y que ningun nodo tenga mas de dos usuarios con sesion iniciada.
%
% Envia al cliente los siguientes mensajes:
% - login_aceptado: cuando recibe un mensaje de login con credenciales validas
% - login_rechazado: cuando recibe un mensaje de login con credenciales invalidas
% - sesion_iniciada: cuando recibe un mensaje de login pero el usuario ya inicio sesion en ese nodo
% - sesion_invalida: cuando recibe un mensaje de login pero el nodo o el usuario tiene una sesion abierta
% - registro_aceptado: cuando recibe un mensaje de registrar y se completa con exito
% - registro_rechazado: cuando recibe un mensaje de registrar pero el usuario ya existe
% - logoff_aceptado: cuando recibe un mensaje de logoff y se cierra sesion con exito
% - logoff_rechazado: cuando recibe un mensaje de logoff de un nodo que no ha iniciado sesion
servidor(Registrados, Loggeados) ->
	% Imprimir el estado del servidor
	io:format("~n-------------------------------------------------------------~n"),
	io:format("Registrados:~n~p~nLoggeados:~n~p~n", [Registrados, Loggeados]),
	io:format("-------------------------------------------------------------~n"),
	receive
		{De, UserType, {login, Usuario, Contrasenia}} ->
			case loggeado(De, Usuario, UserType, Loggeados) of
				aceptado ->
					De ! {servidor_registro, sesion_iniciada},
					servidor(Registrados, Loggeados);
				no_loggeado ->
					case login(Usuario, Contrasenia, UserType, Registrados) of
						aceptado ->
							De ! {servidor_registro, login_aceptado},
							servidor(Registrados, loggear(De, Usuario, UserType, Loggeados));
						_ ->
							De ! {servidor_registro, login_rechazado},
							servidor(Registrados, Loggeados)
					end;
        usuario_loggeado ->
          De ! {servidor_registro, login_aceptado_sesion_existente},
					servidor(Registrados, Loggeados);
				_ ->
					De ! {servidor_registro, sesion_invalida},
					servidor(Registrados, Loggeados)
			end;
		{De, UserType, {registrar, Usuario, Contrasenia}} ->
			case buscar_usuario(Usuario, UserType, Registrados) of
				existe ->
					De ! {servidor_registro, registro_rechazado},
					servidor(Registrados, Loggeados);
				_ ->
					De ! {servidor_registro, registro_aceptado},
					servidor(registrar(UserType, Usuario, Contrasenia, Registrados), Loggeados)
			end;
		{De, UserType, {logoff}} ->
			case buscar_nodo(De, Loggeados) of
				existe ->
					De ! {servidor_registro, logoff_aceptado},
					servidor(Registrados, logoff(De, UserType, Loggeados));
				_ ->
					De ! {servidor_registro, logoff_rechazado},
					servidor(Registrados, Loggeados)
			end;
        {De, UserType, {inscribir_evento, Evento}} ->
            case obtener_nombre(De, UserType, Loggeados) of
                no_existe -> 
                    De ! {servidor_registro, error_usuario_no_loggeado};
                Nombre -> 
                    servidor_programador ! { De, {inscribir, { Evento, Nombre } }}
            end,
            servidor(Registrados, Loggeados);
        
        % Mensajes de organizador
        {De, UserType, {crear_evento, Evento, Capacidad}} ->
            case organizador_loggeado(De, UserType, Loggeados) of
                no_existe ->
                    De ! {servidor_registro, error_organizador_no_loggeado};
                existe ->
                    servidor_programador ! { De, {crear_evento, { Evento, Capacidad } }}
            end,
            servidor(Registrados, Loggeados);
        {De, UserType, {eliminar_evento, Evento}} ->
            case organizador_loggeado(De, UserType, Loggeados) of
                no_existe ->
                    De ! {servidor_registro, error_organizador_no_loggeado};
                existe ->
                    servidor_programador ! { De, {eliminar_evento, Evento }}
            end,
            servidor(Registrados, Loggeados);
        {De, UserType, {modificar_evento, Evento, Capacidad}} ->
            case organizador_loggeado(De, UserType, Loggeados) of
                no_existe ->
                    De ! {servidor_registro, error_organizador_no_loggeado};
                existe ->
                    servidor_programador ! { De, {modificar_capacidad, { Evento, Capacidad } }}
            end,
            servidor(Registrados, Loggeados);
		_ ->
			io:format("[SERVIDOR_REGISTRO]: Mensaje no reconocido.~n")
	end.

inicio() ->
	register(servidor_registro, spawn(registro, servidor, [[], []])).
