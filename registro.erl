-module(registro).
-export([inicio/0, servidor/2]).

% Busca en la lista de usuarios registrados para validar sus credenciales:
% - aceptado: el usuario existe y la contrasenia es valida
% - rechazado: el usuario pero la contrasenia es incorrecta
% - inexistente: el usuario no existe
login(Usuario, Contrasenia, [ {Usuario, Contrasenia} | _ ]) -> aceptado;
login(Usuario, _, [ {Usuario, _} | _ ]) -> rechazado;
login(Usuario, Contrasenia, [ _ | T ]) -> login(Usuario, Contrasenia, T);
login(_, _, _) -> inexistente.

% Busca en la lista el nombre de usuario para determinar si existe en la lista de registrados.
% - existe: el usuario esta en la lista de usuarios registrados.
% - no_existe: el usuario no esta registrado.
buscar(Usuario, [ {Usuario, _} | _ ]) -> existe;
buscar(Usuario, [ _ | T ]) -> buscar(Usuario, T);
buscar(_, _) -> no_existe.

% Busca en la lista de nodos loggeados para determinar el estado loggeado del nodo-usuario:
% - aceptado: el usuario ya esta loggeado en ese mismo nodo
% - nodo_loggeado: el nodo ya tiene un usuario con sesion abierta
% - usuario_loggeado: el usuario ya tiene una sesion abierta en otro nodo
% - no_loggeado: el usuario no ha iniciado sesion y el nodo esta disponible
loggeado(PID, Usuario, [ {PID, Usuario} | _ ] -> aceptado;
loggeado(PID, Usuario, [ {PID, _} | _ ] -> nodo_loggeado;
loggeado(_, Usuario, [ {_, Usuario} | _ ] -> usuario_loggeado;
loggeado(_, _, []) -> no_loggeado;
loggeado(PID, Usuario, [ _ | T ] -> loggeado(PID, Usuario, T);

% Agrega el usuario a la cabeza de la lista de usuarios registrados.
% Regresa la nueva lista.
registrar(Usuario, Contrasenia, Datos) -> [ {Usuario, Contrasenia} | Datos ].

% Agrega el nodo que inicia sesion a la cabeza de la lista de loggeados.
% Regresa la nueva lista.
loggear(PID, Usuario, Datos) -> [ {PID, Usuario} | Datos ].

% Elimina la tupla del nodo origen de la lista de loggeados.
% Regresa la nueva lista.
logoff(PID, [ {PID, _} | Datos ]) -> Datos;
logoff(PID, [ _ | Datos ]) -> logoff(PID, Datos);
logoff(_, []) -> [].

% El servidor maneja dos listas, una para usuarios registrados,
% y otra para los nodos con sesion iniciada.
% Registrados: {Usuario, Contrasenia}
% Loggeados: {PID, Usuario}
% Cuando un nodo inicia sesion, se agrega su PID y el nombre de usuario a la lista,
% para validar que un usuario no inicie sesion en mas de un nodo,
% y que ningun nodo tenga mas de dos usuarios con sesion iniciada.
servidor(Registrados, Loggeados) ->
	receive
		{De, {login, Usuario, Contrasenia}} ->
			case loggeado(De, Usuario) of
				aceptado ->
					De ! {servidor_registrado, sesion_iniciada},
					servidor(Registrados, Loggeados);
				no_loggeado ->
					case login(Usuario, Contrasenia, Registrados) of
						aceptado ->
							De ! {servidor_registro, aceptado},
							servidor(Registrados, loggear(De, Usuario, Loggeados));
						_ ->
							De ! {servidor_registro, rechazado},
							servidor(Registrados, Loggeados)
					end;
				_ ->
					De ! {servidor_registrado, sesion_invalida},
					servidor(Registrados, Loggeados)
			end;
		{De, {registrar, Usuario, Contrasenia}} ->
			case buscar(Usuario, Registrados) of
				existe ->
					De ! {servidor_registro, rechazado},
					servidor(Registrados, Loggeados);
				_ ->
					De ! {servidor_registro, aceptado},
					servidor(registrar(Usuario, Contrasenia, Registrados), Loggeados);
			end;
		{De, {logoff}} ->
			De ! {servidor_registro, bye},
			servidor(Registrados, logoff(De, Loggeados))
	end.

inicio() ->
	register(servidor_registro, spawn(registro, servidor, [[], []])).
