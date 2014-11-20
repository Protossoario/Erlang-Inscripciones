-module(registro).
-export([inicio/0, servidor/1]).

login({Usuario, Contrasenia}, [ {Usuario, Contrasenia} | _ ]) -> aceptado;
login(Usuario, [ _ | T ]) -> login(Usuario, T);
login(_, _) -> rechazado.

buscar(Usuario, [ {Usuario, _} | _ ]) -> existe;
buscar(Usuario, [ _ | T ]) -> buscar(Usuario, T);
buscar(_, _) -> no_existe.

registrar({Usuario, Contrasenia}, Datos) -> [{Usuario, Contrasenia} | Datos].

servidor(Usuarios) ->
	receive
		{De, {login, Usuario, Contrasenia}} ->
			De ! {servidor_registro, login({Usuario, Contrasenia}, Usuarios)},
			servidor(Usuarios);
		{De, {registrar, Usuario, Contrasenia}} ->
			case buscar(Usuario, Usuarios) of
				existe ->
					De ! {servidor_registro, rechazado},
					servidor(Usuarios);
				no_existe ->
					De ! {servidor_registro, aceptado},
					servidor(registrar({Usuario, Contrasenia}, Usuarios));
				_ ->
					De ! {servidor_registro, rechazado},
					servidor(Usuarios)
			end
	end.

inicio() ->
	register(servidor_registro, spawn(registro, servidor, [[]])).
