-module(cliente).
-export([login/2, registrar/2, logoff/0]).

%registro_node() -> 'registro@Eduardos-MacBook-Pro-5'.
%registro_node() -> 'registro@Patricios-MBP-4'.
registro_node() -> 'registro@localhost'.

login(Usuario, Contrasenia) ->
	llama_registro({login, Usuario, Contrasenia}).

registrar(Usuario, Contrasenia) ->
	llama_registro({registrar, Usuario, Contrasenia}).

logoff() ->
  llama_registro({logoff}).

llama_registro(Mensaje) ->
	Node = registro_node(),
	monitor_node(Node, true),
	{servidor_registro, Node} ! {self(), Mensaje},
	receive
		{servidor_registro, Respuesta} ->
			monitor_node(Node, false),
			case Respuesta of
				login_aceptado ->
					io:format("Bienvenido.~n");
        login_aceptado_sesion_existente ->
          io:format("Bienvenido, tienes mas de una sesion abierta.~n");
				login_rechazado ->
					io:format("Usuario o contrasenia invalidos.~n");
				sesion_iniciada ->
					io:format("Ya iniciaste sesion.~n");
				sesion_invalida ->
					io:format("Primero has logoff del usuario actual.~n");
				registro_aceptado ->
					io:format("Usuario registrado.~n");
				registro_rechazado ->
					io:format("Usuario ya existe.~n");
				logoff_aceptado ->
					io:format("Sesion cerrada.~n");
				logoff_rechazado ->
					io:format("No hay sesion iniciada.~n");
				_ ->
					io:format("ERROR: Respuesta del servidor no reconocida.~n")
			end;
		{nodedown, Node} ->
			no;
		_ ->
			io:format("ERROR: Respuesta en formato incorrecto.~n")
	after 30000 ->
		  io:format("ERROR: Timeout.~n")
	end.
