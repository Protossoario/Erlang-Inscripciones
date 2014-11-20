-module(cliente).
-export([login/2, registrar/2, logoff/0]).

%registro_node() -> 'registro@Eduardos-MacBook-Pro-5'.
registro_node() -> 'registro@Patricios-MBP-4'.

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
					io:format("Bienvenido.~p");
				login_rechazado ->
					io:format("Usuario o contrasenia invalidos.~p");
				sesion_iniciada ->
					io:format("Ya iniciaste sesion.~p");
				sesion_invalida ->
					io:format("Alivianate a la verga, morro.~p");
				registro_aceptado ->
					io:format("Usuario registrado.~p");
				registro_rechazado ->
					io:format("Usuario ya existe.~p");
				logoff_aceptado ->
					io:format("Sesion cerrada.~p");
				logoff_rechazado ->
					io:format("No hay sesion iniciada.~p");
				_ ->
					io:format("Respuesta no reconocida (algo anda mal).~p")
			end;
		{nodedown, Node} ->
			no
	end.
