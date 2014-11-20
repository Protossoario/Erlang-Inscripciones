-module(cliente).
-export([login/2, registrar/2]).

registro_node() -> 'registro@Eduardos-MacBook-Pro-5'.

login(Usuario, Contrasenia) ->
	llama_registro({login, Usuario, Contrasenia}).

registrar(Usuario, Contrasenia) ->
	llama_registro({registrar, Usuario, Contrasenia}).

llama_registro(Mensaje) ->
	Node = registro_node(),
	monitor_node(Node, true),
	{servidor_registro, Node} ! {self(), Mensaje},
	receive
		{servidor_registro, Respuesta} ->
			monitor_node(Node, false),
			Respuesta;
		{nodedown, Node} ->
			no
	end.
