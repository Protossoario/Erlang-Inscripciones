-module(organizador).
-export([login/2, registrar/2, logoff/0, crear_evento/2, eliminar_evento/1, modificar_capacidad/2]).

%registro_node() -> 'registro@Eduardos-MacBook-Pro-5'.
registro_node() -> 'registro@Patricios-MacBook-Pro-4'.
%registro_node() -> 'registro@localhost'.

login(Usuario, Contrasenia) ->
	llama_registro({login, Usuario, Contrasenia}).

registrar(Usuario, Contrasenia) ->
	llama_registro({registrar, Usuario, Contrasenia}).

logoff() ->
  llama_registro({logoff}).

crear_evento(Evento, Capacidad) ->
  llama_registro({crear_evento, Evento, Capacidad}).

eliminar_evento(Evento) ->
  llama_registro({eliminar_evento, Evento}).

modificar_capacidad(Evento, Capacidad) ->
  llama_registro({modificar_evento, Evento, Capacidad}).

llama_registro(Mensaje) ->
	Node = registro_node(),
	monitor_node(Node, true),
	{servidor_registro, Node} ! {self(), organizador, Mensaje},
	receive
		{servidor_registro, Respuesta} ->
			monitor_node(Node, false),
			case Respuesta of
				login_aceptado ->
					io:format("Bienvenido Organizador.~n");
        login_aceptado_sesion_existente ->
          io:format("Bienvenido Organizador, tienes mas de una sesion abierta.~n");
				login_rechazado ->
					io:format("Usuario o contrasenia invalidos.~n");
				sesion_iniciada ->
					io:format("Ya iniciaste sesion.~n");
				sesion_invalida ->
					io:format("Primero has logoff del usuario actual.~n");
				registro_aceptado ->
					io:format("Organizador registrado.~n");
				registro_rechazado ->
					io:format("Organizador ya existe.~n");
				logoff_aceptado ->
					io:format("Sesion cerrada.~n");
				logoff_rechazado ->
					io:format("No hay sesion iniciada.~n");
        error_organizador_no_loggeado ->
          io:formato("No tienes permiso para hacer esta operacion.");
				_ ->
					io:format("ERROR: Respuesta del servidor no reconocida.~n")
			end;
     {servidor_programador, Respuesta} ->
			case Respuesta of
        error_evento_no_existe ->
          io:format("El evento que tratas de accesar no existe.~n");
        {listado_exitoso, Eventos} ->
          io:format("La lista de eventos es:~n~p~n", [Eventos]);
        evento_creado ->
          io:format("El evento fue creado satisfactoriamente.~n");
        error_evento_existe ->
          io:format("El evento que tratas de accesar ya existe.~n");
        evento_borrado ->
          io:format("El evento ha sido borrado.~n")
      end;
    {servidor_evento, Respuesta} ->
      case Respuesta of
        {consulta_exitosa, Capacidad, Inscritos} ->
          io:format("La capacidad del evento es ~p.~n Y los inscritos son: ~n~p~n", [Capacidad, Inscritos]);
        capacidad_cambiada ->
          io:format("La capacidad del evento ha sido cambiada.~n");
        error_capacidad_no_valida ->
          io:format("El numero de inscritos excede la capacidad elegida.~n")
      end;
		{nodedown, Node} ->
			no;
		_ ->
			io:format("ERROR: Respuesta en formato incorrecto.~n")
	after 30000 ->
		  io:format("ERROR: Timeout.~n")
	end.

