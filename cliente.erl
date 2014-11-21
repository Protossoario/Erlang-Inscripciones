-module(cliente).
-export([login/2, registrar/2, logoff/0, inscribir_evento/1]).

%registro_node() -> 'registro@Eduardos-MacBook-Pro-5'.
registro_node() -> 'registro@Patricios-MacBook-Pro-4'.
%registro_node() -> 'registro@localhost'.

login(Usuario, Contrasenia) ->
	llama_registro({login, Usuario, Contrasenia}).

registrar(Usuario, Contrasenia) ->
	llama_registro({registrar, Usuario, Contrasenia}).

logoff() ->
  llama_registro({logoff}).

inscribir_evento(Evento) ->
  llama_registro({inscribir_evento, Evento}).

llama_registro(Mensaje) ->
	Node = registro_node(),
	monitor_node(Node, true),
	{servidor_registro, Node} ! {self(), usuario, Mensaje},
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
        error_usuario_no_loggeado ->
          io:format("El usuario que quiere inscribir no esta loggeado.~nPor favor inicie sesion primero.~n");
        _ ->
					io:format("ERROR: Respuesta del servidor no reconocida.~n")
      end;
    {servidor_programador, Respuesta} ->
			case Respuesta of
        error_evento_no_existe ->
          io:format("El evento que tratas de accesar no existe.~n");
        {listado_exitoso, Eventos} ->
          io:format("La lista de eventos es:~n~p~n", [Eventos])
      end;
    {servidor_evento, Respuesta} ->
      case Respuesta of
        ya_inscrito ->
          io:format("El usuario ya estaba inscrito a el evento.~n");
        inscripcion_exitosa ->
          io:format("La inscripcion ha sido exitosa.~n");
        evento_lleno ->
          io:format("El evento esta lleno. No te puedes inscribir.~n");
        usuario_eliminado ->
          io:format("Has sido eliminado del evento.~n");
        usuario_no_existe ->
          io:format("El usuario que quiere eliminar, no esta en ese evento.~n");
        {consulta_exitosa, Capacidad, Inscritos} ->
          io:format("La capacidad del evento es ~p.~n Y los inscritos son: ~n~p~n", [Capacidad, Inscritos])
      end;
    {nodedown, Node} ->
      no;
    _ ->
      io:format("ERROR: Respuesta en formato incorrecto.~n")
  after 30000 ->
          io:format("ERROR: Timeout.~n")
  end.
