-module(registro).
-export([inicio/0, servidor/2]).

login({Usuario, Contrasenia}, [ {Usuario, Contrasenia} | _ ]) -> aceptado;
login(Usuario, [ _ | T ]) -> login(Usuario, T);
login(_, _) -> rechazado.

buscar(Usuario, [ {Usuario, _} | _ ]) -> existe;
buscar(Usuario, [ _ | T ]) -> buscar(Usuario, T);
buscar(_, _) -> no_existe.

registrar({Usuario, Contrasenia}, Datos) -> [{Usuario, Contrasenia} | Datos].

servidor(Usuarios, UsuarioLoggeado) ->
	receive
		{De, {login, Usuario, Contrasenia}} ->
      if undefined == UsuarioLoggeado ->
           case login({Usuario, Contrasenia}, Usuarios) of
             aceptado ->
               De ! {servidor_registro, aceptado},
               servidor(Usuarios, Usuario);
             _ ->
               De ! {servidor_registro, rechazado},
               servidor(Usuarios, undefined)
           end;
         true ->
           De ! {servidor_registro, rechazado},
           servidor(Usuarios, UsuarioLoggeado)
      end;
		{De, {registrar, Usuario, Contrasenia}} ->
			case buscar(Usuario, Usuarios) of
				existe ->
					De ! {servidor_registro, rechazado},
					servidor(Usuarios, UsuarioLoggeado);
				no_existe ->
					De ! {servidor_registro, aceptado},
					servidor(registrar({Usuario, Contrasenia}, Usuarios), UsuarioLoggeado);
				_ ->
					De ! {servidor_registro, rechazado},
					servidor(Usuarios, UsuarioLoggeado)
			end;
    {De, {logoff}} ->
      if
        undefined == UsuarioLoggeado ->
          De ! {servidor_registro, rechazado};
        true ->
         De ! {servidor_registro, aceptado}
      end,
      servidor(Usuarios, undefined)
	end.

inicio() ->
	register(servidor_registro, spawn(registro, servidor, [[], undefined])).
