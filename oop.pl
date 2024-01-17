    %%%% -*- Mode: Prolog -*-

    %%% Attia Christena 894887

    
    Definizione di una classe di base "object" senza campi e metodi
    class(object, [], [], []).

    % Definizione di un puntatore nullo "nullptr"
    point_to(ptr, nullptr).

    % Dichiarazione dinamica dei fatti predicati
    :- dynamic class/4.
    :- dynamic point_to/2.

    %%% Definisce una nuova classe e la inserisce nella base di conoscenza
    def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).

    % Secondo predicato: definisce una classe con il nome ClassName,
    genitori Parents e parti Parts.
    def_class(ClassName, Parents, Parts) :-
    \+ class(ClassName, _, _, _),
    maplist(validate_parent_class, Parents),
    extract_parts(Parts, TmpFields, TmpMethods), 
    initialize_fields(TmpFields, StandardFields),
    initialize_methods(TmpMethods, StandardMethods),
    obtain_specs(Parents, InheritedFields, _),
    get_type_field(StandardFields, InheritedFields, FinalFields),
    forall(member(field(_, Inst), FinalFields),
           is_instance(Inst)),
    assert_trampolines(StandardMethods),
    asserta(class(ClassName, Parents, FinalFields, StandardMethods)),
    !,
    writef("Classe %w creata con successo.\n", [ClassName]).

    % Predicato di fallback in caso la classe esista già.
    def_class(_, _, _) :- writef("Errore: la classe esiste già.\n"), fail.

    % Predicato di validazione per verificare che un genitore
    sia una classe esistente.
    validate_parent_class(Parent) :-
    class(Parent, _, _, _).

    % Predicato per separare una lista di termini in campi e metodi
    extract_parts([], [], []).
    extract_parts([Term | Rest], FieldsRest, MethodsRest) :-
    % Verifica se Term è un campo
    (   Term =.. [field | _],
        FieldsRest = [Term | Fields],
        % Chiamata ricorsiva per elaborare il resto della lista
        extract_parts(Rest, Fields, MethodsRest)
	% Verifica se Term è un metodo
	;   Term =.. [method | _],
        MethodsRest = [Term | Methods],
        extract_parts(Rest, FieldsRest, Methods)
	;   extract_parts(Rest, FieldsRest, MethodsRest)
    ).

    % Predicato per standardizzare i campi di una lista
    initialize_fields([], []).
    initialize_fields([Field | Rest], [StandardField | StandardRest]) :-
    process_field(Field, StandardField),
    initialize_fields(Rest, StandardRest).

    % Predicato per processare un campo e standardizzarlo
    process_field(field(Name, Value), field(Name, Obj)) :-
    \+ is_instance(Value),
    !,
    create(void, Value, Obj).

    process_field(field(Name, Value, Type), field(Name, Obj)) :-
    \+ is_instance(Value),
    !,
    create(Type, Value, Obj).

    process_field(field(Name, object(X, Y)), field(Name, object(X, Y))) :-   
    is_instance(X, Y),
    !.

    process_field(field(Name, object(Type1, Value1)),
		  field(Name, object(Type2, Value1))) :-
    is_instance(object(Type1, Value1)),
    !,
    is_subtype_of(Type1, Type2).



    % Predicato per inizializzare metodi
    initialize_methods([], []) :- !.
    initialize_methods([Method | Rest], [StandardMethod | StandardRest]) :-
    process_method(Method, StandardMethod),
    initialize_methods(Rest, StandardRest).

    % Predicato per elaborare un metodo, trasformando la sua rappresentazione
    % in una forma standard con un unico argomento aggiuntivo chiamato 'This'
    process_method(method(Name, Args, Body),
		   (StandardHead :- StandardBody)) :-
    % Costruisci la testa standard del metodo
    StandardHead =.. [Name, This | Args],
    % Sostituisci tutte le occorrenze di 'this' nel corpo del metodo
    replace_this(This, Body, StandardBody).

    % Predicato per sostituire tutte le occorrenze di 'this' in un
    % termine con un nuovo argomento passato come parametro 
    replace_this(_, Variable, Variable) :-
    % Se la variabile è non istanziata, non cè nulla da sostituire
    var(Variable),
    !.

    replace_this(This, OldTerm, NewTerm) :-
    compound(OldTerm),
    OldTerm =.. [Functor | OldArgs],
    maplist(replace_this(This), OldArgs, NewArgs),
    NewTerm =.. [Functor | NewArgs].

    % Sostituisce 'this' con il nuovo argomento solo se è una variabile
    replace_this(This, OldAtom, This) :-
    atomic(OldAtom),
    OldAtom = this,
    !.

    % Mantiene l'atomo inalterato se non è 'this'
    replace_this(_, OldAtom, OldAtom) :-
    atomic(OldAtom),
    OldAtom \= this.

    % Predicato per l'ereditarietà dei tipi dei campi
    get_type_field([], [], []) :- !.

    % Se la lista Fields è vuota, restituisce InheritedFields come risultato
    get_type_field(Fields, [], Fields) :-
    Fields \= [],
    !.

    % Se InheritedFields è vuota, restituisce Fields come risultato
    get_type_field([], InheritedFields, []) :-
    InheritedFields \= [],
    !.

    % Predicato ricorsivo per lereditarietà dei tipi dei campi
    get_type_field([Field1 | Rest1], [Field2 | Rest2],
		   [ResultField | RestResult]) :-
    process_field_type(Field1, Field2, ResultField),
    get_type_field(Rest1, Rest2, RestResult).

    % Predicato ausiliario per il confronto e
    % l'ereditarietà dei tipi dei campi
    process_field_type(field(FieldName, object(void, Value1)),
		       field(FieldName, object(Type, _)),
		       field(FieldName, object(Type, Value1))) :-
    !.
    process_field_type(field(FieldName, object(Type1, Value1)),
		       field(FieldName, object(Type2, _)),
		       field(FieldName, object(Type1, Value1))) :-
    Type1 \= void,
    is_subtype_of(Type1, Type2),
    !.
    process_field_type(Field1, Field2, Field2) :-
    Field1 \= Field2,
    !.

    % Predicato per creare oggetti senza parametri
    make(Name, Type) :-
    (   \+ point_to(Name, _),
        ground(Name),
        create(Type, Obj),
        asserta(point_to(Name, Obj)),
        !
	;   var(Name),
        create(Type, Obj),
        Name = Obj,
        !
	;   create(Type, Obj),
        Name = Obj
    ).

    % Predicato per creare oggetti con parametri
    make(Name, Type, Params) :-
    (   \+ point_to(Name, _),
        ground(Name),
        create(Type, Params, Obj),
        asserta(point_to(Name, Obj)),
        !
	;   var(Name),
        create(Type, Params, Obj),
        Name = Obj,
        !
	;   create(Type, Params, Obj),
        Name = Obj


    ).


    % Predicato ricorsivo per lereditarietà di campi e
    % metodi da una lista di genitori
    obtain_specs([], [], []) :- !.

    obtain_specs([Parent | Rest], Fields, Methods) :-
    !,
    class(Parent, Grandpas, ParentFields, ParentMethods),
    obtain_specs(Grandpas, GrandpasFields, GrandpasMethods),
    append_fields(ParentFields, GrandpasFields, TmpFields),
    append_methods(ParentMethods, GrandpasMethods, TmpMethods),
    obtain_specs(Rest, RestFields, RestMethods),
    append_fields(TmpFields, RestFields, Fields),
    append_methods(TmpMethods, RestMethods, Methods).


    % Predicato principale per concatenare due liste di campi mantenendo
    % solo i campi con nomi unici
    append_fields([], [], []).

    % Caso base: entrambe le liste sono vuote
    append_fields([field(Name, Value) | Rest1],
		  [field(Name, _) | Rest2],
		  [field(Name, Value) | Result]) :-
    append_fields(Rest1, Rest2, Result).

    % Caso base: la prima lista è vuota
    append_fields(X, [], X) :- X \= [].

    % Caso base: la seconda lista è vuota
    append_fields([], X, X) :- X \= [].

    % Caso generale: entrambe le liste non sono vuote
    append_fields([field(Name1, Value1) | Rest1],
		  [field(Name2, Value2) | Rest2],
		  Result) :-
    (   Name1 \= Name2
    ),
    append_fields_aux(Name1, Value1, Rest1,
		      [field(Name2, Value2) | Rest2], Result).

    append_fields_aux(Name1, Value1, Rest1, Rest2, Result) :-
    append_fields(Rest1, [field(Name1, Value1)], Tmp1),
    append_fields(Tmp1, Rest2, Result).


    % Caso base: entrambe le liste di metodi sono vuote
    append_methods([], [], []).

    % Caso base: la prima lista di metodi è vuota
    append_methods([Method1 | Rest1], [], Result) :-
    % Aggiungi il metodo corrente alla lista risultante
    append_method_to_list(Rest1, [Method1], Result).

    % Caso base: la seconda lista di metodi è vuota
    append_methods([], [Method2 | Rest2], Result) :-
    % Aggiungi il metodo corrente alla lista risultante
    append_method_to_list(Rest2, [Method2], Result).

    % Caso generale: entrambe le liste non sono vuote
    append_methods([(Head1 :- Body1) | Rest1],
		   [(Head2 :- _) | Rest2],
		   Result) :-
    % Estrai i nomi dei metodi
    extract_method_name(Head1, MethodName1),
    extract_method_name(Head2, MethodName2),
    (
     % Se i nomi dei metodi sono uguali, esegui la ricorsione 
     % sulla restante parte delle liste
     MethodName1 = MethodName2,
     append_methods(Rest1, Rest2, Result)
     ;
     % Se i nomi dei metodi sono diversi, aggiungi il metodo 
     % corrente alla lista risultante
     MethodName1 \= MethodName2,
     append_method_to_list(Rest1, [(Head2 :- _) | Rest2], Tmp1),
     % Esegui la ricorsione sulla nuova lista risultante
     append_methods([(Head1 :- Body1) | Tmp1], Rest2, Result)
    ).

    % Predicato per estrarre il nome di un metodo
    extract_method_name(Method, MethodName) :-
    Method =.. [MethodName | _].

    % Predicato per aggiungere un metodo a una lista
    append_method_to_list(List, Method, Result) :-
    (
     % Se la lista non è vuota, esegui la concatenazione
     List \= [],
     append(Method, List, Result)
     ;
     % Se la lista è vuota, il risultato è il metodo corrente
     List = [],
     Result = Method
    ).



    % Creazione di oggetti con valori predefiniti per tipi specifici
    create(void, object(void, null)) :- !.
    create(bool, object(bool, false)) :- !.
    create(number, object(number, 0)) :- !.
    create(float, object(float, 0.0)) :- !.
    create(rational, object(rational, 0)) :- !.
    create(atom, object(atom, '')) :- !.
    create(string, object(string, "")) :- !.
    create(integer, object(integer, 0)) :- !.

    create(Type, object(Type, DefaultFields)) :-
    is_class(Type),
    !,
    obtain_specs([Type], DefaultFields, _).

    % Creazione di oggetti con valori specificati per tipi specifici
    create(void, X, object(void, X)) :- !.
    create(bool, true, object(bool, true)) :- !.
    create(bool, false, object(bool, false)) :- !.
    create(number, X, object(number, X)) :- !, number(X).
    create(float, X, object(float, X)) :- !, float(X).
    create(rational, X, object(rational, X)) :- !, rational(X).
    create(atom, X, object(atom, X)) :- !, atom(X).
    create(string, X, object(string, X)) :- string(X), !.
    create(string, X, object(string, X)) :- atom(X), !.
    create(integer, X, object(integer, X)) :- !, integer(X).

    create(Type, Params, object(Type, Fields)) :-
    is_class(Type),
    obtain_specs([Type], DefaultFields, _),
    init_fields(DefaultFields, Params, Fields).

    % Caso base: nessun campo da inizializzare, restituisce la
    % lista dei campi invariata
    init_fields(Fields, [], Fields) :- !.

    % Inizializzazione dei campi utilizzando i parametri specificati
    init_fields([Field | FieldsRest], [Name = NewValue | ParamsRest],
		[FieldWithName | ResultRest]) :-
    Field = field(FieldName, object(Type, _)),
    FieldWithName = field(FieldName, NewObj),
    !,
    create(Type, NewValue, NewObj),
    init_fields(FieldsRest, ParamsRest, ResultRest).

    % Caso generale: il nome del campo da inizializzare è diverso dal
    % nome del parametro
    init_fields([field(FieldName1, Obj) | FieldsRest],
		[FieldName2 = NewValue | ParamsRest], Result) :-
    FieldName1 \= FieldName2,
    !,
    init_fields(FieldsRest, [FieldName2 = NewValue | ParamsRest], Tmp),
    Result = [field(FieldName1, Obj) | Tmp].


    % Estrae il valore di un campo dato un puntatore e un nome
    field(Ptr, Name, Value) :-
    inst(Ptr, Obj),
    field(Obj, Name, Value).

    % Estrae il valore di un campo da un oggetto, verificando
    % se il tipo è una classe
    field(object(_, Fields), Name, object(Class, Value)) :-
    member(field(Name, object(Class, Value)), Fields),
    is_class(Class),
    !.

    % Estrae il valore di un campo da un oggetto, verificando
    % se il tipo è di base
    field(object(_, Fields), Name, Value) :-
    member(field(Name, object(Type, Value)), Fields),
    is_builtin(Type),
    !.

    % Caso generale: il nome del campo non corrisponde, esegue la ricorsione
    field(object(_, [Field | FieldsRest]), Name, Value) :-
    Field = field(FieldName, _),
    FieldName \= Name,
    field(object(_, FieldsRest), Name, Value).

    %%% Restituisce il valore di un campo percorrendo una lista di campi.
    fieldx(InstanceName, FieldNames, Result) :-
    InstanceName \= object(_, _),
    inst(InstanceName, Obj),
    fieldx_helper(Obj, FieldNames, Result).

    fieldx(object(Type, Value), [FieldName], Result) :-
    field(object(Type, Value), FieldName, Result).

    fieldx(object(Type, Value), [FieldName | Rest], Result) :-
    field(object(Type, Value), FieldName, Tmp),
    fieldx(Tmp, Rest, Result).

    % Caso base: la lista dei nomi dei campi ha un solo elemento
    fieldx_helper(Obj, [FieldName], Result) :-
    field(Obj, FieldName, Result).

    % Caso generale: la lista dei nomi dei campi ha più di un elemento
    fieldx_helper(Obj, [FieldName | Rest], Result) :-
    field(Obj, FieldName, Tmp),
    fieldx_helper(Tmp, Rest, Result).

    % Verifica se un nome è il nome di una classe definita
    is_class(Name) :-
    class(Name, _, _, _),
    ground(Name).

    % Dato un puntatore, restituisce l oggetto corrispondente
    inst(Ptr, Obj) :- point_to(Ptr, Obj).
    % Verifica se un oggetto è un istanza di un certo tipo
    is_instance(object(Type, Value), Type) :-
    is_instance(object(Type, Value)).

    % Verifica se una istanza è valida
    is_instance(object(void, _)) :- !.
    is_instance(object(Type, Value)) :- type_check(Type, Value).

    % Predicato ausiliario per il controllo del tipo
    type_check(number, X) :- number(X).
    type_check(rational, X) :- rational(X).
    type_check(atom, X) :- atom(X).
    type_check(bool, true) :- !.
    type_check(bool, false) :- !.
    type_check(float, X) :- float(X).
    type_check(string, X) :- string(X).
    type_check(integer, X) :- integer(X).

    is_subtype_of(X, X) :- !.
    is_subtype_of(float, number) :- !.
    is_subtype_of(rational, float) :- !.
    is_subtype_of(integer, rational) :- !.
    is_subtype_of(string, atom) :- !.
    is_subtype_of(Class, ParentClass) :- class(Class, Parents, _, _),
    member(ParentClass, Parents).


    % Verifica se un tipo è una classe definita dall'utente
    is_custom_class(Type) :- class(Type, _, _, _).

    % Utilizza il costrutto subset/2 per verificare che una lista
    % sia un sottoinsieme dell'altra
    subset(Sub, Set) :- subset(Sub, Set, Set).
    subset([], _, _).
    subset([X | Xs], Set, OriginalSet) :-
    member(X, OriginalSet),
    subset(Xs, Set, OriginalSet).

    % Verifica se un tipo è built-in
    is_builtin(X) :- member(X, [void, bool, atom, string, number,
				float, rational, integer]).


    % Predicato "trampolino" per i metodi per richiamere i metodi
    % di una classe
    assert_trampolines([]).

    assert_trampolines([Trampoline | Rest]) :-
    assert_trampoline(Trampoline),
    assert_trampolines(Rest).

    assert_trampoline(Head :- _) :-
    extract_method_info(Head, Name, Arity, Args),
    abolish(Name, Arity),
    asserta((Head :- call_method(Name, Args))).

    % Estrae informazioni da un'intestazione di metodo, come nome,
    % arità e argomenti
    extract_method_info(Head, Name, Arity, Args) :-
    Head =.. [Name | Args],
    functor(Head, Name, Arity).

    % Invoca un metodo specificato, adattando il puntatore se necessario
    call_method(Name, [Ptr | Args]) :-
    inst(Ptr, Instance),
    !,
    call_method(Name, [Instance | Args]).
    call_method(Name, [object(Type, Value) | Args]) :-
    CalledHead =.. [Name | [object(Type, Value) | Args]],
    get_best_method([Type], Name, (Head :- Body)),
    asserta((Head :- Body)),
    !,
    call(CalledHead), !,      
    retract(Head :- Body), !.

    % Ottiene il miglior metodo da una lista di classi
    get_best_method([Class], MethodName, BestMethod) :-
    (
     class(Class, _, _, MethodsList),
     get_method_from_methods_list(MethodName, MethodsList, BestMethod)
     ;
     class(Class, Parents, _, MethodsList),
     \+ get_method_from_methods_list(MethodName, MethodsList, _),
     get_best_method(Parents, MethodName, BestMethod)
    ),
    !.

    get_best_method([Class | Classes], MethodName, BestMethod) :-
    Classes \= [],
    (
     get_best_method([Class], MethodName, BestMethod)
     ;
     class(Class, Parents, _, _),
     \+ get_best_method([Class], MethodName, _),
     get_best_method(Parents, MethodName, BestMethod)
    ),
    !,
    get_best_method(Classes, MethodName, BestMethod).

    % Ottiene un metodo specificato da una lista di metodi
    get_method_from_methods_list(MethodName, [(Head :- Body) | _],
				 (Head :- Body)) :-
    Head =.. [MethodName | _].
    get_method_from_methods_list(MethodName, [(Head :- _) | Rest],
				 Result) :-
    Head =.. [Functor | _],
    Functor \= MethodName,
    !,
    get_method_from_methods_list(MethodName, Rest, Result).
