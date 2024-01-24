%%% -*- Mode: Prolog -*-

%%% Attia Christena 894887

%%% Definizione di una classe di base "object" senza campi e metodi
class(object, [], [], []).

%%% Dichiarazione dinamica dei fatti predicati
:- dynamic class/4.
:- dynamic instance/2.

%%% Primo predicato: definisce una nuova classe e la
%%% inserisce nella base di conoscenza
def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).

%%% Secondo predicato: definisce una classe con il nome ClassName,
%%% genitori Parents e parti Parts.
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
    record_trampolines(StandardMethods),
    asserta(class(ClassName, Parents, FinalFields, StandardMethods)),
    !,
    writef("Classe %w creata con successo.\n", [ClassName]).

%%% Predicato di fallback in caso la classe esista già.
def_class(_, _, _) :- writef("Errore: la classe esiste già.\n"), fail.

%%% Predicato di validazione per verificare che un genitore
%%% sia una classe esistente.
validate_parent_class(Parent) :-
    class(Parent, _, _, _).

%%% Predicato per separare una lista di termini in campi e metodi
extract_parts([], [], []).
extract_parts([Term | Rest], FieldsRest, MethodsRest) :-
    %%% Verifica se Term è un campo
    (   Term =.. [field | _],
        FieldsRest = [Term | Fields],
        %%% Chiamata ricorsiva per elaborare il resto della lista
        extract_parts(Rest, Fields, MethodsRest)
    %%% Verifica se Term è un metodo
    ;   Term =.. [method | _],
        MethodsRest = [Term | Methods],
        extract_parts(Rest, FieldsRest, Methods)
    ;   extract_parts(Rest, FieldsRest, MethodsRest)
    ).


%%% Predicato per inizializzare i campi di una lista
initialize_fields([], []).
initialize_fields([Field | Rest], [StandardField | StandardRest]) :-
    process_field(Field, StandardField),
    initialize_fields(Rest, StandardRest).

%%% Predicato per processare un campo e standardizzarlo
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


%%% Predicato per inizializzare metodi
initialize_methods([], []) :- !.
initialize_methods([Method | Rest], [StandardMethod | StandardRest]) :-
    process_method(Method, StandardMethod),
    initialize_methods(Rest, StandardRest).

%%% Predicato per elaborare un metodo, trasformando la sua rappresentazione
%%% in una forma standard con un unico argomento aggiuntivo chiamato 'This'
process_method(method(Name, Args, Body),
	       (StandardHead :- StandardBody)) :-
    %%% Costruisce la testa standard del metodo
    StandardHead =.. [Name, This | Args],
    %%% Sostituisce tutte le occorrenze di 'this' nel corpo del metodo
    replace_this(This, Body, StandardBody).

%%% Predicato per sostituire tutte le occorrenze di 'this' in un
%%% termine con un nuovo argomento passato come parametro 
replace_this(_, Variable, Variable) :-
    var(Variable),
    !.
replace_this(This, OldTerm, NewTerm) :-
    compound(OldTerm),
    OldTerm =.. [Functor | OldArgs],
    maplist(replace_this(This), OldArgs, NewArgs),
    NewTerm =.. [Functor | NewArgs].
replace_this(This, OldAtom, This) :-
    atomic(OldAtom),
    OldAtom = this,
    !.
replace_this(_, OldAtom, OldAtom) :-
    atomic(OldAtom),
    OldAtom \= this.


%%% Predicato per l'ereditarietà dei tipi dei campi
get_type_field(Fields, InheritedFields, Result) :-
    (   
	%%%% Caso base: entrambe le liste sono vuote
	Fields = [], InheritedFields = [], Result = []
    ;
    %%% Caso generale: la lista dei campi da ereditare è vuota,
    %%% restituisce la lista dei campi
    Fields = [], InheritedFields \= [], Result = InheritedFields
    ;
    %%% Caso generale: la lista dei campi ereditati è vuota,
    %%% restituisce la lista dei campi originali
    Fields \= [], InheritedFields = [], Result = Fields
    ).


%%% Predicato ricorsivo per l'ereditarietà dei tipi dei campi
get_type_field([Field1 | Rest1], [Field2 | Rest2],
	       [ResultField | RestResult]) :-
    process_field_type(Field1, Field2, ResultField),
    get_type_field(Rest1, Rest2, RestResult).

%%% Predicato ausiliario per il confronto e
%%% l'ereditarietà dei tipi dei campi
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


%%% make/2: Predicato per creare oggetti senza parametri
make(Name, Type) :-
    is_class(Type),
    create(Type, Obj),
    asserta(instance(Name, Obj)),
    !.

%%% make/3: Predicato per creare oggetti con parametri
make(Name, Type, Params) :-
    is_class(Type),
    create(Type, Params, Obj),
    asserta(instance(Name, Obj)),
    !.


%%% obtain_specs/3: Predicato ricorsivo per l'ereditarietà di campi e
%%% metodi da una lista di genitori

%%% Caso base: la lista delle classi è vuota, restituisce liste
%%% vuote per Fields e Methods
obtain_specs([], [], []) :- !.

%%% Regola ricorsiva per ottenere le specifiche di Fields e Methods
%%% da una lista di classi
obtain_specs([Parent | Rest], Fields, Methods) :-
    class(Parent, Grandparents, ParentFields, ParentMethods),
    obtain_specs(Grandparents, GrandparentsFields, GrandparentsMethods),
    obtain_specs(Rest, RestFields, RestMethods),
    merge_specs(ParentFields, GrandparentsFields, RestFields, Fields),
    merge_specs(ParentMethods, GrandparentsMethods, RestMethods, Methods).

%%% Predicato ausiliario per unire le specifiche
merge_specs(Specs1, Specs2, Specs3, MergedSpecs) :-
    %%% Concatena le specifiche ottenute dalle diverse fonti
    append(Specs1, Specs2, TmpSpecs),
    append(TmpSpecs, Specs3, MergedSpecs).

%%% Predicato ausiliario per unire i campi
merge_fields(field(Name, Value1), field(Name, _), field(Name, Value1)).

%%% Predicato principale per concatenare due liste di campi mantenendo
%%% solo i campi con nomi unici
append_fields([], [], []).

%%% Caso base: la prima lista è vuota
append_fields([], X, X) :- X \= [].

%%% Caso base: la seconda lista è vuota
append_fields(X, [], X) :- X \= [].

%%% Predicato ausiliario  utilizzato per concatenare due liste di campi
append_fields([Field1 | Rest1], [Field2 | Rest2], [MergedField | Result]) :-
    merge_fields(Field1, Field2, MergedField),
    append_fields(Rest1, Rest2, Result).

%%% Caso generale: entrambe le liste non sono vuote
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

%%% Predicato utilizzato per concatenare due liste di metodi
append_methods([], [], []).

%%% Caso base: la prima lista di metodi è vuota, concatena
%%% la seconda lista
append_methods([], [Method2 | Rest2], Result) :-
    append_method_to_list(Rest2, [Method2], Result).

%%% Caso base: la seconda lista di metodi è vuota, concatenando
%%% la prima lista
append_methods([Method1 | Rest1], [], Result) :-
    append_method_to_list(Rest1, [Method1], Result).

%%% Caso generale
append_methods([Method1 | Rest1], [Method2 | Rest2], Result) :-
    extract_method_name(Method1, Name1),
    extract_method_name(Method2, Name2),
    (
        Name1 = Name2,
        append_methods(Rest1, Rest2, Result)
    ;
    Name1 \= Name2,
    append_method_to_list(Rest1, [Method2 | Rest2], Tmp1),
    append_methods([Method1 | Tmp1], Rest2, Result)
    ).

%%% Predicato ausiliario per estrarre il nome di un metodo
extract_method_name(Method, MethodName) :-
    Method =.. [MethodName | _].

%%% Predicato ausiliario per concatenare un metodo a una lista di metodi
append_method_to_list(List, Method, Result) :-
    (
        List \= [],
        append(Method, List, Result)
    ;
    List = [],
    Result = Method
    ).


%%% Creazione di oggetti con valori predefiniti
create(Type, object(Type, Fields)) :-
    is_class(Type),
    obtain_specs([Type], DefaultFields, _),
    init_fields(DefaultFields, [], Fields),
    !.

%%% Creazione di oggetti con valori specificati
create(void, X, object(void, X)) :- !.
create(Type, Val, object(Type, Val)) :- 
    (
        (Type = number, number(Val));
        (Type = float, float(Val));
        (Type = rational, rational(Val));
        (Type = atom, atom(Val));
        (Type = string, string(Val));
        (Type = integer, integer(Val))
    ),
    !.

%%% Creazione di oggetti complessi
create(Type, Params, object(Type, InitializedFields)) :-
    is_class(Type),
    obtain_specs([Type], DefaultFields, _),
    init_fields(DefaultFields, Params, InitializedFields).

%%% Caso base: nessun campo da inizializzare, restituisce la
%%% lista dei campi invariata
init_fields([], _, []).

%%% Inizializzazione dei campi utilizzando i parametri specificati
init_fields([Field | FieldsRest], Params, [InitializedField | ResultRest]) :-
    init_field(Field, Params, InitializedField),
    init_fields(FieldsRest, Params, ResultRest).

init_field(field(FieldName, object(Type, _)), Params,
	   field(FieldName, NewObj)) :-
    member(FieldName = NewValue, Params),
    create(Type, NewValue, NewObj).

%%% Caso generale: il nome del campo da inizializzare è diverso dal
%%% nome del parametro
init_field(Field, Params, Field) :-
    Field = field(FieldName1, _),
    \+ member(FieldName1 = _, Params).

%%% Predicato per ottenere il valore di un campo da un'istanza
field(InstanceName, Name, Value) :-
    instance(InstanceName, Obj),
    field(Obj, Name, Value).

% Predicato per ottenere il valore di un campo da un oggetto
field(object(_, Fields), Name, object(Class, Value)) :-
    member(field(Name, object(Class, Value)), Fields),
    is_class(Class),
    !.
field(object(_, Fields), Name, Value) :-
    member(field(Name, object(Type, Value)), Fields),
    is_builtin(Type),
    !.
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

%%% Caso base: la lista dei nomi dei campi ha un solo elemento
fieldx_helper(Obj, [FieldName], Result) :-
    field(Obj, FieldName, Result).

%%% Caso generale: la lista dei nomi dei campi ha più di un elemento
fieldx_helper(Obj, [FieldName | Rest], Result) :-
    field(Obj, FieldName, Tmp),
    fieldx_helper(Tmp, Rest, Result).


%%% Verifica se un nome è il nome di una classe definita
is_class(Name) :-
    class(Name, _, _, _),
    ground(Name).


%%% Predicato per ottenere l'oggetto dato un nome di istanza
inst(Name, Obj) :- instance(Name, Obj).


%%% Verifica se un oggetto è un istanza di un certo tipo
is_instance(object(Type, Value), Type) :-
    is_instance(object(Type, Value)).

%%% Verifica se una istanza è valida
is_instance(object(void, _)) :- !.
is_instance(object(Type, Value)) :- type_check(Type, Value).

%%% Predicato ausiliario per il controllo del tipo
type_check(number, X) :- number(X).
type_check(rational, X) :- rational(X).
type_check(atom, X) :- atom(X).
type_check(bool, true) :- !.
type_check(bool, false) :- !.
type_check(float, X) :- float(X).
type_check(string, X) :- string(X).
type_check(integer, X) :- integer(X).

%%% Predicato per verificare se una classe è sottotipo di un'altra
is_subtype_of(Class, ParentClass) :- 
    class(Class, Parents, _, _),
    member(ParentClass, Parents), !.
base_subtype(X, X).
base_subtype(float, number).
base_subtype(rational, float).
base_subtype(integer, rational).
base_subtype(string, atom).


%%% Utilizza il costrutto subset/2 per verificare che una lista
%%% sia un sottoinsieme dell'altra
subset(Sub, Set) :- subset(Sub, Set, Set).
subset([], _, _).
subset([X | Xs], Set, OriginalSet) :-
    member(X, OriginalSet),
    subset(Xs, Set, OriginalSet).

%%% Verifica se un tipo è built-in
is_builtin(X) :- member(X, [void, bool, atom, string, number,
			    float, rational, integer]).


%%% Predicato "trampolino" per richiamere i metodi di una classe
record_trampolines([]).
record_trampolines([Trampoline | Rest]) :-
    record_trampoline(Trampoline),
    record_trampolines(Rest).
record_trampoline(Head :- _) :-
    extract_and_record_trampoline(Head).

%%% Predicato ausiliario per estrarre le informazioni e asserire
%%% il trampolino
extract_and_record_trampoline(Head) :-
    extract_method_info(Head, Name, Arity, Args),
    retract_existing_method(Name, Arity),
    record_trampoline_method(Head, Name, Args).

%%% Predicato ausiliario per ritrarre il metodo esistente
retract_existing_method(Name, Arity) :-
    abolish(Name, Arity),
    retractall(Name).

%%% Predicato ausiliario per asserire il nuovo trampolino
record_trampoline_method(Head, Name, Args) :-
    asserta((Head :- call_method(Name, Args))).

%%% Estrae informazioni da un'intestazione di metodo come nome,
%%% arità e argomenti
extract_method_info(Head, Name, Arity, Args) :-
    Head =.. [Name | Args],
    functor(Head, Name, Arity).


%%% Invoca un metodo specificato, adattando il puntatore se necessario
call_method(Name, [Ptr | Args]) :-
    adapt_pointer(Ptr, Instance),
    !,
    call_method(Name, [Instance | Args]).
call_method(Name, [object(Type, Value) | Args]) :-
    build_called_head(Name, [object(Type, Value) | Args], CalledHead),
    find_and_execute_method([Type], Name, CalledHead).

%%% Predicato ausiliario per adattare il puntatore
adapt_pointer(Ptr, Instance) :-
    inst(Ptr, Instance),
    !.

%%% Predicato ausiliario per costruire la testa della chiamata
build_called_head(Name, Args, CalledHead) :-
    CalledHead =.. [Name | Args].

%%% Predicato ausiliario per trovare e eseguire il miglior metodo
find_and_execute_method([Type], Name, CalledHead) :-
    find_best_method([Type], Name, (Head :- Body)),
    asserta((Head :- Body)),
    (   call(CalledHead),
        retract(Head :- Body),
        !
    ;   retract(Head :- Body),
        fail
    ).


%%% Ottiene il miglior metodo da una lista di classi
find_best_method([Class], MethodName, BestMethod) :-
    (
	class(Class, _, _, MethodsList),
	get_method_from_methods_list(MethodName, MethodsList, BestMethod)
    ;
    class(Class, Parents, _, MethodsList),
    \+ get_method_from_methods_list(MethodName, MethodsList, _),
    find_best_method(Parents, MethodName, BestMethod)
    ),
    !.
find_best_method([Class | Classes], MethodName, BestMethod) :-
    Classes \= [],
    (
	find_best_method([Class], MethodName, BestMethod)
    ;
    class(Class, Parents, _, _),
    \+ find_best_method([Class], MethodName, _),
    find_best_method(Parents, MethodName, BestMethod)
    ),
    !,
    find_best_method(Classes, MethodName, BestMethod).


%%% Ottiene un metodo da una lista di metodi
get_method_from_methods_list(MethodName, [(Head :- Body) | _],
			     (Head :- Body)) :-
    extract_method_head(MethodName, Head).
get_method_from_methods_list(MethodName, [(Head :- _) | Rest], Result) :-
    extract_method_head(MethodName, Head),
    get_method_from_methods_list(MethodName, Rest, Result),
    !.

%%% Predicato ausiliario per estrarre la testa del metodo
extract_method_head(MethodName, Head) :-
    Head =.. [MethodName | _].

