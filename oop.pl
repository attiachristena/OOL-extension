class(object, [], [], []).

point_to(ptr, nullptr).

:- dynamic class/4.
:- dynamic point_to/2.


%%% Definisce una nuova classe e la inserisce nella base di conoscenza

% Primo predicato: definisce una classe con il nome ClassName e nessun genitore.
def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).

% Secondo predicato: definisce una classe con il nome ClassName, genitori Parents e parti Parts.
def_class(ClassName, Parents, Parts) :-
    % Verifica che la classe non esista già nella base di conoscenza.
    \+ class(ClassName, _, _, _),
    
    % Verifica che tutti i genitori siano classi esistenti.
    maplist(validate_parent_class, Parents),
    
    % Estrae metodi e campi dalle parti Parts.
    get_methods_and_fields(Parts, TmpFields, TmpMethods),
    
    % Standardizza i campi e i metodi ottenuti.
    standardize_fields(TmpFields, StandardFields),
    standardize_methods(TmpMethods, StandardMethods),
    
    % Eredità: ottiene i campi ereditati dai genitori.
    inherit(Parents, InheritedFields, _),
    
    % Aggiorna i tipi dei campi ereditati.
    inherit_field_type(StandardFields, InheritedFields, FinalFields),
    
    % Verifica che tutte le istanze dei campi siano effettivamente istanze di oggetti.
    forall(member(field(_, Inst), FinalFields),
           is_instance(Inst)),
    
    % Aggiunge i metodi standard (trampolini) alla base di conoscenza.
    assert_trampolines(StandardMethods),
    
    % Aggiunge la nuova classe alla base di conoscenza.
    asserta(class(ClassName, Parents, FinalFields, StandardMethods)),
    
    % Stampa un messaggio di successo.
    !,
    writef("Classe %w creata con successo.\n", [ClassName]).

% Predicato di fallback in caso la classe esista già.
def_class(_, _, _) :- writef("Errore: la classe esiste già.\n"), fail.

% Predicato di validazione per verificare che un genitore sia una classe esistente.
validate_parent_class(Parent) :-
    class(Parent, _, _, _).


%%% Data una lista di metodi e campi, separa campi e metodi in due
%%% liste differenti
get_methods_and_fields([], [], []).
get_methods_and_fields([Term | Rest], FieldsRest, MethodsRest) :-
    % Verifica se Term è un campo
    (   Term =.. [field | _],
        FieldsRest = [Term | Fields],
        get_methods_and_fields(Rest, Fields, MethodsRest)
    % Verifica se Term è un metodo
    ;   Term =.. [method | _],
        MethodsRest = [Term | Methods],
        get_methods_and_fields(Rest, FieldsRest, Methods)
    % Se Term non è né campo né metodo, continua la ricorsione
    ;   get_methods_and_fields(Rest, FieldsRest, MethodsRest)
    ).

%%% Riscrive i campi contenuti in una lista in una forma standard
%%% scelta in fase di implementazione, ovvero field(Nome, Oggetto)
standardize_fields([], []).
standardize_fields([Field | Rest], [StandardField | StandardRest]) :-
    % Processa il campo e lo standardizza
    process_field(Field, StandardField),
    standardize_fields(Rest, StandardRest).

% Predicato per processare un campo e standardizzarlo
process_field(field(Name, Value), field(Name, Obj)) :-
    % Se il valore non è un'istanza, crea un nuovo oggetto void e lo assegna a Obj
    \+ is_instance(Value),
    !,
    new(void, Value, Obj).
process_field(field(Name, Value, Type), field(Name, Obj)) :-
    % Se il valore non è un'istanza, crea un nuovo oggetto del tipo specificato e lo assegna a Obj
    \+ is_instance(Value),
    !,
    new(Type, Value, Obj).
process_field(field(Name, object(X, Y)), field(Name, object(X, Y))) :-
    % Se il valore è già un'istanza di oggetto, mantieni la stessa forma
    is_instance(X, Y),
    !.
process_field(field(Name, object(Type1, Value1)), field(Name, object(Type2, Value1))) :-
    % Se il valore è un'istanza di oggetto, verifica la sottotipizzazione e standardizza il tipo
    is_instance(object(Type1, Value1)),
    !,
    is_subtype_of(Type1, Type2).

standardize_methods([], []) :- !.
standardize_methods([Method | Rest], [StandardMethod | StandardRest]) :-
    process_method(Method, StandardMethod),
    standardize_methods(Rest, StandardRest).

process_method(method(Name, Args, Body), (StandardHead :- StandardBody)) :-
    StandardHead =.. [Name, This | Args],
    replace_this(This, Body, StandardBody).

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

%%% Data una lista di campi, eredita il loro tipo da un'altra lista di campi.
inherit_field_type([], [], []) :- !.
inherit_field_type(Fields, [], Fields) :-
    Fields \= [],
    !.
inherit_field_type([], InheritedFields, []) :-
    InheritedFields \= [],
    !.
inherit_field_type([Field1 | Rest1], [Field2 | Rest2], [ResultField | RestResult]) :-
    process_field_type(Field1, Field2, ResultField),
    inherit_field_type(Rest1, Rest2, RestResult).

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

make(Name, Type) :-
    (   \+ point_to(Name, _),
        ground(Name),
        new(Type, Obj),
        asserta(point_to(Name, Obj)),
        !
    ;   var(Name),
        new(Type, Obj),
        Name = Obj,
        !
    ;   new(Type, Obj),
        Name = Obj
    ).

make(Name, Type, Params) :-
    (   \+ point_to(Name, _),
        ground(Name),
        new(Type, Params, Obj),
        asserta(point_to(Name, Obj)),
        !
    ;   var(Name),
        new(Type, Params, Obj),
        Name = Obj,
        !
    ;   new(Type, Params, Obj),
        Name = Obj
    ).

%%% Eredita, utilizzando un'ereditarietà depth-first left-most,
%%% tutti i campi e i metodi dalle classi specificate.
inherit([], [], []) :- !.
inherit([Parent | Rest], Fields, Methods) :-
    !,
    class(Parent, Grandpas, ParentFields, ParentMethods),
    inherit(Grandpas, GrandpasFields, GrandpasMethods),
    helper_append_fields(ParentFields, GrandpasFields, TmpFields),
    helper_append_methods(ParentMethods, GrandpasMethods, TmpMethods),
    inherit(Rest, RestFields, RestMethods),
    helper_append_fields(TmpFields, RestFields, Fields),
    helper_append_methods(TmpMethods, RestMethods, Methods).

%%% Appende due liste di campi. Nel caso sia presente un campo con lo stesso
%%% nome sia nella prima lista che nella seconda, il campo della seconda lista
%%% viene scartato.
helper_append_fields([], [], []).

helper_append_fields([field(Name, Value) | Rest1],
                     [field(Name, _) | Rest2],
                     [field(Name, Value) | Result]) :-
    helper_append_fields(Rest1, Rest2, Result).
helper_append_fields(X, [], X) :- X \= [].

helper_append_fields([], X, X) :- X \= [].
helper_append_fields([field(Name1, Value1) | Rest1],
                     [field(Name2, Value2) | Rest2],
                     Result) :-
    (   Name1 \= Name2
    ),
    helper_append_fields_aux(Name1, Value1, Rest1, [field(Name2, Value2) | Rest2], Result).

helper_append_fields_aux(Name1, Value1, Rest1, Rest2, Result) :-
    helper_append_fields(Rest1, [field(Name1, Value1)], Tmp1),
    helper_append_fields(Tmp1, Rest2, Result).

%%% Appende due liste di metodi. Nel caso sia presente un metodo con lo stesso
%%% nome sia nella prima lista che nella seconda, il metodo della seconda
%%% lista viene scartato.
helper_append_methods([], [], []).
helper_append_methods([Method1 | Rest1], [], Result) :-
    append_method_to_list(Rest1, [Method1], Result).
helper_append_methods([], [Method2 | Rest2], Result) :-
    append_method_to_list(Rest2, [Method2], Result).

helper_append_methods([(Head1 :- Body1) | Rest1],
                      [(Head2 :- _) | Rest2],
                      Result) :-
    extract_method_name(Head1, MethodName1),
    extract_method_name(Head2, MethodName2),
    (
        MethodName1 = MethodName2,
        helper_append_methods(Rest1, Rest2, Result)
    ;
        MethodName1 \= MethodName2,
        append_method_to_list(Rest1, [(Head2 :- _) | Rest2], Tmp1),
        helper_append_methods([(Head1 :- Body1) | Tmp1], Rest2, Result)
    ).

extract_method_name(Method, MethodName) :-
    Method =.. [MethodName | _].

append_method_to_list(List, Method, Result) :-
    (
        List \= [],
        append(Method, List, Result)
    ;
        List = [],
        Result = Method
    ).

%%% Crea un nuovo oggetto del tipo specificato, inizializzato a un valore
%%% di default.
new(void, object(void, null)) :- !.
new(bool, object(bool, false)) :- !.
new(number, object(number, 0)) :- !.
new(float, object(float, 0.0)) :- !.
new(rational, object(rational, 0)) :- !.
new(atom, object(atom, '')) :- !.
new(string, object(string, "")) :- !.
new(integer, object(integer, 0)) :- !.
new(Type, object(Type, DefaultFields)) :-
    is_class(Type),
    !,
    inherit([Type], DefaultFields, _).

%%% Crea un nuovo oggetto del tipo specificato, con valore uguale a quello
%%% specificato (se accettabile).
new(void, X, object(void, X)) :- !.
new(bool, true, object(bool, true)) :- !.
new(bool, false, object(bool, false)) :- !.
new(number, X, object(number, X)) :- !, number(X).
new(float, X, object(float, X)) :- !, float(X).
new(rational, X, object(rational, X)) :- !, rational(X).
new(atom, X, object(atom, X)) :- !, atom(X).
new(string, X, object(string, X)) :- string(X), !.
new(string, X, object(string, X)) :- atom(X), !.
new(integer, X, object(integer, X)) :- !, integer(X).
new(Type, Params, object(Type, Fields)) :-
    is_class(Type),
    inherit([Type], DefaultFields, _),
    init_fields(DefaultFields, Params, Fields).

% Caso base
init_fields(Fields, [], Fields) :- !.

%%% Inizializza i campi con i valori passati come parametro.
init_fields([Field | FieldsRest], [Name = NewValue | ParamsRest], [FieldWithName | ResultRest]) :-
    Field = field(FieldName, object(Type, _)),
    FieldWithName = field(FieldName, NewObj),
    !,
    new(Type, NewValue, NewObj),
    init_fields(FieldsRest, ParamsRest, ResultRest).

% Inizializza i campi dati con i valori specificati.
init_fields([field(FieldName1, Obj) | FieldsRest], [FieldName2 = NewValue | ParamsRest], Result) :-
    FieldName1 \= FieldName2,
    !,
    init_fields(FieldsRest, [FieldName2 = NewValue | ParamsRest], Tmp),
    Result = [field(FieldName1, Obj) | Tmp].




%%% Estrae il valore del campo di un'istanza. Se il campo è di tipo
%%% built-in verrà ritornato il suo valore, se invece è del tipo di una
%%% classe verrà ritornata l'istanza.
field(Ptr, Name, Value) :-
    inst(Ptr, Obj),
    field(Obj, Name, Value).

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

fieldx_helper(Obj, [FieldName], Result) :-
    field(Obj, FieldName, Result).

fieldx_helper(Obj, [FieldName | Rest], Result) :-
    field(Obj, FieldName, Tmp),
    fieldx_helper(Tmp, Rest, Result).

%%% Verifica se è stata definita una classe con un certo nome.
is_class(Name) :-
    class(Name, _, _, _),
    ground(Name).

%%% Dato un nome, ritorna l'istanza con quel nome.
inst(Ptr, Obj) :- point_to(Ptr, Obj).

%%% Verifica se l'oggetto è istanza di un certo tipo.
is_instance(object(Type1, Value), Type2) :-
    is_subtype_of(Type1, Type2),
    is_instance(object(Type1, Value)).

%%% Verifica se l'istanza è valida.
is_instance(object(void, _)) :- !.
is_instance(object(number, X)) :- !, number(X).
is_instance(object(rational, X)) :- !, rational(X).
is_instance(object(atom, X)) :- !, atom(X).
is_instance(object(bool, true)) :- !.
is_instance(object(bool, false)) :- !.
is_instance(object(float, X)) :- !, float(X).
is_instance(object(string, X)) :- string(X), !.
is_instance(object(string, X)) :- atom(X), !.
is_instance(object(integer, X)) :- !, integer(X).
is_instance(object(Type, Fields)) :-
    \+ is_builtin(Type),
    !,
    class(Type, _, _, _),
    inherit([Type], ClassFields, _),
    forall(member(field(Name, _), Fields),
           member(field(Name, _), ClassFields)).

%%% Verifica se un tipo è sottotipo di un altro.
is_subtype_of(X, X) :- !.
is_subtype_of(float, number) :- !.
is_subtype_of(rational, float) :- !.
is_subtype_of(integer, rational) :- !.
is_subtype_of(string, atom) :- !.
is_subtype_of(Class, ParentClass) :-
    class(Class, Parents, _, _),
    !,
    helper_is_subtype_of(Parents, ParentClass).

%%% Helper usato per scorrere le liste di parent.
helper_is_subtype_of([X | _], Class) :-
    is_subtype_of(X, Class),
    !.
helper_is_subtype_of([X | Xs], Class) :-
    \+ is_subtype_of(X, Class),
    !,
    helper_is_subtype_of(Xs, Class).

%%% Verifica se un tipo è built-in
is_builtin(void).
is_builtin(bool).
is_builtin(atom).
is_builtin(string).
is_builtin(number).
is_builtin(float).
is_builtin(rational).
is_builtin(integer).

% Predicato principale per l'assert dei trampolini.
assert_trampolines([]).

assert_trampolines([Trampoline | Rest]) :-
    assert_trampoline(Trampoline),
    assert_trampolines(Rest).

% Predicato ausiliario per l'assert di un singolo trampolino.
assert_trampoline(Head :- _) :-
    extract_method_info(Head, Name, Arity, Args),
    abolish(Name, Arity),
    asserta((Head :- invoke_method(Name, Args))).

% Estrae informazioni da un predicato di trampolino.
extract_method_info(Head, Name, Arity, Args) :-
    Head =.. [Name | Args],
    functor(Head, Name, Arity).

% Esegue il metodo di un'istanza.
invoke_method(Name, [Ptr | Args]) :-
    inst(Ptr, Instance),
    !,
    invoke_method(Name, [Instance | Args]).
invoke_method(Name, [object(Type, Value) | Args]) :-
    CalledHead =.. [Name | [object(Type, Value) | Args]],
    get_best_method([Type], Name, (Head :- Body)),
    asserta((Head :- Body)),
    !,
    call(CalledHead), !,      
    retract(Head :- Body), !.

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





get_method_from_methods_list(MethodName, [(Head :- Body) | _], (Head :- Body)) :-
    Head =.. [MethodName | _].
get_method_from_methods_list(MethodName, [(Head :- _) | Rest], Result) :-
    Head =.. [Functor | _],
    Functor \= MethodName,
    !,
    get_method_from_methods_list(MethodName, Rest, Result).
