
class(object, [], [], []).
point_to(ptr, nullptr).
:- dynamic class/4.
:- dynamic point_to/2.

%%% Definisce una nuova classe e la inserisce nella base di conoscenza
def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).
def_class(ClassName, Parents, Parts) :-
    \+ class(ClassName, _, _, _),
    forall(member(Parent, Parents), class(Parent, _, _, _)),
    get_methods_and_fields(Parts, TmpFields, TmpMethods),
    standardize_fields(TmpFields, StandardFields),
    standardize_methods(TmpMethods, StandardMethods),
    inherit(Parents, InheritedFields, _),
    inherit_field_type(StandardFields, InheritedFields, FinalFields),
    assert_trampolines(StandardMethods),
    asserta(class(ClassName, Parents, FinalFields, StandardMethods)).

%%% Data una lista di metodi e campi, separa campi e metodi in due
%%% liste differenti
get_methods_and_fields([], [], []).
get_methods_and_fields([Term | Rest], FieldsRest, MethodsRest) :-
    (   Term =.. [field | _],
        FieldsRest = [Term | Fields],
        get_methods_and_fields(Rest, Fields, MethodsRest)
    ;   Term =.. [method | _],
        MethodsRest = [Term | Methods],
        get_methods_and_fields(Rest, FieldsRest, Methods)
    ;   get_methods_and_fields(Rest, FieldsRest, MethodsRest)
    ).


%%% Riscrive i campi contenuti in una lista in una forma standard
%%% scelta in fase di implementazione, ovvero field(Nome, Oggetto)
standardize_fields([], []).
standardize_fields([Field | Rest], [StandardField | StandardRest]) :-
    process_field(Field, StandardField),
    standardize_fields(Rest, StandardRest).

process_field(field(Name, Value), field(Name, Obj)) :-
    \+ is_instance(Value),
    !,
    new(void, Value, Obj).
process_field(field(Name, Value, Type), field(Name, Obj)) :-
    \+ is_instance(Value),
    !,
    new(Type, Value, Obj).
process_field(field(Name, object(X, Y)), field(Name, object(X, Y))) :-
    is_instance(X, Y),
    !.
process_field(field(Name, object(Type1, Value), Type2), field(Name, object(Type2, Value))) :-
    is_instance(object(Type1, Value)),
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
replace_this(This, OldAtom, OldAtom) :-
    atomic(OldAtom),
    OldAtom \= this,
    !.
replace_this(This, this, This).


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
    (   ground(Name),
        \+ point_to(Name, _),
        new(Type, Obj),
        asserta(point_to(Name, Obj))
    ;   var(Name),
        new(Type, Obj),
        Name = Obj
    ;   new(Type, Obj),
        Name = Obj
    ).

make(Name, Type, Params) :-
    (   ground(Name),
        \+ point_to(Name, _),
        new(Type, Params, Obj),
        asserta(point_to(Name, Obj))
    ;   var(Name),
        new(Type, Params, Obj),
        Name = Obj
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




init_fields(DefaultFields, [], DefaultFields) :- !.

init_fields([Field | FieldsRest], [FieldName = NewValue | ParamsRest], [NewField | ResultRest]) :-
    process_fields(Field, FieldName, NewValue, NewField),
    init_fields(FieldsRest, ParamsRest, ResultRest).

init_fields([Field | FieldsRest], [FieldName2 = NewValue | ParamsRest], Result) :-
    Field \= field(FieldName2, _),
    init_fields(FieldsRest, [FieldName2 = NewValue], Tmp),
    init_fields([Field | Tmp], ParamsRest, Result).

process_fields(field(FieldName, object(Type, _)), FieldName, NewValue, field(FieldName, NewObj)) :-
    new(Type, NewValue, NewObj).
process_fields(Field, _, _, Field).





field(Ptr, Name, Value) :-
    inst(Ptr, Obj),
    field_helper(Obj, Name, Value).

field_helper(object(_, Fields), Name, Value) :-
    member(field(Name, FieldValue), Fields),
    !,
    (   is_class_instance(FieldValue, object(Class, Value)),
        is_class(Class)
    ;   is_builtin_instance(FieldValue, Type, Value),
        is_builtin(Type)
    ;   is_builtin(FieldValue),
        Value = FieldValue
    ).

field_helper(object(_, [field(Name, Field) | _]), Name, Field) :-
    !.
field_helper(object(_, [field(FieldName, _) | FieldsRest]), Name, Value) :-
    FieldName \= Name,
    !,
    field_helper(object(_, FieldsRest), Name, Value).

is_class_instance(Term, Instance) :-
    Term =.. [object, Class | _],
    is_class(Class),
    Instance = Term.

is_builtin_instance(Term, Type, Value) :-
    Term =.. [object, Type, Value],
    is_builtin(Type).


fieldx(InstanceName, FieldNames, Result) :-
    inst(InstanceName, Obj),
    \+ InstanceName = object(_, _),
    !,
    fieldx(Obj, FieldNames, Result).

fieldx(Object, [FieldName], Result) :-
    field(Object, FieldName, Result).

fieldx(Object, [FieldName | Rest], Result) :-
    field(Object, FieldName, Tmp),
    fieldx(Tmp, Rest, Result).





%%% Verifica se è stata definita una classe con un certo nome.
is_class(Name) :-
    ground(Name),
    class(Name, _, _, _).

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

%%% Aggiunge alla base di conoscenza un predicato trampolino per
%%% richiamare correttamente i metodi di una classe.
assert_trampolines([]).
assert_trampolines([(Head :- _) | Rest]) :-
    Head =.. [Name | Args],
    functor(Head, Name,  Arity),
    abolish(Name, Arity),
    asserta((Head :- invoke_method(Name, Args))),
    assert_trampolines(Rest).

%%% Esegue il metodo di un'istanza.
invoke_method(Name, [Ptr | Args]) :-
    inst(Ptr, Instance),
    !,
    invoke_method(Name, [Instance | Args]).
invoke_method(Name, [object(Type, Value) | Args]) :-
    get_best_method([Type], Name, (Head :- Body)),
    asserta((Head :- Body)),
    CalledHead =.. [Name | [object(Type, Value) | Args]],
    !,
    call(CalledHead), !,      %%% questo è un red cut
    retract(Head :- Body), !.

%%% Sceglie il migliore metodo da eseguire data una gerarchia di classi.
%%% Per ereditare i metodi viene utilizzata un'ereditarietà depth-first
%%% left-most.
get_best_method([Class], MethodName, BestMethod) :-
    class(Class, _, _, MethodsList),
    get_method_from_methods_list(MethodName, MethodsList, BestMethod),
    !.
get_best_method([Class], MethodName, BestMethod) :-
    class(Class, Parents, _,  MethodsList),
    \+ get_method_from_methods_list(MethodName, MethodsList, _),
    !,
    get_best_method(Parents, MethodName, BestMethod).
get_best_method([Class | Classes], MethodName, BestMethod) :-
    Classes \= [],
    get_best_method([Class], MethodName, BestMethod),
    !.
get_best_method([Class | Classes], MethodName, BestMethod) :-
    Classes \= [],
    \+ get_best_method([Class], MethodName, _),
    class(Class, Parents, _, _),
    get_best_method(Parents, MethodName, BestMethod),
    !.
get_best_method([Class | Classes], MethodName, BestMethod) :-
    Classes \= [],
    \+ get_best_method([Class], MethodName, _),
    class(Class, Parents, _, _),
    \+ get_best_method(Parents, MethodName, _),
    get_best_method(Classes, MethodName, BestMethod),
    !.

%%% Ottiene il metodo di nome specificato da una lista di metodi.
get_method_from_methods_list(MethodName,
			     [(Head :- Body) | _],
			     (Head :- Body)) :-
    Head =.. [MethodName | _].
get_method_from_methods_list(MethodName,
			     [(Head :- _) | Rest],
			     Result) :-
    Head =.. [Functor | _],
    Functor \= MethodName,
    !,
    get_method_from_methods_list(MethodName, Rest, Result).
    
