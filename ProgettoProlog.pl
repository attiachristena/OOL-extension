% Definizione delle classi e delle istanze
:- dynamic class/3, instance/3, field/2.


% Predicato per definire una classe
def_class(ClassName, Parents) :-
    atom(ClassName),
   % atom(Parents),
    is_list(Parents),
   % has_parents(ClassName, Parents),
    atom_list(Parents),
    assertz(class(ClassName, Parents, [])).

def_class(ClassName, Parents, Parts) :-
    atom(ClassName),
    is_list(Parents),
    maplist(is_class, Parents),
    atom_list(Parents),
    is_list(Parts),
    valid_parts(Parts),
    assertz(class(ClassName, Parents, Parts)).

% Verifica se una classe ha i genitori
has_parents(ClassName, Parents) :-
     class(ClassName, ActualParents, _),
    subset(Parents, ActualParents).

% Verifica che tutti gli elementi di una lista siano atomi.
atom_list([]).
atom_list([H | T]) :-
   atom(H),
   atom_list(T).

valid_parts(_).
valid_parts([Field|Rest]) :-
    valid_part(Field),
    valid_parts(Rest).

valid_part(field(FieldName, Value)) :-
    atom(FieldName),
    assertz(field(FieldName, Value)).

valid_part(field(FieldName, Value, Type)) :-
    atom(FieldName),
    var(Type),
    assertz(field(FieldName, Value, Type)).
    
           
           

% Predicato per definire un campo di una classe
fields(Class, FieldName, Value) :-
    assertz(field(Class, FieldName, Value, _Type)).

fields(Class, FieldName, Value, Type) :-
    assertz(field(Class, FieldName, Value, Type)).

% Predicato per definire una istanza di una classe
make(Instance, ClassName, []) :-
    % Associa o unifica il nome della istanza con un nuovo termine vuoto
    (atom(Instance) ; var(Instance)), !,
    assertz(instance(Instance, ClassName, [])).

make(Instance, ClassName, [Field=FieldValue | Rest]) :-
    % Associa o unifica il nome della istanza con un nuovo termine che rappresenta la istanza
    (atom(Instance) ; var(Instance)), !,
    assertz(instance(Instance, ClassName, [Field=FieldValue | Rest])),
    make_fields(Instance, [Field=FieldValue | Rest]).


% Predicato ausiliario per gestire i campi della istanza
make_fields(_, []).
make_fields(Instance, [Field=FieldValue | Rest]) :-
    % Associa o unifica i campi della istanza con i valori forniti
    assertz(field(Instance, Field, FieldValue, _)),
    make_fields(Instance, Rest).

% Predicato per verificare se un atomo è il nome di una classe
is_class(Class) :-
    atom(Class),
    class(Class, _, _).

% Predicato per verificare se un termine è un'istanza di una classe
is_instance(Value) :-
    nonvar(Value),
    instance(Value, _, _).

% Predicato per verificare se un termine è un'istanza di una classe specifica
is_instance(Value, ClassName) :-
    nonvar(Value),
    atom(ClassName),
    instance(Value, Class, _),
    is_subclass(Class, ClassName).

% Predicato ausiliario per verificare la gerarchia delle classi
is_subclass(ClassName, ClassName).
is_subclass(ClassName, SuperClassName) :-
    class(ClassName, Parents, _),
    member(Parent, Parents),
    is_subclass(Parent, SuperClassName).

% Predicato per recuperare un'istanza dato il nome
inst(InstanceName, Instance) :-
    atom(InstanceName),
    instance(InstanceName, _, Fields),
    build_instance(Fields, Instance).

% Predicato ausiliario per costruire l'istanza a partire dai campi
build_instance([], []).
build_instance([Field=FieldValue | RestFields], [Field=FieldValue | RestInstance]) :-
    build_instance(RestFields, RestInstance).

% Predicato per estrarre il valore di un campo da una classe
field(Instance, FieldName, Result) :-
    atom(Instance),
    atom(FieldName),
    instance(Instance, Class, _),
    get_field_value(Class, FieldName, Result).

% Predicato ausiliario per ottenere il valore di un campo dalla classe o dai suoi antenati
get_field_value(Class, FieldName, Result) :-
    (field(Class, FieldName, Result, _Type) ;   % Campo presente nella classe corrente
     class(Class, Parents, _),                   % Ricerca ricorsiva nelle superclassi
     member(Parent, Parents),
     get_field_value(Parent, FieldName, Result)).

% Predicato per estrarre il valore da una classe percorrendo una catena di attributi
fieldx(Instance, FieldNames, Result) :-
    atom(Instance),
    is_list(FieldNames),
    get_nested_field_value(Instance, FieldNames, Result).

% Predicato ausiliario per ottenere il valore di un campo da una sequenza di attributi
get_nested_field_value(Instance, [FieldName], Result) :-
    field(Instance, FieldName, Result, _).

get_nested_field_value(Instance, [FirstFieldName | RestFieldNames], Result) :-
    field(Instance, FirstFieldName, NextInstance, _),
    get_nested_field_value(NextInstance, RestFieldNames, Result).
