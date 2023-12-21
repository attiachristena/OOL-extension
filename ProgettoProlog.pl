%%% -*- Mode: Prolog -*-
%%% oop.pl
%%% Attia Christena Mat 894887

% Predicati dinamici per definire le classi.
:- dynamic class/2.
:- dynamic class/3.
:- dynamic field/4,
   method/4,
   instance/2.




% Predicato def_class per definire la struttura di una classe in OOΠ.

% def_class/2: definizione della classe senza parti (field o method).
def_class(ClassName, Parents) :-
   atom(ClassName), 
   is_list(Parents),
% Verifica che tutti gli elementi di Parents siano atomi.
   atom_list(Parents),
% Memorizza la classe nella base di conoscenza di Prolog
   assertz(class(ClassName, Parents)).
%%%%%%%%%%%%%%%%%%findall e bagof

% def_class/3: definizione della classe con parti specificate (field o method).
def_class(ClassName, Parents, Parts) :-
   atom(ClassName),
   is_list(Parents),
% Verifica che tutti gli elementi di Parents siano atomi.
   atom_list(Parents),
% Si assicura che Parts sia una lista valida di parti (field o method).
   valid_parts(Parts),
% Memorizza la classe nella base di conoscenza di Prolog.
   assertz(class(ClassName, Parents, Parts)).

% Verifica che tutti gli elementi di una lista siano atomi.
atom_list([]).
atom_list([H | T]) :-
   atom(H),
   all_atoms(T).

   
valid_parts([]).
valid_parts([Part|Rest]) :-
    (valid_field(Part)
    ;
     valid_method(Part)),
    valid_parts(Rest).

% valid_field/2 :validazione di un campo con due argomenti
valid_field(field(FieldName, Value)) :-
    atom(FieldName),
    ground(Value), % Verifica se il valore è istanziato
    !. % Utilizzo del cut per evitare backtracking indesiderato

% valid_field/3: validazione di un campo con tre argomenti
valid_field(field(FieldName, Value, Type)) :-
    atom(FieldName),
    ground(Value), % Verifica se il valore è istanziato
    (var(Type)
    ;
    atom(Type)),
    !. % Utilizzo del cut per evitare backtracking indesiderato

% Se la struttura non è conforme ai due metodi precedenti, la regola fallisce.
valid_field(Field) :-
    format("Errore: Il campo ~w non è valido.~n", [Field]),
    fail.

% Validazione di un metodo
valid_method(method(MethodName, ArgList, Form)) :-
    atom(MethodName),
    is_prolog_list(ArgList),
    is_prolog_form(Form),
    !. % Utilizzo del cut per evitare backtracking indesiderato

% Se la struttura non è conforme al metodo precedente, la regola fallisce
valid_method(Method) :-
    format("Errore: Il metodo ~w non è valido.~n", [Method]),
    fail.

% Verifica se un termine è una congiunzione di predicati Prolog
is_prolog_form(Form) :-
    is_list(Form),
    maplist(prolog_predicate, Form).

% Verifica se un termine è una lista standard Prolog
is_prolog_list(List) :-
    is_list(List),
    maplist(ground, List). 

% Verifica se un termine è un predicato Prolog
prolog_predicate(Predicate) :-
    functor(Predicate, Functor, _),
    atom(Functor).

% make/2: Creazione di una nuova istanza di una classe
make(InstanceName, ClassName) :-
    make_instance(InstanceName, ClassName, []).

% make/3: Creazione di una nuova istanza di una classe con campi specificati
make(InstanceName, ClassName, FieldList) :-
    make_instance(InstanceName, ClassName, FieldList).

% make_instance/3: Implementazione della creazione di un'istanza
make_instance(Instance, Class, FieldList) :-
    assertz(instance(Instance, Class)),
    process_field_list(Instance, FieldList).

% process_field_list/2: Processa la lista di campi e asserisce i fatti
process_field_list(_, []).
process_field_list(Instance, [Field=FieldValue|Rest]) :-
    assertz(field(Instance, Field, FieldValue, _Type)),
    process_field_list(Instance, Rest).

% is_class/1: Verifica se un atomo è il nome di una classe
is_class(Class) :-
    class(Class, _).

% is_instance/1: Verifica se un termine è un'istanza qualunque
is_instance(Value) :-
    instance(Value, _).

% is_instance/2: Verifica se un termine è un'istanza di una classe specifica
is_instance(Value, ClassName) :-
    atom(ClassName),
    instance(Instance, ClassName).

% inst/2: Recupera un'istanza dato il nome con cui è stata creata
inst(InstanceName, Instance) :-
    instance(InstanceName, Instance).

% field/3: Estrae il valore di un campo da una classe
field(Instance, FieldName, Result) :-
    field(Instance, FieldName, Result, _Type).

% fieldx/3: Estrae il valore da una classe percorrendo una catena di attributi
fieldx(Instance, FieldNames, Result) :-
    fieldx_recursive(Instance, FieldNames, Result).

% fieldx_recursive/3: Implementazione di fieldx
fieldx_recursive(Instance, [FieldName], Result) :-
    field(Instance, FieldName, Result, _Type).
fieldx_recursive(Instance, [FieldName|Rest], Result) :-
    field(Instance, FieldName, NextInstance, _Type),
    fieldx_recursive(NextInstance, Rest, Result).

    

















