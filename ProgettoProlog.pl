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

% Definizione della classe senza parti (field o method).
def_class(Class_Name, Parents) :-
   atom(Class_Name), 
   is_list(Parents),
% Verifica che tutti gli elementi di Parents siano atomi.
   all_atoms(Parents),
% Memorizza la classe nella base di conoscenza di Prolog
   assertz(class(Class_Name, Parents)).

% Definizione della classe con parti specificate (field o method).
def_class(Class_Name, Parents, Parts) :-
   atom(Class_Name),
   is_list(Parents),
% Verifica che tutti gli elementi di Parents siano atomi.
   all_atoms(Parents),
% Si assicura che Parts sia una lista valida di parti (field o method).
   valid_parts(Parts),
% Memorizza la classe nella base di conoscenza di Prolog.
   assertz(class(Class_Name, Parents, Parts)).

% Verifica che tutti gli elementi di una lista siano atomi.
all_atoms([]).
all_atoms([H | T]) :-
   atom(H),
   all_atoms(T)

% Controlla Parts e analizza il caso in cui sia un campo o un metodo.
valid_parts([]).
valid_parts([Part|Rest]) :-
    (valid_field(Part);
     valid_method(Part)),
    valid_parts(Rest).

% Validazione di un campo con due argomenti
valid_field(field(FieldName, Value)) :-
    atom(FieldName),
    ground(Value), % Verifica se il valore è istanziato
    !. % Utilizzo del cut per evitare backtracking indesiderato

% Validazione di un campo con tre argomenti
valid_field(field(FieldName, Value, Type)) :-
    atom(FieldName),
    ground(Value), % Verifica se il valore è istanziato
    (   var(Type)
    ;   atom(Type)
    ),
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

% Se la struttura non è conforme a method/3, la regola fallisce
valid_method(Method) :-
    format("Errore: Il metodo ~w non è valido.~n", [Method]),
    fail.

% Verifica se un termine è una congiunzione di predicati Prolog
is_prolog_form(Form) :-
    is_list(Form),
    maplist(prolog_predicate, Form).

% Verifica se un termine è un predicato Prolog
prolog_predicate(Predicate) :-
    functor(Predicate, Functor, _),
    atom(Functor).








