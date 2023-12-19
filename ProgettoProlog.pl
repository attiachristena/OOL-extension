%%% -*- Mode: Prolog -*-
%%%oop.pl
%%%Attia Christena Mat 894887

% Predicati dinamici per definire le classi.
:- dynamic class/2.
:- dynamic class/3.
:- dynamic field/4,
   method/4,
   instance/2.




% Predicato def_class per definire la struttura di una classe in OOΠ.

% Definizione della classe senza parti (field o method).
def_class(Class, Parents) :-
atom(Class), 
is_list(Parents),
% Verifica che tutti gli elementi di Parents siano atomi.
all_atoms(Parents),
% Memorizza la classe nella base di conoscenza di Prolog
assertz(class(Class, Parents)).

% Definizione della classe con parti specificate (field o method).
def_class(Class, Parents, Parts) :-
atom(Class), 
is_list(Parents), 
% Verifica che tutti gli elementi di Parents siano atomi.
all_atoms(Parents),
% Si assicura che Parts sia una lista valida di parti (field o method).
process_parts(Class, Parts),
% Memorizza la classe nella base di conoscenza di Prolog con le parti specificate.
assertz(class(Class, Parents, Parts)).


% process_parts: Processa le parti di una classe (fields o methods)
process_parts(_, []).
process_parts(Class, [Part|Rest]) :-
    process_part(Class, Part),
    process_parts(Class, Rest).

% process_part: Processa una singola parte di una classe (field o method)
process_part(Class, field(FieldName, Value)) :-
    assertz(field(Class, FieldName, Value, _Type)).  % Assume un tipo non specificato per il campo.
process_part(Class, field(FieldName, Value, Type)) :-
    assertz(field(Class, FieldName, Value, Type)).
process_part(Class, method(MethodName, ArgList, Form)) :-
    assertz(method(Class, MethodName, ArgList, Form)).


% Verifica che tutti gli elementi di una lista siano atomi.
all_atoms([]).
all_atoms([H | T]) :-
   atom(H),
   all_atoms(T)






