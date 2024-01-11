% Predicati dinamici 
:- dynamic class/2, class/3.
:- dynamic inst/1.


% Predicato per definire una classe
def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).


def_class(ClassName, Parents, Parts) :-
    atom(ClassName),
    is_list(Parts),
    is_list(Parents),
    maplist(is_class, Parents),
    atom_list(Parents),
    extract_parts(Parts, _, Methods),
    load_methods(Methods, ClassName),
    class_exists(ClassName);
    assert(class(ClassName, Parents, Parts)), !.

% Verifica che tutti gli elementi di una lista siano atomi.
atom_list([]).
atom_list([H | T]) :-
   atom(H),
   atom_list(T).



% Predicato per verificare se un atomo è il nome di una classe
is_class(Name) :-
    ground(Name),
    class(Name, _, _).



%%% Data una lista di metodi e campi, separa campi e metodi in due
%%% liste differenti
extract_parts([], [], []) :- !.
extract_parts([Field | Rest], [Field | FieldsRest], MethodsRest) :-
    functor(Field, field, _),
    !,
    extract_parts(Rest, FieldsRest, MethodsRest).

extract_parts([Method | Rest], FieldsRest, [Method | MethodsRest]) :-
    functor(Method, method, _),
    !,
    extract_parts(Rest, FieldsRest, MethodsRest).

class_exists(Class_Name) :-
    is_class(Class_Name),
    class([Class_Name, L, X]),
    retract(class([Class_Name, L, X])),
    (   retract(class([Class_Name, L, []]))
    ;   retract(class([Class_Name, [], []]))
    ).



load_methods([Method | Rest], Class_Name) :-
    make_method(Method, Class_Name),
    load_methods(Rest, Class_Name).

%%% Caso base
load_methods([], _). 


make_method(method(Method_Name, Args, Method_body), Class_Name) :-
    append([this], Args, Method_Args), 
    append([Method_Name], Method_Args, Rest),
    Term =.. Rest,
    term_to_atom(Method_Name, Name),
    term_to_atom(Term, MethodHead),
    term_to_atom(Method_body, BodyNoCheck),
    term_to_atom(Class_Name, Class),
    atom_concat('field(this, ', Name, NameChecked),
    atom_concat(NameChecked, ', S1),', This_field),
    atom_concat(This_field, 'field(', Field1_append),
    atom_concat(Field1_append, Class, Fiel2_append),
    atom_concat(Fiel2_append, ', ', Field3_append),
    atom_concat(Field3_append, Name, Field4_append),
    atom_concat(Field4_append, ', S2),', Class_field),
    % per controllare che le classi siano uguali
    atom_concat(Class_field, 'S1 = S2', Checker),
    atom_concat(Checker, ',', To_append),
    atom_concat(To_append, BodyNoCheck, Body),
    atom_concat(MethodHead, ' :- ', MethodNoBody),
    atom_concat(MethodNoBody, Body, MethodNoThisNoEnd),
    atom_concat(MethodNoThisNoEnd, ', !.', MethodNoThis),
    atom_string(MethodNoThis, MethodNoThisString),
    replace_word(MethodNoThisString, "this", "This", Method_Atom),
    term_to_atom(Method, Method_Atom),
    % controllo se il metodo esiste gia. Se esiste gia, lo sostituisco
    % check_method(Method_Name, Method_Args);
    assert(Method),
    write("Metodo creato con successo : "), write(MethodHead), nl.

replace_word(Parola, Vecchia, Nuova, X) :-
    replace_n_word(Parola, 1, Vecchia, Nuova, Result),
    replace_word(Result, Vecchia, Nuova, X), !.
replace_word(Parola, _, _, Parola).

% Funzione di appoggio per replace_word.
replace_n_word(Parola, Ennesima, Vecchia, Nuova, Result) :-
    call_nth(sub_atom(Parola, Before, _Len, After, Vecchia), Ennesima),
    sub_atom(Parola, 0, Before, _, Left), 
    sub_atom(Parola, _, After, 0, Right), 
    atomic_list_concat([Left, Nuova, Right], Result).
