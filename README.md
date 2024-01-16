# Progetto Prolog

## Introduzione

Questo progetto implementa un sistema di definizione e manipolazione di classi e oggetti in Prolog. Il codice si basa su concetti di programmazione ad oggetti, consentendo la creazione di classi, oggetti, l'accesso ai loro campi e l'invocazione dei loro metodi.

### Definizione delle Classi

Il progetto inizia definendo una classe di base chiamata "object", che ha una lista vuota di genitori, campi e metodi.

prolog class(object, [], [], []).

Viene quindi definito un puntatore nullo chiamato "nullptr".

point_to(ptr, nullptr).

### Dichiarazione Dinamica dei predicati

Vengono dichiarati ipredicati dinamici `class/4` e `point_to/2`, che verranno utilizzati per memorizzare le informazioni sulle classi e i puntatori.

:- dynamic class/4.
:- dynamic point_to/2.


### Definizione di Classi

Il predicato `def_class/2` permette la definizione di nuove classi senza specificare campi o metodi, mentre `def_class/3` consente la definizione di classi con campi e metodi specificati.

def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).

def_class(ClassName, Parents, Parts) :-

### Creazione di Oggetti

Il predicato `make/2` permette la creazione di oggetti senza parametri, mentre `make/3` consente la creazione di oggetti con parametri specificati.

make(Name, Type) :- 

make(Name, Type, Params) :- 

### Manipolazione di Campi e Metodi

Vengono forniti predicati per estrarre e inizializzare campi, manipolare i nomi dei metodi e invocare metodi specificati.

field(Ptr, Name, Value) :- 

initialize_fields([], []).

initialize_methods([], []).

call_method(Name, [Ptr | Args]) :- 

### Verifica del Tipo e Ereditarietà

Vengono forniti predicati per verificare se un oggetto è un'istanza di un certo tipo e per gestire la relazione di ereditarietà tra classi.

is_instance(object(Type, Value), Type) :- 

is_subtype_of(X, X) :-

## Utilizzo

Il progetto può essere utilizzato per definire e manipolare classi e oggetti in Prolog, con la possibilità di ereditare campi e metodi, creare nuovi oggetti e invocare i metodi delle classi. Si consiglia di consultare il codice sorgente per una comprensione più dettagliata e per vedere esempi di utilizzo.

## Note

Il codice è stato organizzato in modo modulare, consentendo una facile estensione e personalizzazione. Le definizioni delle classi, i metodi e i campi possono essere adattati per soddisfare specifici requisiti di progetto.
