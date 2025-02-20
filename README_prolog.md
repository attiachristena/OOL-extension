# ReadMe

## Cognome, Nome, Matricola: Attia Christena 894887

### Definizione e Gestione delle Classi:
1. **Definizione di Classe**: `def_class/2` e `def_class/3`
- Crea una nuova classe con nome, genitori (e parti).
```
def_class(MyClass, [Parent1, Parent2], [field(myField), method(myMethod,
[Arg])]).
```

2. **Validazione del Genitore**: `validate_parent_class/1`
- Verifica che un genitore sia una classe esistente.
```
validate_parent_class(Parent).
```

3. **Estrazione di Campi e Metodi**: `extract_parts/3`
- Separa una lista di termini in campi e metodi.
```
extract_parts([field(name), method(print, [])], Fields, Methods).
```

4. **Inizializzazione di Campi e Metodi**: `initialize_fields/2` e
`initialize_methods/2`
- Inizializza campi e metodi di una classe.
```
initialize_fields([field(myField)], StandardFields).
```

5. **Processamento di Campi e Metodi**: `process_field/2` e `process_method/2`
- Processa campi e metodi standardizzandoli.
```
process_field(field(name, value), StandardField).
```

6. **Gestione del Tipo Ereditato**: `get_type_field/3`
- Ottiene il tipo ereditato dei campi.
```
get_type_field([field(name, object(void, value))], [], Result).
```

### Creazione e Gestione degli Oggetti:
7. **Creazione di Oggetti**: `make/2` e `make/3`
- Crea oggetti senza o con parametri. I vari casi di InstanceName sono gestiti da `create`
```
make(myObj, MyClass).
```

8. **Creazione di Oggetti Complessi**: `create/3` e `create/2`
- Crea oggetti con valori specificati o predefiniti.
```
create(MyClass, [param1, param2], MyObject).
```

9. **Inizializzazione di Campi di Oggetti**: `init_fields/3` e `init_field/3`
- Inizializza campi di un oggetto.
```
init_fields([field(name)], [name = 'John'], InitializedFields).
```

### Gestione dei Metodi e Invocazione:
10. **Ottieni Valore di un Campo**: `field/3` e `fieldx/3`
- Ottiene il valore di un campo da un oggetto o un'istanza.
```
field(myObject, name, Value).
```

11. **Verifica se è una Classe o un'Istanza**: `is_class/1` e `inst/2`
- Verifica se un termine è una classe definita o un'istanza.
```
is_class(MyClass).
```

12. **Verifica se è un'Istanza di un Tipo**: `is_instance_of/2` e
`is_instance/1`
- Verifica se un oggetto è un'istanza di un certo tipo.
```
is_instance_of(myObject, MyClass).
```

13. **Verifica la Sottoclasse**: `is_subtype_of/2` e `base_subtype/2`
- Verifica la relazione di sottotipo tra classi.
```
is_subtype_of(MyClass, ParentClass).
```

14. **Verifica il Sottoinsieme di Liste**: `subset/2`
- Verifica se una lista è un sottoinsieme di un'altra.
```
subset([X, Y], [Y, Z, X]).
```

15. **Gestione dei Trampolini per l'Invocazione di Metodi**:
`record_trampolines/1`, `record_trampoline/1`,
`extract_and_record_trampoline/1`
- Gestisce i trampolini per l'invocazione di metodi.
```
record_trampolines([method(myMethod, [Arg])]).
```

16. **Ricerca ed Esecuzione del Miglior Metodo**: `find_and_execute_method/3`
e `find_best_method/3`
- Trova ed esegue il miglior metodo da una lista di classi.
```
find_and_execute_method([MyClass], myMethod, CalledHead).
```

Nota: Per scrivere la struttura dei metodi ho usato degli esempi per facilitarne la
comprensione
