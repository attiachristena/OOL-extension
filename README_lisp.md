# Readme

## Cognome, Nome, Matricola: Attia Christena 894887

### Tabella hash per memorizzare le specifiche delle classi

Il codice implementa una semplice gestione di classi e
oggetti in Common Lisp, utilizzando una tabella hash (`class-specs`)
per memorizzare le specifiche delle classi.
Le principali funzionalità del codice sono:

### Metodi della Tabella Hash

1. **`add-class-spec`**
- Aggiunge una specifica di classe alla tabella hash.

2. **`class-spec`**
- Restituisce la specifica di classe per il nome dato.

3. **`class-spec-parents`**
- Restituisce la lista dei genitori della classe data.

4. **`class-spec-fields`**
- Restituisce i campi della classe data.

5. **`class-spec-methods`**
- Restituisce i metodi della classe data.

### Creazione e Gestione delle Classi

6. **`def-class`** 
- Definisce una nuova classe con nome, genitori e parti.
Verifica la correttezza delle classi genitore e aggiunge la specifica
alla tabella hash. Evita il controllo is-class in modo tale da poter
definire, se necessario, un'altra classe con lo stesso nome.
### Creazione e Gestione degli Oggetti

7. **`make`**
- Crea un'istanza di classe con il nome e gli argomenti specificati.

### Validazione e Controllo

8. **`check-class-name`**
- Controlla se il nome della classe è già in uso.

9. **`check-parents`**
- Controlla se tutte le classi genitore sono definite.

10. **`is-class`**
- Verifica se la classe è definita.

11. **`is-instance`**
- Verifica se l'oggetto è un'istanza della classe specificata.

### Gestione dell'Eredità e Campi

12. **`get-field-type-parents`**
- Eredità i campi dalle classi genitore.

13. **`define-list-field`**
- Unisce due liste di campi, evitando duplicati.

14. **`get-fields`**
- Estrae la lista dei campi dalle parti della classe.

15. **`helper-rewrite-field`**
- Esegue la validazione dei campi in modo ricorsivo.

16. **`rewrite-field`**
- Esegue la validazione di un campo.

17. **`field`**
- Restituisce il valore di un campo specifico.

18. **`field*`**
- Restituisce i valori di uno o più campi specifici.

19. **`get_field_type`**
- Restituisce la lista di campi con i relativi tipi.

### Gestione dei Metodi

20. **`process-method`**
- Elabora un metodo e lo associa al nome specificato.

21. **`rewrite-method-code`**
- Riscrive il codice del metodo come una funzione lambda.

22. **`invoke-method`**
- Invoca un metodo sull'istanza, tenendo conto dell'ereditarietà.

23. **`upgrade-field`**
- Aggiorna i campi della classe con i nuovi valori.

24. **`upgrade-field-helper`**
- Aggiorna un campo specifico nella lista dei campi.

25. **`get-methods`**
- Estrae la lista dei metodi dalle parti della classe.

26. **`get-methods-helper`**
- Estrae i metodi dalle parti della classe in modo ricorsivo.

27. **`get-field-type`**
- Restituisce il tipo del campo specificato.

### Verifica delle Relazioni tra Classi

28. **`is-subtype-of`**
- Verifica se una classe è un sottotipo di un'altra.

29. **`inherit-class`**
- Verifica se una classe è ereditata da un'altra.

30. **`parents-method`**
- Ottiene il metodo da una delle classi genitore.

31. **`has-method-in-class-spec`**
- Verifica se una classe ha un certo metodo.

32. **`extract-method`**
- Estrae il corpo del metodo dalla lista dei metodi.

