;;; Attia Christena 894887

;;; Tabella hash per memorizzare le specifiche delle classi
(defparameter class-specs (make-hash-table))

;;; Aggiunge una specifica di classe alla tabella hash
(defun add-class-spec (class class-spec)
  (setf (gethash class class-specs) class-spec))

;;; Restituisce la specifica di classe per il nome dato
(defun class-spec (name)
  (gethash name class-specs))

;;; Restituisce la lista dei genitori della classe data
(defun class-spec-parents (name)
  (let ((class-spec (gethash name class-specs)))
    (if class-spec (car class-spec) nil)))

;;; Restituisce i campi della classe data
(defun class-spec-fields (name)
  (let ((class-spec (gethash name class-specs)))
    (if class-spec (cadr class-spec) nil)))

;;; Restituisce i metodi della classe data
(defun class-spec-methods (name)
  (let ((class-spec (gethash name class-specs)))
    (if class-spec (caddr class-spec) nil)))

;;; Definisce una nuova classe con nome, genitori e parti e
;;; aggiunge la specifica alla tabella hash
(defun def-class (class-name parents-list &rest parts)
  (cond ((not (check-parents parents-list)))
	(t (progn
             (add-class-spec
              class-name
              (list parents-list
                    (get_field_type
                     (get-fields parts)
                     (get-field-type-parents parents-list))
                    (get-methods parts)))
             class-name))))

;;; Crea un'istanza di classe con il nome e gli argomenti specificati
(defun make (class-name &rest args)
  (cond ((and (= (length args) 1)
	      (listp (car args))
	      (= (length (car args)) 3)
	      (equal (caar args) 'oolinst))
	 (if (is-subtype-of (cadar args) class-name)
	     (list 'oolinst class-name (caddar args))
             (error "Campo non valido.")))
	((is-class class-name)
	 (list 'oolinst
	       class-name
	       (upgrade-field (define-list-field
				  (class-spec-fields class-name)
				  (get-field-type-parents
                                   (class-spec-parents class-name)))
                              args)))
        ((and (equal class-name 'T)
	      (= (length args) 1))
	 (list 'oolinst 'T (car args)))
	((and (= (length args) 1)
	      (subtypep (type-of (car args)) class-name))
	 (list 'oolinst class-name (car args)))
	(t (error "Campo non valido."))))

;;; Controlla se il nome della classe Ã¨ giÃ  in uso
(defun check-class-name (class-name)
  (if (is-class class-name)
      (error "La classe esiste gia'.")))

;;; Controlla se tutte le classi genitore siano definite
(defun check-parents (parents-list)
  (cond ((null parents-list) T)
        ((is-class (car parents-list))
         (check-parents (cdr parents-list)))
        (t (error "Una o piu' classi genitore non definite."))))

;;; Verifica se la classe e' definita
(defun is-class (class-name)
  (if (class-spec class-name) T
      NIL))

;;; Verifica se l'oggetto Ã¨ un'istanza della classe specificata
(defun is-instance (instance &optional (class-name 'T))
  (and (listp instance)
       (eq (car instance) 'oolinst)
       (if (eq class-name 'T)
           (or (is-class (cadr instance))
               (subtypep (type-of (caddr instance)) (cadr instance)))
           (and (is-subtype-of (cadr instance) class-name)
		(or (is-class (cadr instance))
		    (subtypep (type-of (caddr instance))
			      (cadr instance)))))))

;;; Eredita i campi dalle classi genitore
(defun get-field-type-parents(parents-list)
  (if (not (null parents-list))
      (define-list-field
	  (define-list-field (class-spec-fields (car parents-list))
	      (get-field-type-parents
	       (class-spec-parents (car parents-list))))
	  (get-field-type-parents(cdr parents-list)))
      '()))

;;; Unisce due liste di campi, evitando duplicati
(defun define-list-field (list1 list2)
  (if list2
      (if (not (get-field-type (caar list2) list1))
          (define-list-field (append list1 (list (car list2))) (cdr list2))
          (define-list-field list1 (cdr list2)))
      list1))

;;; Estrae la lista dei campi dalle parti della classe
(defun get-fields (parts)
  (if parts
      (if (equal (caar parts) 'fields)
          (append (mapcar #'rewrite-field (cdar parts))
                  (get-fields (cdr parts)))
          (get-fields (cdr parts)))
      '()))

;;; Esegue la validazione di un campo
(defun rewrite-field (field)
  (if (= (length field) 2)
      (list (symbol-name (car field)) (make 'T (cadr field)))
      (if (= (length field) 3)
          (list (symbol-name (car field)) (make (caddr field) (cadr field)))
          nil)))

;;; Esegue la validazione dei campi in modo ricorsivo
(defun helper-rewrite-field (fields)
  (if fields
      (append (list (rewrite-field (car fields)))
              (helper-rewrite-field (cdr fields)))
      '()))

;;; Restituisce il valore di un campo specifico
(defun field (instance field-name)
  (if (and (is-instance instance) (get-field-type (symbol-name field-name)
						  (caddr instance)))
      (get-field-type (symbol-name field-name) (caddr instance))
      (error "Campo sconosciuto.")))

;;; Restituisce i valori di uno o piÃ¹ campi specifici
(defun field* (instance &rest fields-name)
  (if (is-instance instance)
      (if fields-name
          (if (null (car fields-name))
              (error "Il nome del field non puÃ² essere vuoto.")
              (field* (field instance (car fields-name)) (cdr fields-name)))
          instance)
      (error "Parametro invalido.")))

;;; Restituisce la lista di campi con i relativi tipi
(defun get_field_type (fields parents)
  (cond
    ((null parents) fields)
    ((null fields) '())
    ((and (equal (car fields) (car parents)))
     (if (is-instance (cadr (cadr (car fields))) (cadadr (car parents)))
         (list (cons (caar fields) (cons 'oolinst
					 (cadr (cadr (car fields))))))
         (error "Valore non valido.")))
    ((equal (caar fields) (caar parents))
     (if (is-subtype-of (cadadr (car fields))
                        (cadadr (car parents)))
         (list (car fields))
         (error "Il tipo del campo Ã¨ il supertipo di uno ereditato.")))
    (t (append (get_field_type (list (car fields))
                               (cdr parents))
               (get_field_type (cdr fields)
                               parents)))))

;;; Elabora un metodo e lo associa al nome specificato
(defun process-method (method-name method-spec)
  (setf (fdefinition method-name)
        (lambda (this &rest args)
          (apply (eval (invoke-method this method-name))
                 (cons this args))))
  (rewrite-method-code method-name method-spec))

;; Riscrive il codice del metodo come una funzione lambda
(defun rewrite-method-code (method-name method-spec)
  (list method-name 
        (append (list 'lambda)
                (list (cons 'this (car method-spec)))
                (cdr method-spec))))

;;; Invoca un metodo sull'istanza, tenendo conto dell'ereditarietÃ 
(defun invoke-method (instance method-name)
  (let ((instance-methods (class-spec-methods (cadr instance)))
        (parent-method (parents-method (class-spec-parents (cadr instance))
				       method-name)))
    (if instance-methods
        (if (extract-method method-name instance-methods)
            (extract-method method-name instance-methods)
            (if parent-method
                parent-method
                (error "Il metodo non esiste.")))
        (if parent-method
            parent-method
            (error "Il metodo non esiste.")))))

;;; Aggiorna i campi della classe con i nuovi valori
(defun upgrade-field (old-fields args)
  (if args
      (if (get-field-type (symbol-name (car args)) old-fields)
          (upgrade-field (upgrade-field-helper old-fields
					       (list (car args)
						     (cadr args)))
                         (cddr args))
          (error "Aggiornamento campi non riuscito."))
      old-fields))

;;; Aggiorna un campo specifico nella lista dei campi
(defun upgrade-field-helper (old-fields field)
  (if (null old-fields)
      (error "Campo non trovato.")
      (if (equal (caar old-fields) (symbol-name (car field)))
          (cons (list (caar old-fields)
                      (make (cadr (cadar old-fields)) (cadr field)))
                (cdr old-fields))
          (cons (car old-fields)
                (upgrade-field-helper (cdr old-fields) field)))))

;;; Estrae la lista dei metodi dalle parti della classe
(defun get-methods (parts)
  (cond ((null parts)
	 nil)
        ((equal (caar parts) 'methods)
         (append (get-methods-helper (cdar parts))
                 (get-methods-helper (cdr parts))))
        (t (get-methods (cdr parts)))))

;;; Estrae i metodi dalle parti della classe in modo ricorsivo
(defun get-methods-helper (methods)
  (if (null methods)
      nil
      (cons (process-method (caar methods) (cdar methods))
            (get-methods-helper (cdr methods)))))

;;; Restituisce il tipo del campo specificato
(defun get-field-type (field fields-list)
  (cond ((null fields-list) nil)
        ((equal field (caar fields-list))
         (if (is-class (cadr (cadar fields-list)))
             (cadar fields-list)
             (caddr (cadar fields-list))))
        (t (get-field-type field (cdr fields-list)))))

;;; Verifica se una classe Ã¨ un sottotipo di un'altra
(defun is-subtype-of (subclass parent-class)
  (or (equal parent-class 'T)
      (equal subclass parent-class)
      (inherit-class (list subclass) parent-class)
      (subtypep subclass parent-class)))

;;;; Verifica se una classe Ã¨ ereditata da un'altra
(defun inherit-class (inherited-class parent-class)
  (or (and inherited-class
           (equal (car inherited-class) parent-class))
      (and inherited-class
           (inherit-class (class-spec-parents (car inherited-class))
                          parent-class))
      (inherit-class (cdr inherited-class) parent-class)))

;;; Ottiene il metodo da una delle classi genitore
(defun parents-method (parents-list method-name)
  (if (null parents-list)
      nil
      (if (has-method-in-class-spec (car parents-list) method-name)
          (parents-method (class-spec-parents (car parents-list))
			  method-name)
          (parents-method (cdr parents-list) method-name))))

;;; Verifica se una classe ha un certo metodo passandogli il nome
(defun has-method-in-class-spec (class-spec method-name)
  (extract-method method-name (class-spec-methods class-spec)))

;;; Estrae il corpo del metodo dalla lista dei metodi
(defun extract-method (method-name methods-list)
  (if (null methods-list)
      nil
      (if (equal method-name (caar methods-list))
          (cadar methods-list)
          (extract-method method-name (cdr methods-list)))))
