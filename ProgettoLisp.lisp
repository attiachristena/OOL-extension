;;;; Attia Christena 894887

;; Istanzia la hash-table
(defparameter *class-prop* (make-hash-table))

;; Definisce gli attributi delle hash-table
(defun set-hash (name class-spec)
  (setf (gethash name *class-prop*) class-spec))

;; Ritorna il valore definito precedentemente					
(defun get-hash (name)
  (gethash name *class-prop*))

;; Controlla che esistano i genitori
(defun parents-exist (parents)
  (every #'get-hash parents))

;; Controlla che la classe passata esista
(defun class-exists (class-name)
  (gethash class-name *class-prop*))

;; Definisce la classe
(defun def-class (class-name parents &rest parts)
  ;; Serie di controlli che verificano che i parametri siano validi
  (cond
    ((or (not (listp parents))
	(null parents)
	(not (parents-exist parents))
	(error "Errore: la lista parents non è valida.")))
    ((or (eq (null class-name)
	    (equal '() class-name))
	(not (symbolp class-name))
	(error "Errore: il nome della classe non è valido.")))
    ((class-exists class-name)
     (error "Errore: la classe ~a esiste già." class-name))
    (t
     ;; Creazione della classe e aggiunta dei genitori
     (setf (gethash class-name *class-prop*) (cons class-name parents))
     ;; Chiamata alla funzione ausiliaria per definire i campi e i metodi
     (def-class-fields class-name (rest parts))))
  ;; Ritorna il nome della classe 
  class-name)


;; Definisce la parte dei field e dei method
(defun def-class-fields (class-name parts)
  (cond
    ;; Se la lista di parti è vuota, termina la ricorsione
    ((null parts) nil)
    ;; Se la prima parte è 'fields', aggiunge i campi alla classe
    ((eq (first (first parts)) 'fields)
     (setf (gethash class-name *class-prop*)
           (append (gethash class-name *class-prop*) (rest (first parts))))
     ;; Chiamata ricorsiva con le parti rimanenti
     (def-class-fields class-name (rest parts)))
    ;; Se la prima parte è 'methods', aggiunge i metodi alla classe
    ((eq (first (first parts)) 'methods)
     (setf (gethash class-name *class-prop*)
           (append (gethash class-name *class-prop*) (rest (first parts))))
     ;; Chiamata ricorsiva con le parti rimanenti
     (def-class-fields class-name (rest parts)))
    ;; Se la prima parte non è né 'fields' né 'methods', segnala un errore
    (t
     (error "Errore: parte invalida"))))

;; Function to check if a symbol is a class
(defun is-class (name)
  "Restituisce T se l'atomo passato è il nome di una classe."
  (and (listp name)
       (eq (car name) 'is-class)
       (symbolp (cadr name))))


;; Function to check if an object is an instance of a class
(defun is-instance (object &optional class-name)
  (cond
    ((eq class-name 'T) t)  ;; If class-name is T, any instance is considered valid
    ((and (is-class class-name) (typep object (symbol-value class-name))) t)
    (t nil)))


	
 (defun make (class-name &rest fields)
  "Crea un'istanza della classe con i campi specificati."
  (cond ((not (is-class class-name))
         nil)
        (t
         (list 'oolinst
               class-name
               (field-structure (check-method (check-slot-exists class-name fields)))))))


(defun field-structure (fields)
  (cond ((= (list-length fields) 0) nil)
        ((member (car fields) (get-method-names (check-method fields)))
         (cons (list (car fields)
                     '=> 
                     (process-method (car fields) (caddr fields)))
               (field-structure (cdddr fields))))
        (t (cons (list (car fields) (cadr fields))
                 (field-structure (cddr fields))))))

;;; check-method: estrae i metodi dai vari fields passati 
;;; come argomento elementi e li restituisce in una cons.
(defun check-method (fields) 
  "Estrae i metodi dai fields."
  (cond ((null fields) nil) 
        ((and (listp (caddr fields)) (member '=> (caddr fields))) 
         (cons (cadr fields) 
               (cons (caddr fields) (check-method (cdddr fields)))))
        (t (check-method (cddr fields)))))

;;; get-method-names: dato in input una lista che contiene metodi, estrae
;;; e restituisce come cons solo i nomi del metodo senza il corpo.
(defun get-method-names (methods)
  "Restituisce una lista contenente solo i nomi dei metodi."
  (cond ((null methods) nil) 
        (t (cons (car methods) (get-method-names (cddr methods))))))

  






	   


     









    
  
    
  
		      

	 


	 
	
	
	
	
	
	
	
	      
