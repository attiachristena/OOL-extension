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


  
    
  
    
  
		      

	 


	 
	
	
	
	
	
	
	
	      
