(defglobal
	?*HwG* = 5
    ?*HwhG* = 5  
    ?*nmr* = 0
    ?*mr* = 0
    ?*true* = 0
    ?*false* = 0
    ?*prc* = "%"
    ?*f* = ""
)

(defglobal
    ?*d* = 0
	?*typ* = ""
    ?*rm* = 0
    ?*pr* = 0
    ?*lc* = "" 
    ?*gr* = 0  
)

(deftemplate user
    (slot name (default ""))
    (slot gender (default ""))
	(slot type (default ""))
    (slot interest (default ""))
    (slot income (default 0))
    (slot location (default "")) 
    (slot garageNum (default 0))
    (slot input (default 0))
)

(deftemplate HwhG
    (slot id)
	(slot type (default ""))
    (slot room (default 0))
    (slot price (default 0))
    (slot location (default ""))    
)

(deftemplate HwhG2
    (slot id)
	(slot type (default ""))
    (slot room (default 0))
    (slot price (default 0))
    (slot location (default ""))    
)

(deftemplate HwG 
    (slot id)
	(slot typee (default ""))
    (slot roomm (default 0))
    (slot pricee (default 0))
    (slot locationn (default "")) 
    (slot garageNumm (default 0))   
)

(deftemplate HwG2 
    (slot id)
	(slot typee (default ""))
    (slot roomm (default 0))
    (slot pricee (default 0))
    (slot locationn (default "")) 
    (slot garageNumm (default 0))    
)

;TAMBAH
(deftemplate matchRate
	(slot match (default 0))
    (slot type (default ""))
    (slot room (default 0))
    (slot price (default 0))
    (slot location (default "")) 
    (slot garageNum (default 0)) 
    (slot id (default 0))    
)

(deffacts factHwG
    (HwG (id 1) (typee "Light House")(roomm 4)(pricee 7500)(locationn "West Jakarta")(garageNumm 1))
    (HwG (id 2) (typee "Skyscraper")(roomm 5)(pricee 175000)(locationn "South Jakarta")(garageNumm 3))
    (HwG (id 3) (typee "Cottage")(roomm 3)(pricee 4500)(locationn "North Jakarta")(garageNumm 1))
    (HwG (id 4) (typee "Light House")(roomm 2)(pricee 7500)(locationn "South Jakarta")(garageNumm 2))
    (HwG (id 5) (typee "Cottage")(roomm 3)(pricee 30000)(locationn "West Jakarta")(garageNumm 2))
)

(deffacts factHwhG
    (HwhG (id 1) (type "Light House")(room 3)(price 10000)(location "West Jakarta"))
    (HwhG (id 2) (type "Cottage")(room 2)(price 5000)(location "North Jakarta"))
    (HwhG (id 3) (type "Skyscraper")(room 4)(price 100000)(location "West Jakarta"))
    (HwhG (id 4) (type "Light House")(room 3)(price 25000)(location "South Jakarta"))
    (HwhG (id 5) (type "Cottage")(room 3)(price 7500)(location "South Jakarta"))
)

(defrule ast
    ?x <- (astt)
    ?idx <- (HwhG (id ?id) (type ?type) (room ?room) (price ?price) (location ?loc))
    =>
    (assert (HwhG2 (id ?input) (type ?type) (room ?room) (price ?price) (location ?loc)))
    (retract ?idx)
    (retract ?x)
)

(defrule mod
    ?d <- (modd) 
   	?iddd <- (HwhG (id ?input) (type ?type) (room ?room) (price ?price) (location ?loc))
    =>
    (if (eq ?input ?*d*) then
	    (modify ?iddd (type ?*typ*) (room ?*rm*) (price ?*pr*) (location ?*lc*))
        (assert(astt))
     else
        (assert (HwhG2 (id ?input) (type ?type) (room ?room) (price ?price) (location ?loc)))
        (retract ?iddd)
    )
)

(defrule assert
	(ast)
    ?hpps <- (HwhG2 (id ?id) (type ?type) (room ?room) (price ?price) (location ?loc))
    =>
    (assert (HwhG (id ?id) (type ?type) (room ?room) (price ?price) (location ?loc)))
    (retract ?hpps)
)

(defrule astHwG
    ?y <- (astH)
    ?idy <- (HwG (id ?id) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar))
    =>
    (assert (HwG2 (id ?input) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar)))
    (retract ?idy)
    (retract ?y)
)

(defrule modHwG
    ?d1 <- (mod2) 
   	?iddd1 <- (HwG (id ?input) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar))
    =>
    (if (eq ?input ?*d*) then
	    (modify ?iddd1 (typee ?*typ*) (roomm ?*rm*) (pricee ?*pr*) (locationn ?*lc*) (garageNumm ?*gr*))
        (assert(astH))
     else
        (assert (HwG2 (id ?input) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar)))
        (retract ?iddd1)
    )
)

(defrule assertHwG
	(asst)
    ?hps <- (HwG2 (id ?id) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar))
    =>
    (assert (HwG (id ?id) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar)))
    (retract ?hps)
)

(defrule insertHwG2
	(instHwG2)    
    (HwG (id ?id) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar))
    =>
    (assert (HwG2 (id ?id) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar)))
)

(defrule printHwG
    (prtHwG)
    (HwG2 (id ?id) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar))
    ?id2 <- (HwG2)
    =>
    (printout t (format nil "| %d . | %-23s | %-18d| %-11d$USD | %-17s | %-11d |" ?id ?type ?room ?price ?loc ?gar) crlf)
    (retract ?id2)
)

(defrule insertHwhG2
	(instHwhG2)    
    (HwhG (id ?id) (type ?type) (room ?room) (price ?price) (location ?loc))
    =>
    (assert (HwhG2 (id ?id) (type ?type) (room ?room) (price ?price) (location ?loc)))
)

(defrule printHwhG
    (prtHwhG)
    (HwhG2 (id ?id) (type ?type) (room ?room) (price ?price) (location ?loc))
    ?id3 <- (HwhG2)
    => 
    (printout t (format nil "| %d . | %-23s | %-18d| %-11d$USD | %-16s |" ?id ?type ?room ?price ?loc) crlf)
    (retract ?id3)
)

(defquery tesT
	(user (input ?inp))    
)

(defquery finalRes
	(user (name ?name)(gender ?gen)(type ?type)(interest ?int)(income ?inc)(location ?loc)(garageNum ?car))
)

(defquery finalMatchHwG
    (matchRate(type ?type)(room ?room)(price ?price)(location ?loc)(garageNum ?gar)(match ?mt)(id ?id))
)

(defquery finalMatchHwhG
    (matchRate(type ?type)(room ?room)(price ?price)(location ?loc)(match ?mt)(id ?id))
)

(defrule deleteQ
    (dQr)
    ?dQ <- (user (name ?name)(gender ?gen)(type ?type)(interest ?int)(income ?inc)(location ?loc)(garageNum ?car))
    =>
    (retract ?dQ)
)

(defrule deleteF
    ?d3 <- (dQf)
    ?dF <- (matchRate(type ?type)(room ?room)(price ?price)(location ?loc)(garageNum ?gar)(match ?mt)(id ?id))
    =>
    (retract ?dF)
    (retract ?d3)
)

(defrule retractHwG
    ?rr <- (rHwG)
	?r1 <- (HwG (id ?id) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar))
    =>
    (if (eq ?id ?*d*) then
	    (retract ?r1)
     else
        (assert (HwG2 (id ?*nmr*) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar)))
        (retract ?r1)
        (-- ?*nmr*)
    )
)

(defrule reassertHwG
	(raHwG)
    ?hps1 <- (HwG2 (id ?id) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar))
    =>
    (assert (HwG (id ?id) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar)))
    (retract ?hps1)
)

(defrule retractHwhG
    ?rl <- (rHwhG)
	?r2 <- (HwhG (id ?id) (type ?type) (room ?room) (price ?price) (location ?loc))
    =>
    (if (eq ?id ?*d*) then
	    (retract ?r2)   
     else
        (assert (HwhG2 (id ?*nmr*) (type ?type) (room ?room) (price ?price) (location ?loc)))
        (retract ?r2)
        (-- ?*nmr*)
    )
)

(defrule reassertHwhG
	(raHwhG)
    ?hps2 <- (HwhG2 (id ?id) (type ?type) (room ?room) (price ?price) (location ?loc))
    =>
    (assert (HwhG (id ?id) (type ?type) (room ?room) (price ?price) (location ?loc)))
    (retract ?hps2)
)

(deffunction menu()
	(printout t "================" crlf)
    (printout t "|| Beli Rumah ||" crlf)
    (printout t "================" crlf)
    (printout t "1. View House" crlf)
    (printout t "2. Add a New House" crlf)
    (printout t "3. Update House Detail" crlf)
    (printout t "4. Delete House" crlf)
    (printout t "5. Search Match" crlf)
    (printout t "6. Exit" crlf)    
)

(deffunction menu1()
    (printout t "1. House with Garage" crlf)
    (printout t "2. House without Garage" crlf)    
)

(deffunction ClearScreen()
	(for (bind ?i 0)(< ?i 40)(++ ?i)
    	(printout t crlf)    
    )    
)

(defrule chkHwG
    ?chk <- (chHwG)
    (HwG (id ?id) (typee ?type) (roomm ?room) (pricee ?price) (locationn ?loc) (garageNumm ?gar))
    =>
    (if(eq (str-compare ?*lc* ?loc) 0)then
    	(++ ?*true*)
        
     elif(neq (str-compare ?*lc* ?loc) 0)then
        (bind ?*mr* (- ?*mr* 10))
        (++ ?*false*)         		
    )
    
    (if(eq (str-compare ?*typ* ?type) 0)then
    	(++ ?*true*)
        
     elif(neq (str-compare ?*typ* ?type) 0)then
        (bind ?*mr* (- ?*mr* 5))
        (++ ?*false*)         
    )
    
    (if(>= ?*d* ?price)then
    	(++ ?*true*)
        
     else
        (bind ?*mr* (- ?*mr* 10))
        (++ ?*false*)         
    )
    
    (if(<= ?*gr* ?gar)then
    	(++ ?*true*)
        
     else
        (bind ?*mr* (- ?*mr* 10))
        (++ ?*false*)         
    )
    
    (if(> ?*mr* 0) then
        (bind ?*true* 0)
	    (bind ?*false* 0)
	    (assert(matchRate(type ?type)(room ?room)(price ?price)(location ?loc)(garageNum ?gar)(match ?*mr*)(id ?id)))
	    (bind ?*mr* 100)
    )
)

(defrule chkHwhG
    ?chhk <- (chHwhG)
    (HwhG (id ?id) (type ?type) (room ?room) (price ?price) (location ?loc))
    =>
    (if(eq (str-compare ?*lc* ?loc) 0)then
    	(++ ?*true*)
        
     elif(neq (str-compare ?*lc* ?loc) 0)then
        (bind ?*mr* (- ?*mr* 10))
        (++ ?*false*)         
    )
    
    (if(eq (str-compare ?*typ* ?type) 0)then
    	(++ ?*true*)
        
     elif(neq (str-compare ?*typ* ?type) 0)then
        (bind ?*mr* (- ?*mr* 5))
        (++ ?*false*)         
    )
    
    (if(>= ?*d* ?price)then
    	(++ ?*true*)
        
     else
        (bind ?*mr* (- ?*mr* 10))
        (++ ?*false*)         
    )
    
    (if(> ?*mr* 0) then
        (bind ?*true* 0)
	    (bind ?*false* 0)
	    (assert(matchRate(type ?type)(room ?room)(price ?price)(location ?loc)(match ?*mr*)(id ?id))) 
	    (bind ?*mr* 100)
    )
)

(reset)
(bind ?i 0)
(while (neq ?i 6)
    (ClearScreen)
    (menu)
    (bind ?i 0)
    (while(or(< ?i 1)(> ?i 6))
        (printout t ">> Input [1-6]: ")
        (bind ?i(read))
        (if(eq (numberp ?i) FALSE) then
        	(bind ?i 0)
        )
    )
    ;NOMOR 1
    (if(eq ?i 1) then
        (ClearScreen)
        (printout t "List of house to be viewed" crlf)
        (printout t "==========================" crlf)
        (menu1)
        (bind ?input -1)
        (while(or(< ?input 0)(> ?input 2))
        	(printout t ">> Choose [1..2 | 0 back to main menu]: ")
        	(bind ?input(read))
        	(if(eq (numberp ?input) FALSE) then
        		(bind ?input -1)
        	)
        )
        
        
        (if(eq ?input 1) then
            (printout t "=========================================================================================================" crlf)
            (printout t "| No. | Type                    | Room              | Price           | Location          | Garage      |" crlf)
   			(printout t "=========================================================================================================" crlf)
           	(assert(instHwG2))
            (run)
            (assert(prtHwG))
            (run)  
            (retract-string "(prtHwG)")
            (retract-string "(instHwG2)")
            (printout t "=========================================================================================================" crlf)
           
            (printout t "Press Enter to Continue...")
            (readline)
         
         elif(eq ?input 2) then
            (printout t "==========================================================================================" crlf)
            (printout t "| No. | Type                    | Room              | Price           | Location         |" crlf)
   			(printout t "==========================================================================================" crlf)
            (assert(instHwhG2))
            (run)
            (assert(prtHwhG))
            (run)  
            (retract-string "(prtHwhG)")
            (retract-string "(instHwhG2)")
            (printout t "==========================================================================================" crlf)
            
            (printout t "Press Enter to Continue...") 
            (readline)
        )
     
     ;NOMOR 2
     elif(eq ?i 2) then
        (ClearScreen)
        (printout t "Type of houses to be added" crlf)
        (printout t "==========================" crlf)
        (menu1)
        (bind ?input -1)
        
        (while(or(< ?input 0)(> ?input 2))
        	(printout t ">> Choose [1..2 | 0 back to main menu]: ")
        	(bind ?input(read))
        	(if(eq (numberp ?input) FALSE) then
        		(bind ?input -1)
        	)
        )
        
        
        (if(eq ?input 1) then
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;INSERT HwG
            (ClearScreen)
	        (bind ?type "")
            
	        (while(and(neq (str-compare ?type "Cottage") 0)(neq (str-compare ?type "Light House") 0)
                    (neq (str-compare ?type "Skyscraper") 0)) 
                (printout t "Input house type [Cottage | Light House | Skyscraper](CASE-SENSITIVE): ")           
	            (bind ?type(readline))
	            (if(eq (lexemep ?type) FALSE) then
	                (bind ?type "")
	            )
	        )
            
	        (bind ?num 0)
	        
            (while(or(< ?num 1)(> ?num 5))
                (printout t "Input room number [1 - 5] : ")
	            (bind ?num(read))
	            (if(eq (numberp ?num) FALSE) then
	                (bind ?num 0)
	            )
	        )
            
	        (bind ?price 0)
	        
            (while(or(< ?price 1000)(> ?price 500000))
                (printout t "Input house price [1000 - 500000] (dollars) : ")
	            (bind ?price(read))
	            (if(eq (numberp ?price) FALSE) then
	                (bind ?price 0)
	            )
	        )
             
	        (bind ?loc "")
            
	        (while(and(neq (str-compare ?loc "West Jakarta") 0)(neq (str-compare ?loc "North Jakarta") 0)
                    (neq (str-compare ?loc "South Jakarta") 0))   
                (printout t "Input house location [West Jakarta | North Jakarta | South Jakarta](CASE-SENSITIVE): ")         
	            (bind ?loc(readline))
	            (if(eq (lexemep ?loc) FALSE) then
	                (bind ?loc "")
	            )
	        )
                        
	        (bind ?gar 0)
	        
            (while(or(< ?gar 1)(> ?gar 5))
                (printout t "Input garage number [1 - 5] : ")
	            (bind ?gar(read))
	            (if(eq (numberp ?gar) FALSE) then
	                (bind ?num 0)
	            )
	        )
            
            (++ ?*HwG*)
            (assert (HwG (id ?*HwG*) (typee ?type) (roomm ?num) (pricee ?price) (locationn ?loc) (garageNumm ?gar)))   
	        (printout t "Press ENTER to Continue...")
	        (readline)
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
         elif(eq ?input 2) then
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;INSERT HwhG
            (ClearScreen)            
	        (bind ?type "")
            
	        (while(and(neq (str-compare ?type "Cottage") 0)(neq (str-compare ?type "Light House") 0)
                    (neq (str-compare ?type "Skyscraper") 0))
                (printout t "Input house type [Cottage | Light House | Skyscraper](CASE-SENSITIVE): ")            
	            (bind ?type(readline))
	            (if(eq (lexemep ?type) FALSE) then
	                (bind ?type "")
	            )
	        )
                       
	        (bind ?num 0)
	        
            (while(or(< ?num 1)(> ?num 5))
                (printout t "Input room number [1 - 5] : ")
	            (bind ?num(read))
	            (if(eq (numberp ?num) FALSE) then
	                (bind ?num 0)
	            )
	        )
                       
	        (bind ?price 0)
	        
            (while(or(< ?price 1000)(> ?price 500000))
                (printout t "Input house price [1000 - 500000] (dollars) : ")
	            (bind ?price(read))
	            (if(eq (numberp ?price) FALSE) then
	                (bind ?price 0)
	            )
	        )
                        
	        (bind ?loc "")
            
	        (while(and(neq (str-compare ?loc "West Jakarta") 0)(neq (str-compare ?loc "North Jakarta") 0)
                    (neq (str-compare ?loc "South Jakarta") 0))      
                (printout t "Input house location [West Jakarta | North Jakarta | South Jakarta](CASE-SENSITIVE): ")      
	            (bind ?loc(readline))
	            (if(eq (lexemep ?loc) FALSE) then
	                (bind ?loc "")
	            )
	        )
            
            (++ ?*HwhG*)
            (assert (HwhG (id ?*HwhG*) (type ?type) (room ?num) (price ?price) (location ?loc)))	        
	        (printout t "Press ENTER to Continue...")
	        (readline)
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
        )
     
     ;NOMOR 3
     elif(eq ?i 3) then
        (ClearScreen)
        (printout t "Types of houses to be updated" crlf)
        (printout t "=============================" crlf)
        (menu1)
        (bind ?input -1)
        
        (while(or(< ?input 0)(> ?input 2))
        	(printout t ">> Choose [1..2 | 0 back to main menu]: ")
        	(bind ?input(read))
        	(if(eq (numberp ?input) FALSE) then
        		(bind ?input -1)
        	)
        )
        
        
        (if(eq ?input 1) then
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;UPDATE HwG
            (ClearScreen)
            (printout t "=========================================================================================================" crlf)
            (printout t "| No. | Type                    | Room              | Price           | Location          | Garage      |" crlf)
   			(printout t "=========================================================================================================" crlf)
           	(assert(instHwG2))
            (run)
            (assert(prtHwG))
            (run)  
            (retract-string "(prtHwG)")
            (retract-string "(instHwG2)")
            (printout t "=========================================================================================================" crlf)
            
            (bind ?input -1)
	        
	        (while(or(< ?input 0)(> ?input ?*HwG*))
	        	(printout t "Which house to be deleted[1.." ?*HwG* " | 0 back to main menu]: ")
	        	(bind ?input(read))
	        	(if(eq (numberp ?input) FALSE) then
	        		(bind ?input -1)
	        	)
	        )
            
            (if (neq ?input 0) then
	            (bind ?*d* ?input)
	            
		        (bind ?type "")
	            
		        (while(and(neq (str-compare ?type "Cottage") 0)(neq (str-compare ?type "Light House") 0)
	                    (neq (str-compare ?type "Skyscraper") 0)) 
	                (printout t "Input house type [Cottage | Light House | Skyscraper](CASE-SENSITIVE): ")           
		            (bind ?type(readline))
		            (if(eq (lexemep ?type) FALSE) then
		                (bind ?type "")
		            )
		        )
	            
	            (bind ?*typ* ?type)
	            
		        (bind ?num 0)
		        
	            (while(or(< ?num 1)(> ?num 5))
	                (printout t "Input room number [1 - 5] : ")
		            (bind ?num(read))
		            (if(eq (numberp ?num) FALSE) then
		                (bind ?num 0)
		            )
		        )
	            
	            (bind ?*rm* ?num)
	            
		        (bind ?price 0)
		        
	            (while(or(< ?price 1000)(> ?price 500000))
	                (printout t "Input house price [1000 - 500000] (dollars) : ")
		            (bind ?price(read))
		            (if(eq (numberp ?price) FALSE) then
		                (bind ?price 0)
		            )
		        )
	             
	            (bind ?*pr* ?price)
	            
		        (bind ?loc "")
	            
		        (while(and(neq (str-compare ?loc "West Jakarta") 0)(neq (str-compare ?loc "North Jakarta") 0)
	                    (neq (str-compare ?loc "South Jakarta") 0))   
	                (printout t "Input house location [West Jakarta | North Jakarta | South Jakarta](CASE-SENSITIVE): ")         
		            (bind ?loc(readline))
		            (if(eq (lexemep ?loc) FALSE) then
		                (bind ?loc "")
		            )
		        )
	            
	            (bind ?*lc* ?loc)
	                        
		        (bind ?gar 0)
		        
	            (while(or(< ?gar 1)(> ?gar 5))
	                (printout t "Input garage number [1 - 5] : ")
		            (bind ?gar(read))
		            (if(eq (numberp ?gar) FALSE) then
		                (bind ?num 0)
		            )
		        )
	            
	            (bind ?*gr* ?gar)
	            
	            (assert(mod2))
	            (run)
	            (retract-string "(mod2)")
	            
	            (assert(asst))
	            (run)
	            (retract-string "(asst)")   
	                 
		        (printout t "Press ENTER to Continue...")
		        (readline)
             )
            
         elif(eq ?input 2) then
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;UPDATE HwhG
            (ClearScreen)
            (printout t "==========================================================================================" crlf)
            (printout t "| No. | Type                    | Room              | Price           | Location         |" crlf)
   			(printout t "==========================================================================================" crlf)
           	(assert(instHwhG2))
            (run)
            (assert(prtHwhG))
            (run)  
            (retract-string "(prtHwhG)")
            (retract-string "(instHwhG2)")
            (printout t "==========================================================================================" crlf)
                     
            (bind ?input -1)
	        
	        (while(or(< ?input 0)(> ?input ?*HwhG*))
	        	(printout t "Which house to be deleted[1.." ?*HwhG* " | 0 back to main menu]: ")
	        	(bind ?input(read))
	        	(if(eq (numberp ?input) FALSE) then
	        		(bind ?input -1)
	        	)
	        )
            
            (if (neq ?input 0) then
	            (bind ?*d* ?input)
	               
		        (bind ?type "")
	            
		        (while(and(neq (str-compare ?type "Cottage") 0)(neq (str-compare ?type "Light House") 0)
	                    (neq (str-compare ?type "Skyscraper") 0))
	                (printout t "Input house type [Cottage | Light House | Skyscraper](CASE-SENSITIVE): ")            
		            (bind ?type(readline))
		            (if(eq (lexemep ?type) FALSE) then
		                (bind ?type "")
		            )
		        )
	            
	            (bind ?*typ* ?type)
	                       
		        (bind ?num 0)
		        
	            (while(or(< ?num 1)(> ?num 5))
	                (printout t "Input room number [1 - 5] : ")
		            (bind ?num(read))
		            (if(eq (numberp ?num) FALSE) then
		                (bind ?num 0)
		            )
		        )
	            
	            (bind ?*rm* ?num)
	                       
		        (bind ?price 0)
		        ;
	            (while(or(< ?price 1000)(> ?price 500000))
	                (printout t "Input house price [1000 - 500000] (dollars) : ")
		            (bind ?price(read))
		            (if(eq (numberp ?price) FALSE) then
		                (bind ?price 0)
		            )
		        );
	            
	            (bind ?*pr* ?price)
	                        
		        (bind ?loc "")
	            
		        (while(and(neq (str-compare ?loc "West Jakarta") 0)(neq (str-compare ?loc "North Jakarta") 0)
	                    (neq (str-compare ?loc "South Jakarta") 0))      
	                (printout t "Input house location [West Jakarta | North Jakarta | South Jakarta](CASE-SENSITIVE): ")      
		            (bind ?loc(readline))
		            (if(eq (lexemep ?loc) FALSE) then
		                (bind ?loc "")
		            )
		        )
	         
	    		(bind ?*lc* ?loc)
	            
	            
	            (assert(modd))
	            (run)
	            (retract-string "(modd)")
	            
	            (assert(ast))
	            (run)
	            (retract-string "(ast)")   
	                
		        (printout t "Press ENTER to Continue...")
		        (readline)
	            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            )
        )
       
     ;NOMOR 4
     elif(eq ?i 4) then
        (ClearScreen)
        (printout t "Types of house to be deleted" crlf)
        (printout t "============================" crlf)
        (menu1)
        
        (bind ?input -1)
        
        (while(or(< ?input 0)(> ?input 2))
        	(printout t ">> Choose [1..2 | 0 back to main menu]: ")
        	(bind ?input(read))
        	(if(eq (numberp ?input) FALSE) then
        		(bind ?input -1)
        	)
        )
        
        (if(eq ?input 1) then
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;DELETE HwG
            (ClearScreen)
            (printout t "=========================================================================================================" crlf)
            (printout t "| No. | Type                    | Room              | Price           | Location          | Garage      |" crlf)
   			(printout t "=========================================================================================================" crlf)
           	(assert(instHwG2))
            (run)
            (assert(prtHwG))
            (run)  
            (retract-string "(prtHwG)")
            (retract-string "(instHwG2)")
            (printout t "=========================================================================================================" crlf)
            
            (bind ?input -1)
	        
	        (while(or(< ?input 0)(> ?input ?*HwG*))
	        	(printout t "Which house to be deleted[1.." ?*HwG* " | 0 back to main menu]: ")
	        	(bind ?input(read))
	        	(if(eq (numberp ?input) FALSE) then
	        		(bind ?input -1)
	        	)
	        )
            
            (if (neq ?input 0) then
	            (bind ?*d* ?input)
	            (-- ?*HwG*)
	            (bind ?*nmr* ?*HwG*)
	            (assert(rHwG))
	            (run)
	            (retract-string "(rHwG)")
	            
	            (assert(raHwG))
	            (run)
	            (retract-string "(raHwG)")
	            
	            (printout t "Press ENTER to Continue...")
		        (readline)
           	)
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             
         elif(eq ?input 2) then
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;DELETE HwhG
            (ClearScreen)
            (printout t "==========================================================================================" crlf)
            (printout t "| No. | Type                    | Room              | Price           | Location         |" crlf)
   			(printout t "==========================================================================================" crlf)
           	(assert(instHwhG2))
            (run)
            (assert(prtHwhG))
            (run)  
            (retract-string "(prtHwhG)")
            (retract-string "(instHwhG2)")
            (printout t "==========================================================================================" crlf)
            
            (bind ?input -1)
	        
	        (while(or(< ?input 0)(> ?input ?*HwhG*))
	        	(printout t "Which house to be deleted[1.." ?*HwhG* " | 0 back to main menu]: ")
	        	(bind ?input(read))
	        	(if(eq (numberp ?input) FALSE) then
	        		(bind ?input -1)
	        	)
	        )
            
            (if(neq ?input 0) then
	            (bind ?*d* ?input)
	            (-- ?*HwhG*)
	            (bind ?*nmr* ?*HwhG*)
	            (assert(rHwhG))
	            (run)
	            (retract-string "(rHwhG)")
	            (assert(raHwhG))
	            (run)
	            (retract-string "(raHwhG)")
	            (printout t "Press ENTER to Continue...")
		        (readline)
            )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
        )
     
     ;NOMOR 5
     elif(eq ?i 5) then
        (ClearScreen)
        
        (bind ?name "")
        (while(or(< (str-length ?name) 3)(> (str-length ?name) 20))
            (printout t "Input your name [3 - 20 characters length]: ")
            (bind ?name(readline))
            (if(eq (lexemep ?name) FALSE) then
                (bind ?name "")
            )
        )
        
        (bind ?gen "")
        
	    (while(and(neq (str-compare ?gen "Male") 0)(neq (str-compare ?gen "Female") 0))
            (printout t "Input your gender [ Male | Female ](CASE-SENSITIVE): ")            
	        (bind ?gen(readline))
	        (if(eq (lexemep ?gen) FALSE) then
	            (bind ?gen "")
	        )
	    )
        
        (bind ?prf "")
        
        (while(and(neq (str-compare ?prf "With Garage") 0)(neq (str-compare ?prf "Without Garage") 0))
            (printout t "Input your house preference [ With Garage | Without Garage ](CASE-SENSITIVE): ")            
	        (bind ?prf(readline))
	        (if(eq (lexemep ?prf) FALSE) then
	            (bind ?prf "")
	        )
	    )
        
        (bind ?inc 0)
	    
        (while(or(< ?inc 10000)(> ?inc 500000))
            (printout t "Input your income [10000 - 500000] (dollars) : ")
	        (bind ?inc(read))
	        (if(eq (numberp ?inc) FALSE) then
	            (bind ?inc 0)
	        )
	    )
        
        (bind ?loc "")
        
	    (while(and(neq (str-compare ?loc "West Jakarta") 0)(neq (str-compare ?loc "North Jakarta") 0)
                (neq (str-compare ?loc "South Jakarta") 0))      
            (printout t "Input your work location [West Jakarta | North Jakarta | South Jakarta](CASE-SENSITIVE): ")      
	        (bind ?loc(readline))
	        (if(eq (lexemep ?loc) FALSE) then
	            (bind ?loc "")
	        )
	    )
        
        (bind ?type "")
        
	    (while(and(neq (str-compare ?type "Cottage") 0)(neq (str-compare ?type "Light House") 0)
                (neq (str-compare ?type "Skyscraper") 0))
            (printout t "Input your preferred house type [Cottage | Light House | Skyscraper](CASE-SENSITIVE): ")            
	        (bind ?type(readline))
	        (if(eq (lexemep ?type) FALSE) then
	            (bind ?type "")
	        )
	    )
        
        
        (if (eq (str-compare ?prf "With Garage") 0) then
            (bind ?car 0)
	    	
	        (while(or(< ?car 1)(> ?car 5))
	            (printout t "Input number of car you own [1 - 5](cars): ")
		        (bind ?car(read))
		        (if(eq (numberp ?car) FALSE) then
		            (bind ?car 0)
		        )
		    )
            
            (bind ?*gr* ?car)
            (bind ?*d* ?inc)
	        (bind ?*typ* ?type)
	        (bind ?*lc* ?loc)
	        
	        (bind ?*mr* 100) 
            
            (assert(chHwG))
            (run)
            (retract-string "(chHwG)")
            
            (assert(dQr))
            (run)
            (retract-string "(dQr)")
            
            (assert (user (name ?name)(gender ?gen)(type ?type)(interest "With Garage")(income ?inc)(location ?loc)(garageNum ?car)))
           
         else
            (bind ?*d* ?inc)
	        (bind ?*typ* ?type)
	        (bind ?*lc* ?loc)
	        
	        (bind ?*mr* 100)
            
            (assert(chHwhG))
            (run)
            (retract-string "(chHwhG)")
            
            (assert(dQr))
            (run)
            (retract-string "(dQr)")
            
            (assert (user (name ?name)(gender ?gen)(type ?type)(interest "Without Garage")(income ?inc)(location ?loc)))
              
        )
        (bind ?i 6)
        
     ;NOMOR 6
     elif(eq ?i 6) then        
        (assert(user(input 6)))     
    )
)
