; PRÁCTICA FINAL 
; Sistema Experto basado en Clips para asesorar a un inversor en bolsa

; ----------------------------------------------------------------------------
; ----------------------------------------------------------------------------
;									MODULO 0
; 						Entrada de datos y primera deducciones
; —---------------------------------------------------------------------------
; ----------------------------------------------------------------------------

; Antes de nada nos creamos un deftemplate donde leer los campos de cada uno 
; de los archivos que contienen la información necesaria para hacer la práctica

; Analisis.txt
(deftemplate AnalisisEmpresa
	(field Nombre)
	(field Precio)
	(field VarDia)
	(field Capitalizacion)
	(field PER)
	(field RPD)
	(field Tamanio)
	(field Ibex)
	(field EtiqPER)
	(field EtiqRPD)
	(field Sector)
	(field Var5dias)
	(field Perd3consec)
	(field Perd5consec)	
	(field VarRespSector5dias)
	(field VRS5)
	(field VarMen)
	(field VarTri)
	(field VarSem)
	(field Var12mes)
)

; AnalisiSector.txt
(deftemplate AnalisisSector
	(field Nombre)
	(field VarDia)
	(field Capitalizacion)
	(field PER)
	(field RPD)
	(field Ibex)
	(field Var5dias)
	(field Perd3consec)
	(field Perd5consec)	
	(field VarMen)
	(field VarTri)
	(field VarSem)
	(field Var12mes)
)

; Noticias.txt
(deftemplate Noticia
	(field Sobre)
	(field Tipo)
	(field Antiguedad)
	(field Fecha)
)

; Cartera.txt
(deftemplate Cartera_Acciones
	(field Nombre)
	(field Acciones)
	(field ValorActual)
)


; -----------------------------------------------------------------------------
; Tarea 0.1: Recoger datos
; -----------------------------------------------------------------------------

; ENTRADA: fichero.txt
; SALIDA: Datos guardados del fichero.txt en nuestra base de hechos

; Cargar datos de Analisis.txt (datos sobre empresas del ibex) 
; -----------------------------------------------------------------------------

(defrule openfileAnalisis
	(declare (salience 10))
	=>
	(open "Analisis.txt" Analisis)
	(assert (SeguirLeyendoAnalisis))
)

(defrule LeerValoresCierreFromFileAnalisis
	?f <- (SeguirLeyendoAnalisis)
	=>
	(bind ?nombre (read Analisis))
	(retract ?f)
	(if (neq ?nombre EOF) then
		(assert (AnalisisEmpresa
         	(Nombre ?nombre)
			(Precio (read Analisis))
			(VarDia (read Analisis))
			(Capitalizacion (read Analisis))
			(PER (read Analisis))
			(RPD (read Analisis))
			(Tamanio (read Analisis))
			(Ibex (read Analisis))
			(EtiqPER (read Analisis))
			(EtiqRPD (read Analisis))
			(Sector (read Analisis))
			(Var5dias (read Analisis))
			(Perd3consec (read Analisis))
			(Perd5consec (read Analisis))	
			(VarRespSector5dias (read Analisis))
			(VRS5 (read Analisis))
			(VarMen (read Analisis))
			(VarTri (read Analisis))
			(VarSem (read Analisis))
			(Var12mes (read Analisis)))
		)
		(assert (SeguirLeyendoAnalisis))
	)
)

(defrule closefileAnalisis
	(declare (salience -10))
	=>
	(close Analisis)
)

; Cargar datos de AnalisisSectores.txt (datos sobre los sectores) 
; -----------------------------------------------------------------------------

(defrule openfileSectores
	(declare (salience 10))
	=>
	(open "AnalisisSectores.txt" Sectores)
	(assert (SeguirLeyendoSectores)))

(defrule leerValoresCierreFromFileSectores
	?f <- (SeguirLeyendoSectores)
	=>
	(bind ?nombre (read Sectores))
	(retract ?f)
	(if (neq ?nombre EOF) then    
		(assert (AnalisisSector
         	(Nombre ?nombre)
			(VarDia (read Sectores))
			(Capitalizacion (read Sectores))
			(PER (read Sectores))
			(RPD (read Sectores))
			(Ibex (read Sectores))
			(Var5dias (read Sectores))
			(Perd3consec (read Sectores))
			(Perd5consec (read Sectores))	
			(VarMen (read Sectores))
			(VarTri (read Sectores))
			(VarSem (read Sectores))
			(Var12mes (read Sectores)))
		)
		(assert (SeguirLeyendoSectores))
	)
)

(defrule closefileSectores
	(declare (salience -10))
	=>
	(close Sectores)
)

; Cargar datos sobre Noticias.txt (noticias de tipo económico) 
; -----------------------------------------------------------------------------

(defrule openfileNoticias
	(declare (salience 10))
	=>
	(open "Noticias.txt" Noticias)
	(assert (SeguirLeyendoNoticias))
)

(defrule leerValoresCierreFromFileNoticias
	?f <- (SeguirLeyendoNoticias)
	=>
	(bind ?sobre (read Noticias))
	(retract ?f)
	(if (neq ?sobre EOF) then
		(assert (Noticia
         	(Sobre ?sobre)
			(Tipo (read Noticias))
			(Antiguedad (read Noticias))
			(Fecha (read Noticias)))
		)
		(assert (SeguirLeyendoNoticias))
	)
)

(defrule closefileNoticias
	(declare (salience -10))
	=>
	(close Noticias)
)

; -----------------------------------------------------------------------------
; Tarea 0.2: Recoger datos del usuario
; -----------------------------------------------------------------------------

; ENTRADA: Cartera.txt
; SALIDA: Datos guardados de Cartera.txt en nuestra base de hechos

; Solicitar y leer Cartera.txt del usuario

(defrule openfileCartera
	(declare (salience 10))
	=>
	(open "Cartera.txt" Cartera)
	(assert (SeguirLeyendoCartera))
)

(defrule leerCartera
   ?f <- (SeguirLeyendoCartera)
   =>
   (bind ?nombre (read Cartera))
   (retract ?f)
   (if (neq ?nombre EOF) then
       (assert (Cartera_Acciones
           (Nombre ?nombre) 
           (Acciones (read Cartera)) 
           (ValorActual (read Cartera)))
       )
       (assert (SeguirLeyendoCartera))
   )
)

(defrule closefileCartera
	(declare (salience -10))
	=>
	(close Cartera)
)

; -----------------------------------------------------------------------------
; Tarea 0.3: Deducir los datos deducidos
; -----------------------------------------------------------------------------

; Deducir los valores inestables
; -----------------------------------------------------------------------------

; ENTRADA: Si el campo del sector de la empresa es Construccion
; SALIDA: entonces es un valor inestable 
(defrule SectorConstruccionInestabilidad
	(declare (salience 10))
	; ponemos una prioridad de tiempo, ya que es un valor por defecto 
	; (nos lo dice el pdf)
	(AnalisisEmpresa (Nombre ?empresa) (Sector Construccion))
 	=>
 	(assert (Inestable ?empresa))
)

; ENTRADA: Si la econonomia lleva cayendo 5 dias en el sector Servicios
; SALIDA: entonces tales empresas tiene valores inestables
(defrule SectorServiciosInestabilidad
	(AnalisisEmpresa (Nombre ?empresa) (Sector Servicios) (Perd5consec true))
 	=>
 	(assert (Inestable ?empresa))
)

; ENTRADA: Si hay una noticia positiva sobre la empresa o su sector
; SALIDA: una empresa o sector con valor inestable deja de serlo durante 2 días
(defrule NoticiaPositiva
	(Noticia (Sobre ?empresa) (Tipo Buena)) 
	?valor <- (Inestable  ?empresa)  
	=>
	(retract ?valor)
	(assert (Estable ?valor)) 
)	

; ENTRADA: Si hay una noticia negativa sobre la empresa
; SALIDA: un valor pasa a ser inestable durante 2 días
(defrule NoticiaNegativa
	(Noticia (Sobre ?empresa) (Tipo Mala))
	=>
	(assert (Inestable ?empresa))
)

; ENTRADA: Si hay una noticia negativa sobre un sector
; SALIDA: los valores del sector pasan a ser inestables durante 2 días
(defrule NoticiaNegativaSector
	(Noticia (Sobre ?empresa) (Tipo Mala))
	(AnalisisEmpresa (Nombre ?empresa) (Sector ?s))
	=>
	(assert (Inestable ?empresa))
)

; ENTRADA: Si hay una noticia negativa sobre la economía (Ibex)
; SALIDA: todos los valores pasan a ser inestables durante 2 días
(defrule NoticiaNegativaEconomia
	(Noticia (Sobre ?empresa) (Tipo Mala))
	(AnalisisSector (Nombre Ibex))
	=>
	(assert (Inestable ?empresa))
)



; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
;									MODULO 1
; 							Detector de valores peligrosos
; —----------------------------------------------------------------------------
; -----------------------------------------------------------------------------

; ENTRADA: Empresas con tales caracteríscticas
; SALIDA: son valores peligrosos

; Solo aplicable a los valores de los que el usuario posee acciones
; --> (Cartera_Acciones (Nombre ?empresa))

; Si un valor es inestable y está perdiendo de forma continua durante 
; los últimos 3 dias es peligroso
; ENTRADA: Empresa de mi cartera que sea inestable y esté perdiendo 3 días seguidos
; SALIDA: Esa empresa es peligrosa.
(defrule ValorPeligroso3dias
	(Cartera_Acciones (Nombre ?empresa))
	(Inestable ?empresa)
	(AnalisisEmpresa (Nombre ?empresa) (Perd3consec true))
	=>
	(assert (Peligroso ?empresa))
)

; Si un valor está perdiendo durante los últimos 5 dias y la variación 
; en esos 5 días con respecto a la variación del sector es mayor de un -5%, 
; ese valor es peligroso
; ENTRADA: Empresa de mi cartera que sea inestable y esté perdiendo 5 días seguidos
; y tenga una variación con su sector de más de un -5% 
; SALIDA: Esa empresa es peligrosa.
(defrule ValorPeligroso5dias
	(Cartera_Acciones (Nombre ?empresa))
	(Inestable ?empresa)
	(AnalisisEmpresa (Nombre ?empresa) (Perd3consec true) 
						(Perd5consec true) (VarRespSector5dias ?s))
	(test(> ?s -5))
	=>
	(assert (Peligroso ?empresa))
)


; Una vez terminadas las reglas para detectar si un valor es peligrosos debemos 
; empezar un módulo nuevo, en este caso comenzaremos el modulo para dectectar si
; los valores están sobrevalorados  
(defrule PasamosModuloSobrevalorado
	=>
	(assert (modulo_sobrevalorado))
)



; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
;									MODULO 2
; 							Detector de valores sobrevalorados
; —----------------------------------------------------------------------------
; -----------------------------------------------------------------------------

; ENTRADA: Empresas que cumplan tales condiciones
; SALIDA: son empresas sobrevaloradas

; Caso General
; ENTRADA: Si el PER es Alto y el RPD Bajo de la empresa 
; SALIDA: la empresa está sobrevalorada
(defrule SobrevaloradoGeneral
	(modulo_sobrevalorado)
	(AnalisisEmpresa (Nombre ?empresa) (EtiqPER Alto) (EtiqRPD Bajo))
	=>
	(assert (Sobrevalorada ?empresa))
)

; Caso Empresa Pequeña
; ENTRADA: Si el PER de la empresa es Alto 
; SALIDA: entonces la empresa está sobrevalorada
(defrule SobrevaloradoEmpresaPequenio1
	(modulo_sobrevalorado)
	(AnalisisEmpresa (Nombre ?empresa) (Tamanio PEQUENIA) (EtiqPER Alto))
	=>
	(assert (Sobrevalorada ?empresa))
)

; Caso Empresa Pequeña: 
; ENTRADA: Si el PER es Medio y el RPD es Bajo 
; SALIDA: la empresa está sobrevalorada
(defrule SobrevaloradoEmpresaPequenio2	
	(modulo_sobrevalorado)
	(AnalisisEmpresa (Nombre ?empresa) (Tamanio PEQUENIA) (EtiqPER Medio) 
						(EtiqRPD Bajo))
	=>
	(assert (Sobrevalorada ?empresa))
)

; Caso Empresa Grande
; ENTRADA: Si el RPD es Bajo y el PER es Medio o Alto 
; SALIDA: la empresa está sobrevalorada
(defrule SobrevaloradoEmpresaGrande1
	(modulo_sobrevalorado)
	(AnalisisEmpresa (Nombre ?empresa) (Tamanio GRANDE) (EtiqPER Medio|Alto) 
					(EtiqRPD Bajo)) 
	=>
	(assert (Sobrevalorada ?empresa))
)

; Caso Empresa Grande
; ENTRADA: Si el RPD es Mediano y el PER es Alto 
; SALIDA: la empresa está sobrevalorada
(defrule SobrevaloradoEmpresaGrande2
	(AnalisisEmpresa (Nombre ?empresa) (Tamanio GRANDE) (EtiqPER Alto) 
						(EtiqRPD Medio)) 
	(Modulo Sobrevalorado)
	=>
	(assert (Sobrevalorada ?empresa))
)


; Una vez terminado este modulo, debemos cerrar el módulo y pasar al siguiente
(defrule CerrarModuloSobrevalorados
  (declare (salience -1000))
  ?f <- (modulo_sobrevalorado)
  =>
  (retract ?f)
  (assert (modulo_infravalorado))
)



;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;;									MODULO 3
;;							Detector de valores infravalorados
;;—----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------

; ENTRADA: Empresas que cumplan tales condiciones
; SALIDA: son empresas infravaloradas

; ENTRADA: Si el PER es Bajo y el RPD Alto
; SALIDA: la empresa está infravalorada
(defrule InfravaloradoGeneral
	(modulo_infravalorado)
	(AnalisisEmpresa (Nombre ?empresa) (EtiqPER Bajo) (EtiqRPD Alto)) 
	=>
	(assert (Infravalorada ?empresa))
)

; Si  la empresa ha caído bastante (más de un 30%) (en los últimos 3, 6 o 12 ), 
; ha subido pero no mucho en el último mes, y el PER es bajo, la  empresa está 
; infravalorada

; Cuando habla de que ha subido pero no mucho en el último mes, según el experto 
; podemos situar el valor entre un 5%-10%, escogeré un 5%

; Dividiré en tres reglas distintas si la empresa ha caido en los últimos 3 meses, 
; en los últimos 6 meses o en los últimos 12 meses

; Si  la empresa ha caído bastante (más de un 30%) en los últimos 3 meses, ha 
; subido pero no mucho en el último mes, y el PER es bajo, 
; SALIDA: la  empresa está infravalorada
(defrule InfravaloradoCaida3meses
	(modulo_infravalorado)
	(AnalisisEmpresa (Nombre ?empresa) (EtiqPER Bajo) (VarMen ?mes) (VarTri ?tri))
	(test (< ?tri -30))
	(test(< ?mes 5))
  	(test(> ?mes 0))
	=>
	(assert (Infravalorada ?empresa))
)

; Si  la empresa ha caído bastante (más de un 30%) en los últimos 6 meses, ha 
; subido pero no mucho en el último mes, y el PER es bajo, 
; SALIDA: la empresa está infravalorada
(defrule InfravaloradoCaida6meses
	(modulo_infravalorado)
	(AnalisisEmpresa (Nombre ?empresa) (EtiqPER Bajo) (VarMen ?mes) (VarSem ?sem))
	(test (< ?sem -30))
	(test(< ?mes 5))
  	(test(> ?mes 0))
	=>
	(assert (Infravalorada ?empresa))
)

; Si  la empresa ha caído bastante (más de un 30%) en los últimos 12 meses, 
; ha subido pero no mucho en el último mes, y el PER es bajo, 
; SALIDA: la empresa está infravalorada
(defrule InfravaloradoCaida12meses
	(modulo_infravalorado)
	(AnalisisEmpresa (Nombre ?empresa) (EtiqPER Bajo) (VarMen ?mes) 
						(Var12mes ?anio))
	(test (< ?anio -30))
	(test(< ?mes 5))
  	(test(> ?mes 0))
	=>
	(assert (Infravalorada ?empresa))
)


; Una vez terminado este modulo, debemos cerrar el módulo y pasar al siguiente
(defrule CerrarModuloInfravalorado
  (declare (salience -1000))
  ?f <- (modulo_infravalorado)
  =>
  (retract ?f)
  (assert (modulo_propuestas))
)



; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
;									MODULO 4
;							Realización de Propuestas
; —----------------------------------------------------------------------------
; -----------------------------------------------------------------------------

; (RE "rendimiento esperado" = revalorización anual esperada + 
;									dividendos anuales esperado)

; -----------------------------------------------------------------------------
; Módulo 4.1: Obtener posibles propuestas (junto con el rendimiento esperado)
; -----------------------------------------------------------------------------

; Nos creamos un deftemplate donde guardaremos las propuestas
(deftemplate Propuesta
	(slot Operacion) (slot Empresa1) (slot Empresa2) (slot RE) (slot Explicacion)
)

; Nos creamos un deftemplate donde guardamos las posibles acciones a realizar 
; a partir de las propuestas
(deftemplate AccionesPosibles
    (slot Operacion) (slot Empresa1) (slot Empresa2) (slot RE) (slot Explicacion)
)

; Proponer vender valores de empresas peligrosas
; -----------------------------------------------------------------------------

; ENTRADA: Si una empresa es peligrosa, ha bajado el último mes y ha bajado más de 
; un 3% con respecto a su sector en el último mes 
; SALIDA: proponer vender las acciones de la empresa
; RE = 20-rpd

(defrule VenderValoresEmpresasPeligrosas
	(modulo_propuestas)
	(Peligroso ?empresa)
	(Cartera_Acciones (Nombre ?empresa))
	(AnalisisEmpresa (Nombre ?empresa) (Sector ?s) (VarMen ?p) (RPD ?rpd) 
						(VRS5 ?v))
  	(test (< ?v -3))
	=>
	(bind ?re (- 20 ?rpd))
	(assert (AccionesPosibles (Operacion VenderPeligroso)
				(Empresa1 ?empresa) (RE ?re)
				(Explicacion (str-cat "La empresa es peligrosa. Además está "
					"entrando en tendencia bajista con respecto a su sector. "
					"Según mi estimación existe una probabilidad no despreciable"
					" de que pueda caer al cabo del año un 20%, aunque produzca "
                    ?rpd "% por dividendos perderíamos un " ?re "%."))))
)

; Proponer invertir en empresas infravaloradas
; -----------------------------------------------------------------------------

; ENTRADA: Si empresa está infravalorada y el usuario tiene dinero para invertir 
; SALIDA: proponer invertir dinero en  las acciones de la empresa. 
; RE = (PERMedio-PER)*100/(5*PER) + RPD 

(defrule InvertirEmpresasInfravaloradas
	(modulo_propuestas)
	(Infravalorada ?empresa)
	(Cartera_Acciones (Nombre DISPONIBLE) (Acciones ?dinero) (ValorActual ?v))
	(AnalisisEmpresa (Nombre ?empresa) (Precio ?p) (Sector ?s) (PER ?per) 
						(RPD ?rpd))
	(AnalisisSector (Nombre ?s) (PER ?permedio))
	(test (> ?v ?p))
	(test (neq ?per 0))
	=>
	(bind ?re (+ (/ (* 100 (- ?permedio ?per)) (* 5 ?per)) ?rpd))
	(assert (AccionesPosibles (Operacion INVERTIR) (Empresa1 ?empresa) 
				(RE ?re)
				(Explicacion (str-cat "Esta empresa está infravalorada "
	                "y seguramente el PER tienda al PER medio en 5 años, "
	                "con lo que se debería revalorizar un " ?re
	                " anual a lo que habría que sumar el " ?rpd
	                " de beneficios por dividendos.")))) 
)

; Proponer vender valores de empresas sobrevaloradas 
; -----------------------------------------------------------------------------

; ENTRADA: Si empresa de mi cartera está sobrevalorada y
; el rendimiento por año < 5 + precio dinero, 
; SALIDA: proponer vender las acciones de esa empresa; 
; RE = -RPD+(PER-PERMedioSector)/(5*PER)

(defrule VenderValoresSobrevalorados
	(modulo_propuestas)
	(Sobrevalorada ?empresa)
	(Cartera_Acciones (Nombre ?empresa))
	(AnalisisEmpresa (Nombre ?empresa)(Precio ?p) (Sector ?s) (PER ?per) (RPD ?rpd) 
			(Var12mes ?var))
	(AnalisisSector (Nombre ?s) (PER ?permedio))
	(test (< ?var (+ ?p 5)))
	=>
	(bind ?re (+ (* -1 ?rpd) (/ (- ?per ?permedio) (* 5 ?per))))
	(assert (AccionesPosibles (Operacion VenderSobrevalorado) (Empresa1 ?empresa) 
				(RE ?re) 
				(Explicacion (str-cat "Esta empresa está sobrevalorada, "
					"es mejor amortizar lo invertido, ya que seguramente el "
					"PER tan alto deberá bajar al PER medio del sector en "
					"unos 5 años, con lo que se devería devaluar un " ?re
                    " anual, así que aunque se pierda el " ?rpd
                    " de beneficios por dividendos saldría rentable."))))
)

; Proponer cambiar una inversión a valores más rentables
; -----------------------------------------------------------------------------

; Si una empresa (empresa1) no está sobrevalorada y su RPD es mayor que el 
; (revalorización por año esperado + RPD+ 1) de una empresa de mi cartera (empresa2); que no está infravalorada
; SALIDA: proponer cambiar las acciones de una empresa por las de la otra, 
; RE= (RPD empresa1 - (rendimiento por año obtenido  empresa2 + rdp empresa2 +1)

(defrule CambiarInversionMasRentable
        (modulo_propuestas)
        (AnalisisEmpresa (Nombre ?empresa1) (RPD ?rpd1))
		(Sobrevalorada ~?empresa1)
		(Cartera_Acciones (Nombre ?empresa2))
		(AnalisisEmpresa (Nombre ?empresa2) (RPD ?rpd2) 
							(Var12mes ?revalorizacion))
		(Infravalorada ~?empresa2)
		(test (neq ?empresa1 ?empresa2))
		(test (> ?rpd1 (+ ?rpd2 1 ?revalorizacion)))
        =>
        (bind ?re (- ?rpd1 (+ ?revalorizacion ?rpd2)))
        (assert (AccionesPosibles (Operacion CAMBIAR) (Empresa1 ?empresa1)
            		(Empresa2 ?empresa2) (RE ?re)
               		(Explicacion (str-cat ?empresa1
                        " debe tener una revalorización acorde con la evolución "
                        "de la bolsa. Por dividendos se espera un " ?rpd1
                        ", que es más que lo que te está dando " ?empresa2
                        ", por eso te propongo cambiar los valores por los de "
                        "esta otra empresa. Aunque se pague el 1% del coste del cambio te saldría rentable."))))
)


; Una vez terminado este modulo, debemos cerrar el módulo y pasar al siguiente
; que será el módulo de la interación con el usuario --> (modulo_usuario)
(defrule CerrarModuloPropuestas
    (declare (salience -1000))
    ?f<-(modulo_propuestas)
    =>
    (retract ?f)
    (assert (modulo_usuario1))
)

; -----------------------------------------------------------------------------
; Módulo 4.2: Módulo de realización de propuestas al usuario
; -----------------------------------------------------------------------------

; Nos creamos un deftemplate donde definiremos el resultado de la pregunta
(deftemplate Propuesta_Preguntas
	(slot Operacion) (slot Empresa) (slot Acciones))

; Inicializamos un constructor donde le asignamos un valor al coste maximo
(deffacts mayor (coste_maximo 0))

; Nos creamos un contador para
(deffacts contador (cont 0))

; Calculamos el máximo RE
;(defrule Maximo
;   (modulo_usuario1)
;	(Propuesta (Empresa1 ?empresa1) (RE ?re1))
;  	(not (and (Propuesta (Empresa1 ?empresa2) (RE ?re2)) 
;  	(test(> ?re2 ?re1))))
;  	=>
;	(assert (MejorPropuesta ?empresa1 ?re1))
;)

(defrule Maximo
    (modulo_usuario1)
    (AccionesPosibles (Empresa1 ?empresa1) (RE ?re1))
    ?maximo <- (coste_maximo ?re2)
    (test (> ?re1 ?re2))
    =>
    (assert (coste_maximo ?re1))
    (retract ?maximo)
)

; Elegimos de entre todos el máximo y mostramos las 5 propuestas
(defrule Elegir
    (modulo_usuario1)
    ?elegir <- (AccionesPosibles (Operacion ?accion) (Empresa1 ?empresa1)
    			(Empresa2 ?empresa2) (RE ?re))
    ?maximo <- (coste_maximo ?re)
    ?contador <- (cont ?cont)
    (test(< ?cont 5))
    =>
    (assert (Propuesta (Operacion ?accion) (Empresa1 ?empresa1)
        			(Empresa2 ?empresa2) (RE ?re)))
    (retract ?elegir)
    (retract ?maximo)
    (retract ?contador)
    (assert (coste_maximo 0))
    (assert (cont (+ ?cont 1)))
)

; Una vez terminado este modulo, dentro del mismo modulo, debemos cerrar el módulo 
; y pasar al siguiente que será otro módulo de la interación 
; con el usuario --> (modulo_usuario2)
(defrule CerrarModuloUsuario1
    (declare (salience -1000))
    (cont ?cont)
    (test (>= ?cont 5))
    ?f <- (modulo_usuario1)
    =>
    (retract ?f)
    (assert (modulo_usuario2))
)
    
; Una vez clasificadas las propuestas, pasamos a clasificar las propuestas entre
; las 4 posibles operaciones a realizar

; Si elegimos la operación de vender valores peligrosos
(defrule ModificarVenderPeligrosos
    (modulo_usuario2)
    (Cartera_Acciones (Nombre ?empresa))
    (Propuesta (Operacion VenderPeligroso) (Empresa1 ?empresa) (RE ?re))
    =>
    (printout t crlf "Propuesta para vender valor peligroso de la empresa " 
    					?empresa " con RE = " ?re)
)

; Si elegimos la operación de vender valores sobrevalorados
(defrule ModificarVenderSobrevaloradas
    (modulo_usuario2)
    (Cartera_Acciones (Nombre ?empresa1))
    (Propuesta (Operacion VenderSobrevalorado) (Empresa1 ?empresa1) (RE ?re))
    =>
    (printout t crlf "Propuesta para vender valor sobrevalorado de la "	
    					"empresa " ?empresa1 " con RE = " ?re)
)

; Si elige invertir en valores infravalorados
(defrule ModificarInvertirMasRentable
    (modulo_usuario2)
    (Propuesta (Operacion INVERTIR) (Empresa1 ?empresa) (RE ?re))
    =>
    (printout t crlf "Propuesta para invertir en valores mas rentables "
    					"de la empresa " ?empresa " con RE = " ?re crlf)
)

; Si elige cambiar en valores más rentables
(defrule ModificarCambiarInversion
    (modulo_usuario2)
    (Cartera_Acciones (Nombre ?empresa2))
    (Propuesta (Operacion CAMBIAR) (Empresa1 ?empresa1) (Empresa2 ?empresa2)
                (RE ?re))
    =>
    (printout t crlf "Propuesta para cambiar las acciones de la " 
    					"empresa " ?empresa2 " por la empresa " ?empresa1 
    					" con RE = " ?re)
)

; Una vez cerrado el modulo usuario 2, nos vamos a realizar la interacción 
; con el usuario, otro módulo dentro del mismo
(defrule CerrarModuloUsuario2
    (declare (salience -1000))
    ?f<-(modulo_usuario2)
    =>
    (retract ?f)
    (assert (modulo_interaccion))
)

; Con esta regla comenzamos la interfaz que verá el usuario
(defrule CabeceraInterfaz
	=>
    (printout t crlf " - Bienvenido al Sistema Experto que le asesorará a "
    	" invertir en bolsa - " crlf)
)

; Pasamos ahora a realizar la interaccion con el usuario, realizando preguntas
(defrule UsuarioInteraccion
    (declare (salience -100))
    ?f <- (modulo_usuario2)
    =>
    (retract ?f)

    (printout t crlf crlf "Introduzca la operacion que quiere realizar: " crlf
    		"	- Invertir: invertir en empresas infravaloradas " crlf
    		"	- Cambiar: cambiar una empresa a otra por valores + rentables " crlf
    		"	- VenderPeligrosos: vender valores peligrosos " crlf
    		"	- VenderSobrevalorado: vender valores sobrevalorados " crlf)
    (bind ?Resposta1 (read))

    (printout t crlf "Introducir empresa con la que realizar operacion " 
    					?Resposta1 crlf)
    (bind ?Resposta2 (read))   
 
    (printout t crlf "¿Cuantas acciones de la empresa " ?Resposta2 " quiere " 
    				?Resposta1 "?" crlf)
    (bind ?Resposta3 (read))

    (assert (Propuesta_Preguntas (Operacion (upcase ?Resposta1)) 
    			(Empresa(upcase ?Resposta2)) (Acciones ?Resposta3)))

   	(printout t crlf "OPERACION REALIZADA CON EXITO" crlf)

    (printout t crlf "¿Desea seguir realizando operaciones? Escriba Seguir, sino"
    	 " pulsa cualquier otra letra. ¿Qué decide? ")
  		(bind ?Salir (read))

  	(if (or (eq ?Salir Seguir) (eq ?Salir seguir)) then
		(assert (modulo_interaccion)))
)

; Cuando se ha elegido vender valores peligrosos, debemos actualizar nuestra 
; cartera, eliminando las acciones que hayamos vendido de nuestra cartera
; de acciones de la empresa específica
(defrule ElegirVentaAccionesPeligrosos
	?m <- (modulo_interaccion)
    ?p <- (Propuesta_Preguntas (Operacion VenderPeligrosos) (Empresa ?empresa) 
    						(Acciones ?acciones_propuestas))
    ?f <- (Cartera_Acciones (Nombre ?empresa) (Acciones ?acciones))
    ?h <- (Cartera_Acciones (Nombre DISPONIBLE) (Acciones ?dinero))
    (AnalisisEmpresa (Nombre ?empresa) (Precio ?coste))
    (test (> ?acciones_propuestas 0))
    (test (>= ?acciones ?acciones_propuestas))
    =>
    (retract ?p)
    (retract ?m)
    (modify ?h (Acciones (+ ?dinero (* ?acciones_propuestas ?coste))))
    (modify ?f (Acciones (- ?acciones ?acciones_propuestas)) 
    			(ValorActual (* (- ?acciones ?acciones_propuestas) ?coste)))
    (printout t crlf crlf "Ha realizado la venta de " ?acciones_propuestas 
     	" acciones de la empresa " ?empresa crlf)
    (assert (modulo_propuestas))
)

; Cuando se ha elegido vender valores sobrevalorados, debemos actualizar nuestra 
; cartera, eliminando las acciones que hayamos comprado de nuestra cartera
; de acciones de la empresa específica
(defrule ElegirVentaSobrevalorados
    ?m <- (modulo_interaccion)
    ?p <- (Propuesta_Preguntas (Operacion VenderSobrevalorado) (Empresa ?empresa) 
    				(Acciones ?acciones_propuestas))
    ?f <- (Cartera_Acciones (Nombre ?empresa) (Acciones ?acciones))
    ?h <- (Cartera_Acciones (Nombre DISPONIBLE) (Acciones ?dinero))
    (AnalisisEmpresa  (Nombre ?empresa) (Precio ?coste))
    (test (> ?acciones_propuestas 0))
    (test (>= ?acciones ?acciones_propuestas))
    =>
    (retract ?p)
    (retract ?m)
    (modify ?h (Acciones (+ ?dinero (* ?acciones_propuestas ?coste))))
    (modify ?f (Acciones (- ?acciones ?acciones_propuestas)) 
    			(ValorActual (* (- ?acciones ?acciones_propuestas) ?coste)))
   	(printout t crlf crlf "Ha realizado la venta de " ?acciones_propuestas 
     			" acciones de la empresa " ?empresa crlf)		
    (assert (modulo_propuestas))
)

; Cuando se ha elegido comprar valores infravalorados, debemos actualizar nuestra 
; cartera, sumando las acciones que hayamos comprado de la empresa específica
(defrule ElegirComprarValoresInfravalorados
	?m <- (modulo_interaccion)
    ?p <- (Propuesta_Preguntas (Operacion INVERTIR)(Empresa ?empresa) 
    		(Acciones ?acciones_propuestas))
    ?f <- (Cartera_Acciones (Nombre DISPONIBLE) (Acciones ?dinero))
    (AnalisisEmpresa (Nombre ?empresa) (Precio ?coste))
    (test (>= ?dinero (* ?acciones_propuestas ?coste)))
    (test (> ?acciones_propuestas 0))
    =>
    (retract ?p)
   	(retract ?m)
    (modify ?f (Acciones (- ?dinero (* ?acciones_propuestas ?coste))))
    (assert (Cartera_Acciones (Nombre ?empresa) (Acciones ?acciones_propuestas)
    			(ValorActual (* ?acciones_propuestas ?coste))))
    (printout t crlf crlf "Ha comprado " ?acciones_propuestas 
    		" acciones de la empresa " ?empresa crlf)
    (assert (modulo_propuestas))
)

; Al elegir cambiar: cambiamos la inversión de empresa 2 por empresa 1
(defrule ElegirCambiarInversiones
    ?m <- (modulo_interaccion)
    ?a <- (Propuesta_Preguntas (Operacion CAMBIAR) (Empresa ?empresa1) 
    		(Acciones ?acciones_propuestas))
    (AnalisisEmpresa (Nombre ?empresa1))
    (AnalisisEmpresa (Nombre ?empresa2))
    (Propuesta (Empresa1 ?empresa1) (Empresa2 ?empresa2))
    (Cartera_Acciones (Nombre DISPONIBLE) (Acciones ?acciones))
    (Cartera_Acciones (Nombre ?empresa2) (Acciones ?acciones1))
    ?f <- (Cartera_Acciones (Nombre ?empresa2) (Acciones ?acciones2))
    =>
    (retract ?m)
    (retract ?a)
    (retract ?f)
    (printout t crlf crlf "Ha cambiado las acciones de la " ?empresa2 
    		" por las acciones de la empresa " ?empresa1 crlf)
    (assert (Cartera_Acciones (Nombre ?empresa1) (Acciones acciones2)))
  	(assert (modulo_propuestas))
)

; Una vez hemos terminado y el inversor desea seguir con las operaciones, 
; debemos volver al módulo 4.1, para recalcular las nuevas propuestas
(defrule Recalcular
	?f <- (modulo_interaccion)
  	(cont 0)
  	=>
  	(printout t crlf "Loading... Recalculando propuestas.")
  	(retract ?f)
  	(assert (modulo_propuestas))
)

; Cerramos definitivamente el modulo
(defrule CerrarModuloUltimo
	(declare (salience -10000))
  	?f <- (modulo_interaccion)
  	=>
  	(retract ?f)
)