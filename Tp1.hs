-- TP N*1 Grupo 4

-- Integrantes:
--     Agurto Luis
--     Costa Carla
--     Davyt M.Daniela
--     Fiorentino Maximiliano
--     González Nimiatype Edad= Int


--DOMINIO
type Edad= Int
type Felicidonios= Int
type Suenios=Int
type Nombre=String
type Habilidades=[String]
type Persona=(Edad,Suenios,Nombre,Felicidonios,Habilidades)

laura :: Persona
laura=(21,3,"Laura Hernandez",130,["Jugar al futbol","Mecanica","Tocar la bateria"])

roberto :: Persona
roberto=(25,2,"Roberto",101,["Jugar al futbol","Mecanica","Tocar la bateria"])

melina :: Persona
melina=(26,2,"Melina",100,["Jugar al futbol","Mecanica","Tocar la bateria"])

ariel :: Persona
ariel=(18,1,"Ariel",50,["Jugar al futbol","Mecanica","Tocar la bateria"])

evangelina :: Persona
evangelina=(18,1,"Evangelina",14,["Jugar al futbol","Mecanica","Tocar la bateria"])

maximiliano :: Persona
maximiliano=(18,1,"Maximiliano",12,["Jugar al futbol","Mecanica","Tocar la bateria"])

-- PUNTO 1 

-- Punto a: Coeficiente de satisfacción
--      Saber el coeficiente de satisfacción de una persona
--           ●	Si los felicidonios son > a 100, son los felicidonios * la edad
--           ●	Si los felicidonios son <= a 100 y > 50, son la cantidad de sueños * los felicidonios
--           ●	En caso contrario, es la división entera de los felicidonios por 2

satisfaccion::(Int,Int,String,Int,[String])->Int
satisfaccion (edad,suenios,_,felicidonios,_)
    | felicidonios > 100 =felicidonios*edad
    | felicidonios>50 && felicidonios<=100 =suenios*felicidonios
    | otherwise=div felicidonios 2

-- PRUEBA ------------------
-- *Main> satisfaccion roberto
-- 2525   
-- *Main> satisfaccion melina
-- 200    
-- *Main> satisfaccion ariel
-- 25 

-- Punto b: Grado de ambición de una persona
--      Saber el grado de ambición de una persona
--           ●	Si los felicidonios son > 100, el grado de ambición son los felicidonios * la cantidad de sueños
--           ●	Si los felicidonios son <= 100 y > 50, será la edad * la cantidad de sueños
--           ●	En caso contrario, serán la cantidad de sueños * 2

ambicion (edad,suenios,_,felicidonios,_)
    | felicidonios>100 =felicidonios*suenios
    | felicidonios>50 && felicidonios<=100= edad*suenios
    | otherwise= suenios*2

-- PRUEBA --------------
-- *Main> ambicion roberto
-- 202    
-- *Main> ambicion melina
-- 52
-- *Main> ambicion ariel
-- 2


-- PUNTO 2

-- ATENCIÓN: Resolver únicamente con Composición y aplicación parcial

-- No se puede utilizar recursividad ni definir funciones auxiliares en ningún paso de este punto.

-- Punto a: Nombre largo
--       Saber si una persona tiene un nombre largo, de más de 10 caracteres.

nombreLargo (_,_,nombre,_,_)=((>10).length) nombre

-- PRUEBA  ------------------
-- *Main> nombreLargo evangelina
-- False
-- *Main> nombreLargo maximiliano
-- True

-- Parte b: Persona suertuda
--       Saber si una persona es suertuda, que como todos sabemos esto se cumple si el triple de su coeficiente de satisfacción es par.

suertuda=even.(*3).satisfaccion

-- PRUEBA  ------------------
-- *Main> suertuda evangelina
-- False
-- *Main> suertuda maximiliano
-- True

-- Parte c: Nombre lindo
--       Saber si una persona tiene un nombre lindo, esto es que su última letra termine en 'a'.

nombreLindo (_,_,nombre,_,_)= ((=='a').last) nombre

-- PRUEBA ----------
-- *Main> nombreLindo ariel
-- False
-- *Main> nombreLindo melina
-- True

-- Punto 3: Los sueños sueños son...  
--       Cada persona tiene sueños que cuando los cumple pasan distintas cosas. Modelar los siguientes sueños:
--              ●	Recibirse de una carrera, esto le da 1000 felicidonios por cada letra de la carrera y le agrega la carrera como habilidad. 
--                  Ej: "arquitectura" le suma 12000 felicidonios.
--              ●	Viajar a una lista de ciudades, suma 100 felicidonios por cada ciudad que visita, en el interín pasa un año (la persona 
--                  tendrá un año más luego de viajar).
--              ●	Enamorarse de otra persona, por lo que suma los felicidonios que esta persona tenga. El sueño no es bidireccional, que X 
--                  se enamore de Y no implica lo mismo para Y

--       Deben implementar también:
--              ●	para los conformistas, el sueño “que todo siga igual”, que mantiene a la persona sin cambios.
--              ●	combo perfecto: se recibe de la carrera de "Medicina", viaja a "Berazategui" y "París" y como bonus extra suma 
--                  100 felicidonios por el combo. Definirlo únicamente con funciones existentes.

recibirse::String->Persona->Persona
recibirse carrera (edad,suenios,nombre,felicidonios,habilidades)=(edad,suenios,nombre,felicidonios+1000,habilidades ++ [carrera])


viajar ciudad (edad,suenios,nombre,felicidonios,habilidades)
    |ciudad=="bonus"=(edad,suenios,nombre,felicidonios+100,habilidades)
    |otherwise=(edad+1,suenios,nombre,felicidonios+100*length(ciudad),habilidades)

enamorarse (a,b,c,d,e) (_,_,_,f,_)= (a,b,c,d+f,e)

comboPerfecto::Persona->Persona
comboPerfecto persona=viajar "bonus" (recibirse "Medicina" (viajar "Paris" (viajar "Berazategui" persona)))

queTodoSigaIgual= id
