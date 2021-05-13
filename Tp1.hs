Probando clonacion y comit sin modificar codigo
type Edad= Int
type Felicidonios= Int
type Suenios=Int
type Nombre=String
type Habilidades=[String]
type Persona=(Edad,Suenios,Nombre,Felicidonios,Habilidades)

laura :: Persona
laura=(21,3,"Laura Hernandez",130,["Jugar al futbol","Mecanica","Tocar la bateria"])

satisfaccion::(Int,Int,String,Int,[String])->Int
satisfaccion (a,b,_,c,_)
    | c > 100 =c*a
    | c>50 && c<=100 =b*c
    | otherwise=div c 2

ambicion (a,b,_,c,_)
    | c>100 =c*b
    | c>50 && c<=100= a*b
    | otherwise= b*2

nombreLargo (_,_,a,_,_)=((>10).length) a

suertuda=even.(*3).satisfaccion

nombreLindo (_,_,a,_,_)= ((=='a').last) a
