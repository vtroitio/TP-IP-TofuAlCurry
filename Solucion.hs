-- Nombre de Grupo: TofuAlCurry
-- Integrante 1: Valentin Troitiño, valiktroi16@gmail.com, 709/23
-- Integrante 2: Josefina Negrotto, josefinanegrotto@gmail.com, 545/23
-- Integrante 3: Facundo Chenlo, chenlofacundo@gmail.com, 335/23
-- Integrante 4: Tobias Oshiro, tobiasoshiro@gmail.com, 852/23

module Solucion where

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Predicados

-- Dado un elemento y una lista, determina si ese elemento pertenece a la lista o no
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

-- Dadas dos listas determina si tienen los mismos elementos o no
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos l1 l2 = listaIncluida l1 l2 && listaIncluida l2 l1

-- Verifica si una lista esta incluida dentro de otra, manteniendo las repeticiones si las hay
listaIncluida :: (Eq t) => [t] -> [t] -> Bool
listaIncluida [] _ = True
listaIncluida (x:xs) ys = pertenece x ys && listaIncluida xs (quitarPrimeraAparicion x ys)

-- Verifica si una lista de usuarios de una red es una cadena de amigos
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] _ = True
cadenaDeAmigos [u] _ = True
cadenaDeAmigos (u1:u2:us) red = relacionadosDirecto u1 u2 red && cadenaDeAmigos (u2:us) red
    where
        -- Verifica si dos usuarios de la red son amigos
        relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
        relacionadosDirecto u1 u2 red = pertenece (u1,u2) (relaciones red) || pertenece (u2,u1) (relaciones red)

-- Verifica si una lista de usuarios pertenecea a cierta red
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed _ [] = True
sonDeLaRed red (u:us) = pertenece u (usuarios red) && sonDeLaRed red us

empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon e l = head l == e

terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon e l = last l == e

-- Dada una lista determina si tiene repeticiones o no
sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs)
    | not (pertenece x xs)  = sinRepetidos xs
    | otherwise             = False

-- Auxiliares genericas
longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs


-- Dada una lista y un elemento, devuelve la lista sin la primera aparicion de dicho elemento
quitarPrimeraAparicion :: (Eq t) => t -> [t] -> [t]
quitarPrimeraAparicion _ [] = []
quitarPrimeraAparicion e (x:xs)
    | e == x        = xs
    | otherwise     = (x:quitarPrimeraAparicion e xs)

-- Ejercicios

-- Ejercicio 1
-- Dada una red social, devuelve una lista de nombres de usuarios sin repetidos
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)
    where
        -- Dada una lista de usuarios devuelve sus nombres sin repetidos
        proyectarNombres :: [Usuario] -> [String]
        proyectarNombres []  = []
        proyectarNombres [u] = [nombreDeUsuario u]
        proyectarNombres (u:us)
            | not (pertenece nombre (proyectarNombres us)) = (nombre: proyectarNombres us)
            | otherwise            = proyectarNombres us
            where nombre = nombreDeUsuario u

-- Ejercicio 2
-- Dada una red y un usuario, devuelve una lista de usuarios que son amigos del usuario de entrada en esa red.
-- Dicha lista de salida no tiene repetidos
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = amigosDeConRelaciones  (relaciones red) u
    where
        amigosDeConRelaciones :: [Relacion] -> Usuario -> [Usuario]
        amigosDeConRelaciones [] _ = []
        amigosDeConRelaciones (rel:rels) u
            | u == fst rel  = (snd rel : amigosDeConRelaciones rels u)
            | u == snd rel  = (fst rel : amigosDeConRelaciones rels u)
            | otherwise     = amigosDeConRelaciones rels u

-- Ejercicio 3
-- Dada una red social y un usuario devuelve cuantos amigos tiene ese usuario en esa red
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)

-- Ejercicio 4
-- Compara la cantidad de amigos de todos los usuarios en la red social y devuelve el que tiene más amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = comparaCantidadDeAmigosDeUsuarios red (usuarios red) (head (usuarios red))
    where 
          comparaCantidadDeAmigosDeUsuarios :: RedSocial -> [Usuario] -> Usuario -> Usuario
          comparaCantidadDeAmigosDeUsuarios _ [] u = u
          comparaCantidadDeAmigosDeUsuarios red us u
            | cantidadDeAmigos red (head us) <= cantidadDeAmigos red u = comparaCantidadDeAmigosDeUsuarios red (tail us) u
            | otherwise = comparaCantidadDeAmigosDeUsuarios red (tail us) (head us)

-- Ejercicio 5
-- Verifica si hay algún usuario en la red social con más de 1.000.000 (10 en realidad) de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],_,_) = False
estaRobertoCarlos red = longitud (amigosDe red (usuarioConMasAmigos red)) > 10


-- Ejercicio 6
-- Dada una red y un usuario devuelve las publicaciones de dicho usuario
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = publicacionesDeUsuario (publicaciones red) u
    where 
          publicacionesDeUsuario :: [Publicacion] -> Usuario -> [Publicacion]
          publicacionesDeUsuario [] _ = []
          publicacionesDeUsuario (pub:pubs) u
            | usuarioDePublicacion pub == u = pub:(publicacionesDeUsuario pubs u)
            | otherwise = publicacionesDeUsuario pubs u


-- Ejercicio 7
-- Dada una red y un usuario devuelve todas las publicaciones, de dicha red, que le gustan a dicho usuario
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = publicacionesQueLeGustanAlUsuario (publicaciones red) u -- verifica likes tiene nombre que pinta booleano
    where 
          publicacionesQueLeGustanAlUsuario :: [Publicacion] -> Usuario -> [Publicacion]
          publicacionesQueLeGustanAlUsuario [] _ = []
          publicacionesQueLeGustanAlUsuario (pub:pubs) u
            | pertenece u (likesDePublicacion pub) = pub:publicacionesQueLeGustanAlUsuario pubs u
            | otherwise = publicacionesQueLeGustanAlUsuario pubs u

-- Ejercicio 8
-- Dada una red y dos usuarios, verifica si a los dos usuarios le gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)



-- Ejercicio 9
-- Dada una red y un usuario, verifica si todas las plublicaciones de ese usuario fueron likeadas, al menos, por una misma persona
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = verificaSeguidorFiel red (quitarPrimeraAparicion u (usuarios red)) (publicacionesDe red u)
    where
        verificaSeguidorFiel :: RedSocial -> [Usuario] -> [Publicacion] -> Bool
        verificaSeguidorFiel _ [] _ = False
        verificaSeguidorFiel _ _ [] = False
        verificaSeguidorFiel red (u:us) pubs
            | listaIncluida pubs (publicacionesQueLeGustanA red u) = True
            | otherwise = verificaSeguidorFiel red us pubs


-- Ejercicio 10
-- Verifica que (en caso que exitsa) la secuenciaDeAmigos encontrada sea de la Red y una cadenaDeAmigo
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos ([],[],[]) _ _ = False
existeSecuenciaDeAmigos red u1 u2
    | secuenciaDeAmigos red u1 u2 (amigosDe red u1) [u1] == [] = False
    | longitud (secuenciaDeAmigos red u1 u2 (amigosDe red u1) [u1]) > 0 && sonDeLaRed red (secuenciaDeAmigos red u1 u2 (amigosDe red u1) [u1]) && cadenaDeAmigos (secuenciaDeAmigos red u1 u2 (amigosDe red u1) [u1]) red = True

-- Busca una secuencia de Amigos entre dos usuarios: partir del usuario base, busca dentro
-- de sus amistades relaciones hasta llegar con el segundo usuario
-- red usuario1 usuario2 [amigos de usuario 1] 
secuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> [Usuario] -> [Usuario] -> [Usuario]
secuenciaDeAmigos (_,[],_) _ _ _ _ = []
secuenciaDeAmigos _ _ _ [] _ = []
secuenciaDeAmigos red u1 u2 (u:us) (x:xs)
    | (u:us) == [] = []
    | u1 == u2 && (u:us) /= [] = [u1]
    | pertenece u2 (u:us)          = [u2]
    | not (sinRepetidos (x:xs))    = secuenciaDeAmigos red u1 u2 (amigosDe red (last (init xs))) (x:(init xs))
    | pertenece u1 (u:us)          = secuenciaDeAmigos red u1 u2 (quitarPrimeraAparicion u1 (u:us)) (x:xs)
    | pertenece u (amigosDe red u) = secuenciaDeAmigos red u1 u2 (quitarPrimeraAparicion u (amigosDe red u)) (x:xs)
    | otherwise                    = u:(secuenciaDeAmigos red u1 u2 (amigosDe red u) (u:(x:xs)))
