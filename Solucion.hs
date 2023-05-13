module Solucion where
-- Completar con los datos del grupo
--
-- Nombre de Grupo: TofuAlCurry
-- Integrante 1: Valentin Troitiño, valiktroi16@gmail.com, 709/23
-- Integrante 2: Josefina Negrotto, josefinanegrotto@gmail.com, 545/23
-- Integrante 3: Facundo Chenlo, 45421244, 335/23
-- Integrante 4: Nombre Apellido, email, LU

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

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs


-- Dadas dos listas determina si tienen los mismos elementos o no
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos2 l1 l2 = listaIncluida l1 l2 && listaIncluida l2 l1
    where
        listaIncluida [] _ = True
        listaIncluida (x:xs) ys = pertenece x ys && listaIncluida xs (quitarPrimeraAparicion x ys)

-- Dada una lista y un elemento, devuelve la lista sin la primera aparicion de dicho elemento
quitarPrimeraAparicion :: (Eq t) => t -> [t] -> [t]
quitarPrimeraAparicion _ [] = []
quitarPrimeraAparicion e (x:xs)
    | e == x        = xs
    | otherwise     = (x:quitarPrimeraAparicion e xs)


cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] _ = True
cadenaDeAmigos [u] _ = True
cadenaDeAmigos (u1:u2:us) red = relacionadosDirecto u1 u2 red && cadenaDeAmigos (u2:us) red
    where relacionadosDirecto u1 u2 red = pertenece (u1,u2) (relaciones red) || pertenece (u2,u1) (relaciones red)

sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed _ [] = True
sonDeLaRed red (u:us) = pertenece u (usuarios red) && sonDeLaRed red us 

empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon e l = head l == e

terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon e l = last l == e

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos [x] = True
sinRepetidos (x:y:ys) = x /= y && sinRepetidos (y:ys)

-- Ejercicios

-- Dada una red social, devuelve una linsta de nombres de usuarios sin repetidos
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres( usuarios red)

-- Dada una lista de usuarios devuelve sus nombres, sin repeticiones
proyectarNombres :: [Usuario] -> [String]
proyectarNombres []  = []
proyectarNombres [u] = [nombreDeUsuario u]
proyectarNombres (u:us)
    | not (pertenece nombre (proyectarNombres us)) = (nombre: proyectarNombres us)
    | otherwise            = proyectarNombres us
    where nombre = nombreDeUsuario u

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- Compara la cantidad de amigos de todos los usuarios en la red social y devuelve el que tiene más amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = comparaCantidadDeAmigosDeUsuarios red (usuarios red) (head (usuarios red))
    where comparaCantidadDeAmigosDeUsuarios _ [] u = u
          comparaCantidadDeAmigosDeUsuarios red us u
            | cantidadDeAmigos red (head us) <= cantidadDeAmigos red u = comparaCantidadDeAmigosDeUsuarios red (tail us) u
            | otherwise = comparaCantidadDeAmigosDeUsuarios red (tail us) (head us)

-- Verifica si hay algún usuario en la red social con más de 1.000.000 de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = verificaCantidadDeAmigosDeUsuarios red (usuarios red)
    where verificaCantidadDeAmigosDeUsuarios _ [] = False
          verificaCantidadDeAmigosDeUsuarios red (u:us) = cantidadDeAmigos red u > 1000000 || verificaCantidadDeAmigosDeUsuarios red us

-- Devuelve las publicaciones de un usuario
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = publicacionesDeUsuario (publicaciones red) u
    where publicacionesDeUsuario [] _ = []
          publicacionesDeUsuario (pub:pubs) u
            | usuarioDePublicacion pub == u = pub:(publicacionesDeUsuario pubs u)
            | otherwise = publicacionesDeUsuario pubs u

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
