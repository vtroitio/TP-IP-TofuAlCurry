-- Completar con los datos del grupo
--
-- Nombre de Grupo: TofuAlCurry
-- Integrante 1: Valentin Troitiño, valiktroi16@gmail.com, 709/23
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
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

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos l1 l2 = perteneceL1 l1 l2 && perteneceL2 l1 l2
    where perteneceL1 [] _ = True
          perteneceL1 (x:xs) ys = pertenece x ys && perteneceL1 xs ys
          perteneceL2 _ [] = True
          perteneceL2 xs (y:ys) = pertenece y xs && perteneceL2 xs ys

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

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios = undefined

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
