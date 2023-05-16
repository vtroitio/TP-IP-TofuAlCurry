import Test.HUnit
import Solucion
--import Data.List

main = runTestTT todosLosTest

-- Cuando nosotros corriamos los casos de test notamos que era
-- mas claro si  lo haciamos de  esta forma asi ya veiamos que
-- funcion en particular era la que tenia problemas.
-- Decidimos poner en los  casos como "Caso n: descripcion" ya
-- que nos parecia que poner "nombreDeLaFuncion n: descripcion"
-- se volveria muy engorroso de leer.
mainConTitulos = do
    -- Ej 1
    putStrLn("Test suite: nombresDeUsuarios")
    runTestTT testsuiteNombresDeUsuarios
    putStrLn("")

    --Ej 2
    putStrLn("Test suite: amigosDe")
    runTestTT testsuiteAmigosDe
    putStrLn("")

    --Ej 3
    putStrLn("Test suite: cantidadDeAmigos")
    runTestTT testsuiteCantidadDeAmigos
    putStrLn("")

    --Ej 4
    putStrLn("Test suite: usuarioConMasAmigos")
    runTestTT testsuiteUsuarioConMasAmigos
    putStrLn("")

    -- Ej 5
    putStrLn("Test suite: estaRobertoCarlos")
    runTestTT testsuiteEstaRobertoCarlos
    putStrLn("")

    -- Ej 6
    putStrLn("Test suite: publicacionesDe")
    runTestTT testsuitePublicacionesDe
    putStrLn("")

    -- Ej 7
    putStrLn("Test suite: publicacionesQueLeGustanA")
    runTestTT testsuitePublicacionesQueLeGustanA
    putStrLn("")

    -- Ej 8
    putStrLn("Test suite: leGustanLasMismasPublicaciones")
    runTestTT testsuiteLesGustanLasMismasPublicaciones
    putStrLn("")

    -- Ej 9
    putStrLn("Test suite: tieneUnSeguidorFiel")
    runTestTT testsuiteTieneUnSeguidorFiel
    putStrLn("")

    -- Ej 10
    putStrLn("Test suite: existeSecuenciaDeAmigos")
    runTestTT testsuiteExisteSecuenciaDeAmigos
    putStrLn("")

todosLosTest = test [
    testsuiteNombresDeUsuarios,
    testsuiteAmigosDe,
    testsuiteCantidadDeAmigos,
    testsuiteUsuarioConMasAmigos,
    testsuiteEstaRobertoCarlos,
    testsuiteTieneUnSeguidorFiel,
    testsuitePublicacionesDe,
    testsuitePublicacionesQueLeGustanA,
    testsuiteLesGustanLasMismasPublicaciones,
    testsuiteExisteSecuenciaDeAmigos
    ]

-- Aclaracion: testsuiteNombresDeUsuarios
-- Si fuera un test de caja negra propiamente dicho deberiamos poner
-- todos   los  test  case  posibles,  incluyendo  las  posibles  de
-- permutaciones de las relaciones y publicaciones.
-- Dado que conocemos que la implementacion hecha no depende de esos
-- posibles casos  vamos a descartarlos, pasando siempre los  mismos
-- valores para dichos parametros.
-- Aclaracion general:
-- Esa va a ser la forma de encarar todos los casos de test para las
-- funciones que se comporten de forma similar para todo el TP.

testsuiteNombresDeUsuarios = test [
    "Caso 1: Lista de usuarios vacia" ~: (nombresDeUsuarios redUsuariosVacia) ~?= [],
    "Caso 2.1: Lista de usuarios sin nombres repetidos" ~: (nombresDeUsuarios redUsuariosSinNombresRepetidos) ~?= ["Facu", "Jose", "Valen", "Tobi", "Cumbio"],
    "Caso 2.2: Lista de usuarios sin nombres repetidos" ~: (mismosElementos (nombresDeUsuarios redUsuariosSinNombresRepetidos) ["Jose", "Facu", "Valen", "Cumbio", "Tobi"] )~?= True,
    "Caso 3.1: Lista de usuarios con nombres repetidos" ~: (nombresDeUsuarios redUsuariosConNombresRepetidos) ~?= ["Tobi","Valen", "Cumbio", "Mati_capo_49"],
    "Caso 3.2: Lista de usuarios con nombres repetidos" ~: (mismosElementos (nombresDeUsuarios redUsuariosConNombresRepetidos) ["Tobi", "Mati_capo_49", "Valen", "Cumbio"]) ~?= True
    ]
-- Para la funcion nombresDeUsuarios notamos que mas de un resultado
-- podia ser correcto. Esto se debe a  que cualquier permutacion  de
-- una lista que contenga todos los nombres de usuario, sin repetir,
-- era correcta. Esto se puede  implementar en el  testeo usando  la
-- funcion expectAny(en el archivo de la catedra)  y alguna  funcion
-- que  devuelva todas  las permutaciones  posibles de  los  nombres
-- correctos,  por  ejemplo permutations  de Data.List.  Esto no  lo
-- implementamos debido a que no se podian meter librerias nuevas.
-- Lo que si hicimos fue comparar si la lista de la funcion y la que
-- se esperaba(en otro orden) tenian los mismos elementos.


-- Para estos casos de test las publicaciones no son importantes
testsuiteAmigosDe = test [
    "Caso 1: Usuario sin amigos" ~: (amigosDe redUsuarioXSinAmigos usuario9) ~?= [],
    "Caso 2: Red sin relaciones" ~: (amigosDe redRelacionesVacias usuario1) ~?= [],
    "Caso 3: Usuario con relaciones de distinto ordenamiento" ~: (amigosDe redAmigosDe usuario1) ~?= [usuario2,usuario3,usuario4],
    "Caso 4: Usuario amigo de todos" ~: (amigosDe redAmigoDeTodos usuario10) ~?= todosLosUsuariosSin10 -- Es igual de demostrativo que 3 amigos
    -- El caso 4 tira error por el orden en el q devuelve. De todas formas podemos comparar con mismosElementos
    --"Caso 5: Usuario amigo de todos" ~: expectAny (amigosDe redAmigoDeTodos usuario10) (permutations todosLosUsuariosSin10)
    ]

-- Amigos de: cuadro casos. Elegir que hacer entre este y el anterior
testsuiteAmigosDe2 = test [
    "Caso 1: Red sin relaciones" ~: (amigosDe redRelacionesVacias usuario10) ~?= [],
    "Caso 2: Usuario sin amigos" ~: (amigosDe redA usuario8) ~?= [],
    "Caso 3: Usuario con amigos" ~: (amigosDe redA usuario2) ~?= [usuario1, usuario4, usuario3],
    "Caso 3.2: Usuario con amigos" ~: mismosElementos (amigosDe redA usuario2) [usuario1, usuario3, usuario4] ~?= True
    ]
-- El mismo argumento de las permutaciones que planteamos
-- en testsuiteNombresDeUsuarios aplica para esta funcion

-- Para estos casos de test las publicaciones no son importantes
testsuiteCantidadDeAmigos = test [
    "Caso 1: Usuario sin amigos" ~: (cantidadDeAmigos redUsuarioXSinAmigos usuario9) ~?= 0,
    "Caso 2: Usuario con 3 amigos" ~: (cantidadDeAmigos redAmigosDe usuario1) ~?= 3,
    "Caso 3: Usuario amigo de todos" ~: (cantidadDeAmigos redAmigoDeTodos usuario10) ~?= 11 -- Usuarios totales de la red
    ]

-- Elegir con el anterior
testsuiteCantidadDeAmigos2 = test [
    "Caso 1: Red sin relaciones" ~: (cantidadDeAmigos redRelacionesVacias usuario10) ~?= 0, -- Caso 1 fuerza caso 2
    "Caso 2: Usuario sin amigos" ~: (cantidadDeAmigos redB usuario4) ~?= 0,
    "Caso 3: Usuario con amigos" ~: (cantidadDeAmigos redB usuario12) ~?= 3
    ]

-- Para estos casos de test las publicaciones no son importantes
testsuiteUsuarioConMasAmigos = test [
    "Caso 1: Lista de relaciones vacia" ~: expectAny (usuarioConMasAmigos redRelacionesVacias) [usuario8, usuario12, usuario10, usuario5],
    "Caso 2: Uno solo con la mayor cantidad de amigos" ~: (usuarioConMasAmigos redA) ~?= usuario1,
    "Caso 3: Mas de uno con la mayor cantidad de amigos" ~: expectAny (usuarioConMasAmigos redB) [usuario8, usuario12]
    ]

-- Para estos casos de test las publicaciones no son importantes
testsuiteEstaRobertoCarlos = test [
    "Caso 1: Lista de usuarios vacia" ~: (estaRobertoCarlos redUsuariosVaciaRobertoCarlos) ~?= False,
    "Caso 2: Lista de relaciones vacia" ~: (estaRobertoCarlos redRelacionesVacias) ~?= False,
    "Caso 3: Ningun usuario con mas de 10 amigos" ~: (estaRobertoCarlos redSinRobertoCarlos) ~?= False,
    "Caso 4: Algun usuario con mas de 10 amigos" ~: (estaRobertoCarlos redConRobertoCarlos) ~?= True
    ]

-- Para estos casos de test las relaciones no son importantes
testsuitePublicacionesDe = test [
    "Caso 1: Red sin publicaciones" ~: (publicacionesDe redSinPublicaciones usuario2) ~?= [],
    "Caso 2: Usuario sin publicaciones" ~: (publicacionesDe redConPublicaciones usuario2) ~?= [],
    "Caso 3.1: Usuario con publicaciones" ~: (publicacionesDe redConPublicaciones usuario12) ~?= [publicacion12_1, publicacion12_2, publicacion12_3, publicacion12_4, publicacion12_5]
    -- Poner dos casos igual de representativos no es necesario, capaz si es un testeo mas intenso si.
    --"Caso 3.2: Usuario con publicaciones" ~: (publicacionesDe redConPublicaciones usuario8) ~?= [publicacion8_1, publicacion8_2]
    ]

-- Para estos casos de test las relaciones no son importantes
testsuitePublicacionesQueLeGustanA = test [
    "Caso 1: Red sin publicaciones" ~: (publicacionesQueLeGustanA redSinPublicaciones usuario2) ~?= [],
    "Caso 2: Usuario no likeo publicaciones" ~: (publicacionesQueLeGustanA redSeguidorFiel usuario8) ~?= [],
    "Caso 3: Usuario likeo publicaciones" ~: (publicacionesQueLeGustanA redSeguidorFiel usuario7) ~?= [publicacion8_1, publicacion12_3, publicacion12_5]
    ]

-- Para estos casos de test las relaciones no son importantes
testsuiteLesGustanLasMismasPublicaciones = test [
    "Caso 1: Red sin publicaciones" ~: (lesGustanLasMismasPublicaciones redSinPublicaciones usuario1 usuario2) ~?= True,
    "Caso 2: Ninguno likeo publicaciones" ~: (lesGustanLasMismasPublicaciones redSeguidorFiel usuario2 usuario8) ~?= True,
    "Caso 3: Ambos likearon publicaciones pero no las mismas" ~: (lesGustanLasMismasPublicaciones redSeguidorFiel usuario11 usuario3) ~?= False,
    "Caso 4: Ambos likearon las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones redSeguidorFiel usuario10 usuario12) ~?= True
    ]


-- Para estos casos de test las relaciones no son importantes
testsuiteTieneUnSeguidorFiel = test [
    "Caso 1: Red sin publicaciones" ~: (tieneUnSeguidorFiel redSinPublicaciones usuario6) ~?= False,
    "Caso 2: Usuario sin publicaciones" ~: (tieneUnSeguidorFiel redSeguidorFiel usuario1) ~?= False,
    "Caso 3: Publicaciones de usuario sin likes" ~: (tieneUnSeguidorFiel redSeguidorFiel usuario2) ~?= False,
    "Caso 4: Publicaciones de usuario con likes (sin fiel)" ~: (tieneUnSeguidorFiel redSeguidorFiel usuario8) ~?= False,
    "Caso 5: Publicaciones de usuario con likes (con fiel)" ~: (tieneUnSeguidorFiel redSeguidorFiel usuario12) ~?= True
    ]

testsuiteExisteSecuenciaDeAmigos = test [
    "Caso 1: Red sin relaciones" ~: (existeSecuenciaDeAmigos redRelacionesVacias usuario8 usuario5) ~?= False,
    "Caso 2: u1 sin amigos" ~: (existeSecuenciaDeAmigos redSecuenciaDeAmigos usuario8 usuario12) ~?= False,
    "Caso 3: u2 sin amigos" ~: (existeSecuenciaDeAmigos redSecuenciaDeAmigos usuario5 usuario8) ~?= False,
    "Caso 4: u1 y u2 con amigos, u1 = u2" ~: (existeSecuenciaDeAmigos redSecuenciaDeAmigos usuario1 usuario1) ~?= True,
    "Caso 5: u1 y u2 con amigos, u1 /= u2 (existe)" ~: (existeSecuenciaDeAmigos redSecuenciaDeAmigos usuario7 usuario11) ~?= True,
    "Caso 6: u1 y u2 con amigos, u1 /= u2 (no existe)" ~: (existeSecuenciaDeAmigos redSecuenciaDeAmigos usuario5 usuario3) ~?= False
    ]
-- No sabemos si podemos usar esto o no
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

usuario1  = (1,  "Facu")
usuario2  = (2,  "Jose")
usuario3  = (3,  "Valen")
usuario4  = (4,  "Tobi")
usuario5  = (5,  "Cumbio")
usuario6  = (6,  "Mati_capo_49")
usuario7  = (7,  "Xx_CerealKiller_xX")
usuario8  = (8,  "Cumbio")
usuario9  = (9,  "Mati_capo_49")
usuario10 = (10, "gobernador2003@hotmail.com")
usuario11 = (11, "Adam Flayman")
usuario12 = (12, "Barry B. Benson")

todosLosUsuarios = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]

relacionesVacia   = []
publicacionesVacia = []

-- Para el test suite nombresDeUsuarios
usuariosVacia = []
redUsuariosVacia = (usuariosVacia, relacionesVacia, publicacionesVacia)

usuariosSinNombresRepetidos = [usuario1, usuario2, usuario3, usuario4, usuario5]
redUsuariosSinNombresRepetidos = (usuariosSinNombresRepetidos, relacionesVacia, publicacionesVacia)

usuariosConNombresRepetidos = [usuario9, usuario8, usuario4, usuario3, usuario5, usuario6]
redUsuariosConNombresRepetidos = (usuariosConNombresRepetidos, relacionesVacia, publicacionesVacia)

-- Para el test suite amigosDe y cantidadDeAmigos
usuariosAmigosDe = [usuario1,usuario2,usuario3,usuario4]

redUsuarioXSinAmigos = (todosLosUsuarios, relacionesSinUsuarioX, publicacionesVacia)
relacionesSinUsuarioX = [relacion1_2, relacion3_1, relacion4_2, relacion5_1, relacion2_3, relacion1_4, relacion4_5, relacion8_5, relacion8_12, relacion10_8, relacion12_5, relacion10_12, relacion5_10, relacion7_6, relacion4_11]

relacion7_6 = (usuario6, usuario7)
relacion4_11 = (usuario4, usuario11)

relacionesAmigosDe = [relacion1_2, relacion3_1, relacion2_3, relacion1_4] -- Notar la posición cambiante 
                                                                          -- De los usuarios en las relaciones

redAmigosDe = (usuariosAmigosDe, relacionesAmigosDe, publicacionesVacia) -- Es indistinto la presencia de
                                                                         -- Publicaciones para este ejercicio
                                                                         -- En particular

redAmigoDeTodos = (todosLosUsuarios, relacionAmigoDeTodos,publicacionesVacia)      
relacionAmigoDeTodos = [relacion10_1,relacion10_12,relacion10_2,relacion11_10,relacion3_10,relacion10_4,relacion5_10,relacion10_6,relacion7_10,relacion10_8,relacion10_9]                                                                   
todosLosUsuariosSin10 = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario11, usuario12]

-- Para el test suite usuarioConMasAmigos
usuariosA = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario8]
usuariosB = [usuario8, usuario12, usuario10, usuario5, usuario4]

relacion1_2 = (usuario1, usuario2)
relacion3_1 = (usuario3, usuario1)
relacion4_2 = (usuario4, usuario2)
relacion5_1 = (usuario5, usuario1)
relacion2_3 = (usuario2, usuario3)
relacion1_4 = (usuario1, usuario4)
relacion4_5 = (usuario4, usuario5)

relacionesA = [relacion1_2, relacion3_1, relacion4_2, relacion5_1, relacion2_3, relacion1_4, relacion4_5]

relacion8_5   = (usuario8, usuario5)
relacion8_12  = (usuario8, usuario12)
relacion10_8  = (usuario10, usuario8)
relacion12_5  = (usuario12, usuario5)
relacion10_12 = (usuario10, usuario12)
relacion5_10  = (usuario5, usuario10)

relacionesB = [relacion8_5, relacion8_12, relacion10_8, relacion12_5, relacion10_12, relacion5_10]

redRelacionesVacias = (usuariosB, relacionesVacia, publicacionesVacia)
redA = (usuariosA, relacionesA, publicacionesVacia)
redB = (usuariosB, relacionesB, publicacionesVacia)

-- Para el test de Roberto Carlos
redUsuariosVaciaRobertoCarlos = (usuariosVacia, relacionesA, publicacionesVacia)


relacion10_1   = (usuario10, usuario1)
relacion10_2   = (usuario10, usuario2)
relacion3_10   = (usuario3, usuario10)
relacion10_4   = (usuario10, usuario4)
relacion10_6   = (usuario10, usuario6)
relacion7_10   = (usuario7, usuario10)
relacion10_9   = (usuario10, usuario9)
relacion11_10  = (usuario11, usuario10)
relacion12_10  = (usuario12, usuario10)

relacionesSinRobertoCarlos = relacionesA
relacionesConRobertoCarlos = relacionesA ++ [relacion10_1, relacion10_2, relacion3_10, relacion10_4, relacion5_10, relacion12_10, relacion10_6, relacion7_10, relacion10_8, relacion10_9, relacion11_10, relacion10_12]

redSinRobertoCarlos = (todosLosUsuarios, relacionesSinRobertoCarlos, publicacionesVacia)
redConRobertoCarlos = (todosLosUsuarios, relacionesConRobertoCarlos, publicacionesVacia)

-- Para el test de publicacionesDe

publicacion8_1 = (usuario8, "La revolucion industrial y sus consecuencias", [usuario3, usuario7, usuario1, usuario6])
publicacion8_2 = (usuario8, "Fueron un desastre para la raza humana", [usuario10, usuario4, usuario12])

publicacion12_1 = (usuario12, "According to all known laws of aviation, there is no way a bee should be able to fly.", [usuario11, usuario5])
publicacion12_2 = (usuario12, "Its wings are too small to get its fat little body off the ground.", [usuario11, usuario4])
publicacion12_3 = (usuario12, "The bee, of course, flies anyway because bees don't care what humans think is impossible.", [usuario1,usuario3,usuario4,usuario5,usuario6,usuario7,usuario9,usuario11])
publicacion12_4 = (usuario12, "Yellow, black. Yellow, black. Yellow, black. Yellow, black.", [usuario11, usuario9 ])
publicacion12_5 = (usuario12, "Ooh, black and yellow! Let's shake it up a little.", [usuario11, usuario7, usuario10, usuario12])

publicaciones_8_12 = [publicacion8_1, publicacion8_2, publicacion12_1, publicacion12_2, publicacion12_3, publicacion12_4, publicacion12_5]

redConPublicaciones = (todosLosUsuarios, relacionesVacia, publicaciones_8_12)
redSinPublicaciones = (todosLosUsuarios, relacionesVacia, publicacionesVacia)

-- Para el test de tieneUnSeguidorFiel

publicacion2_1 = (usuario2, "Como ojos", [])
publicacion2_2 = (usuario2, "Mick Jones tiene frio por los ojos", [])

publicaciones_seguidorFiel = [publicacion2_1, publicacion2_2] ++ publicaciones_8_12
redSeguidorFiel = (todosLosUsuarios, relacionesVacia, publicaciones_seguidorFiel)

-- Para el test de existeSecuenciaDeAmigos

relacion7_1   = (usuario7, usuario1)
--relacion1_4   = (usuario4, usuario1) (ya esta def)
relacion4_3   = (usuario4, usuario3)
relacion3_12  = (usuario12, usuario3)
relacion12_11 = (usuario12, usuario11)

relacion5_2 = (usuario5, usuario2)
relacion6_5 = (usuario6, usuario5)

relacionesSecuenciaDeAmigos = [relacion7_1, relacion1_4, relacion4_3, relacion3_12, relacion12_11, relacion5_2, relacion6_5]

redSecuenciaDeAmigos = (todosLosUsuarios, relacionesSecuenciaDeAmigos, publicaciones_seguidorFiel)
