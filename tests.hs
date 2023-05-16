import Test.HUnit
import Solucion

main = runTestTT todosLosTest

mainConTitulos = do
    putStrLn("Test suite: nombresDeUsuarios")
    runTestTT testsuiteNombresDeUsuarios
    putStrLn("")

    putStrLn("Test suite: amigosDe")
    runTestTT testsuiteAmigosDe
    putStrLn("")

     putStrLn("Test suite: cantidadDeAmigos")
    runTestTT testsuiteCantidadDeAmigos
    putStrLn("")

    putStrLn("Test suite: usuarioConMasAmigos")
    runTestTT testsuiteUsuarioConMasAmigos
    putStrLn("")

    putStrLn("Test suite: estaRobertoCarlos")
    runTestTT testsuiteEstaRobertoCarlos
    putStrLn("")

    putStrLn("Test suite: publicacionesDe")
    runTestTT testsuitePublicacionesDe
    putStrLn("")
    

todosLosTest = test [
    testsuiteNombresDeUsuarios,
    testsuiteUsuarioConMasAmigos,
    testsuiteEstaRobertoCarlos,
    testsuiteAmigosDe,
    testsuiteCantidadDeAmigos
    ]

-- Aclaracion: testsuiteNombresDeUsuarios
-- Si fuera un test de caja negra propiamente dicho deberiamos poner
-- todos   los  test  case  posibles,  incluyendo  las  posibles  de
-- permutaciones de las relaciones y publicaciones.
-- Dado que conocemos que la implementacion hecha no depende de esos
-- posibles casos  vamos a descartarlos, pasando siempre los  mismos
-- valores para dichos parametros.
-- Aclaracion general:
-- Esa va a ser la forma de encarar todos los casos de  test para las
-- funciones que se comporten de forma similar para todo el TP.

testsuiteNombresDeUsuarios = test [
    "Caso 1: Lista de usuarios vacia" ~: (nombresDeUsuarios redUsuariosVacia) ~?= [],
    "Caso 2: Lista de usuarios sin nombres repetidos" ~: (nombresDeUsuarios redUsuariosSinNombresRepetidos) ~?= ["Facu", "Jose", "Valen", "Tobi", "Cumbio"],
     -- Aca medio que el test case esta "elegido a mano" porque no sabiamos como poner todas las permutaciones sin usar Data.List
    "Caso 3: Lista de usuarios con nombres repetidos" ~: (nombresDeUsuarios redUsuariosConNombresRepetidos) ~?= ["Tobi","Valen", "Cumbio", "Mati_capo_49"]
    ]

-- Para estos casos de test las publicaciones no son importantes
testsuiteAmigosDe = test [
    "Caso 1: Usuario sin amigos" ~: (amigosDe redUsuarioXSinAmigo usuario9) ~?= []
    "Caso 2: Red sin relaciones" ~: (amigosDe redRelacionesVacias usuario1) ~?= []
    "Caso 3: Usuario con relaciones de distinto ordenamiento" ~: (amigosDe redAmigosDe usuario1) ~?= [usuario2,usuario3,usuario4]
    "Caso 4: Usuario amigo de todos" ~: (amigosDe redAmigoDeTodos usuario10) ~?= todosLosUsuariosSin10
    ]

-- Para estos casos de test las publicaciones no son importantes
testsuiteCantidadDeAmigos = test [
    "Caso 1: Usuario sin amigos" ~: (cantidadDeAmigos redUsuarioXSinAmigo usuario9) ~?= 0
    "Caso 2: Usuario con 3 amigos" ~: (cantidadDeAmigos redAmigosDe usuario1) ~?= 3
    "Caso 3: Usuario amigo de todos" ~: (cantidadDeAmigos redAmigoDeTodos usuario10) ~?= 11 -- Usuarios totales de la red
    ]

-- Para estos casos de test las publicaciones no son importantes
testsuiteUsuarioConMasAmigos = test [
    --"Caso 1: Lista de relaciones vacia" ~: (usuarioConMasAmigos redRelacionesVacias) ~?= usuario8,
    "Caso 1: Lista de relaciones vacia" ~: expectAny (usuarioConMasAmigos redRelacionesVacias) [usuario8, usuario12, usuario10, usuario5],
    "Caso 2: Uno solo con la mayor cantidad de amigos" ~: (usuarioConMasAmigos redA) ~?= usuario1,
    --"Caso 3: Mas de uno con la mayor cantidad de amigos" ~: (usuarioConMasAmigos redB) ~?= usuario8,
    "Caso 3: Mas de uno con la mayor cantidad de amigos" ~: expectAny (usuarioConMasAmigos redB) [usuario8, usuario12]
    ]

-- Para estos casos de test las publicaciones no son importantes
testsuiteEstaRobertoCarlos = test [
    -- Segun la especificacion interpretamos que puede haber una lista de relaciones no vacia con una lista de usuarios vacia en una red
    "Caso 1: Lista de usuarios vacia" ~: (estaRobertoCarlos redUsuariosVaciaRobertoCarlos) ~?= False, -- Red usuarios vacia tiene relaciones vacia tambien. Hace falta probar todos los casos???
    "Caso 2: Lista de relaciones vacia" ~: (estaRobertoCarlos redRelacionesVacias) ~?= False,
    "Caso 3: Ningun usuario con mas de 10 amigos" ~: (estaRobertoCarlos redSinRobertoCarlos) ~?= False,
    "Caso 4: Algun usuario con mas de 10 amigos" ~: (estaRobertoCarlos redConRobertoCarlos) ~?= True
    ]

-- Para estos casos de test las relaciones no son importantes
testsuitePublicacionesDe = test [
    "Caso 1: Red sin publicaciones" ~: (publicacionesDe redSinPublicaciones usuario2) ~?= [],
    "Caso 2: Usuario sin publicaciones" ~: (publicacionesDe redConPublicaciones usuario2) ~?= [],
    "Caso 3.1: Usuario con publicaciones" ~: (publicacionesDe redConPublicaciones usuario12) ~?= [publicacion12_1, publicacion12_2, publicacion12_3, publicacion12_4, publicacion12_5],
    "Caso 3.2: Usuario con publicaciones" ~: (publicacionesDe redConPublicaciones usuario8) ~?= [publicacion8_1, publicacion8_2]
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
relacionesSinUsuario = [relacion1_2, relacion3_1, relacion4_2, relacion5_1, relacion2_3, relacion1_4, relacion4_5, relacion8_5, relacion8_12, relacion10_8, relacion12_5, relacion10_12, relacion5_10, relacion7_6, relacion4_11]

relacion7_6 = (usuario6, usuario7)
relacion4_11 = (usuario4, usuario11)

relacionesAmigosDe = [relacion1_2, relacion3_1, relacion2_3, relacion1_4] -- Notar la posición cambiante 
                                                                          -- De los usuarios en las relaciones

redRelacionesVacias = (usuariosB, relacionesVacia, publicacionesVacia)
redAmigosDe = (usuariosAmigosDe, relacionesAmigosDe, publicacionesVacia) -- Es indistinto la presencia de
                                                                         -- Publicaciones para este ejercicio
                                                                         -- En particular

redAmigoDeTodos = (todosLosUsuarios, relacionAmigoDeTodos,publicacionesVacia)      
relacionAmigoDeTodos = [relacion10_1,relacion10_12,relacion10_2,relacion11_10,relacion3_10,relacion10_4,relacion5_10,relacion10_6,relacion7_10,relacion10_8,relacion10_9]                                                                   
todosLosUsuariosSin10 = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario11, usuario12]

-- Para el test suite usuarioConMasAmigos
usuariosA = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario8]
usuariosB = [usuario8, usuario12, usuario10, usuario5]

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

publicacion12_1 = (usuario12, "According to all known laws of aviation, there is no way a bee should be able to fly.", [usuario11, usuario5, usuario2])
publicacion12_2 = (usuario12, "Its wings are too small to get its fat little body off the ground.", [usuario11, usuario4])
publicacion12_3 = (usuario12, "The bee, of course, flies anyway because bees don't care what humans think is impossible.", todosLosUsuarios)
publicacion12_4 = (usuario12, "Yellow, black. Yellow, black. Yellow, black. Yellow, black.", [usuario11, usuario9 ])
publicacion12_5 = (usuario12, "Ooh, black and yellow! Let's shake it up a little.", [usuario11, usuario7])

publicaciones_8_12 = [publicacion8_1, publicacion8_2, publicacion12_1, publicacion12_2, publicacion12_3, publicacion12_4, publicacion12_5]

redConPublicaciones = (todosLosUsuarios, relacionesVacia, publicaciones_8_12)
redSinPublicaciones = (todosLosUsuarios, relacionesVacia, publicacionesVacia)