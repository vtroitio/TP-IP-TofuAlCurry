import Test.HUnit
import Solucion

main = runTestTT todosLosTest

mainConTitulos = do
    putStrLn("Test suite: nombresDeUsuarios")
    runTestTT testsuiteNombresDeUsuarios
    --putStrLn("")

todosLosTest = test [
    testsuiteNombresDeUsuarios
    ]

-- Aclaracion: testsuiteNombresDeUsuarios
-- Si fuera un test de caja negra propiamente dicho deberiamos poner
-- todos  los test  case posibles,  incluyendo las  posibilidades de
-- permutaciones relativas a las relaciones y publicaciones.
-- Dado que conocemos que la implementacion hecha no depende de esos
-- posibles casos  vamos a descartar esos  casos, pasando siempre los
-- mismos valores para dichos parametros.
-- Aclaracion general:
-- Esa va a ser la forma de encarar todos los casos de  test para las
-- funciones que se comporten de forma similar para todo el TP.

testsuiteNombresDeUsuarios = test [
    "Caso 1: Lista de usuarios vacia" ~: (nombresDeUsuarios redUsuariosVacia) ~?= [],
    "Caso 2: Lista de usuarios sin nombres repetidos" ~: (nombresDeUsuarios redUsuariosSinNombresRepetidos) ~?= ["Facu", "Jose", "Valen", "Tobi", "Cumbio"],
    "Caso 3: Lista de usuarios con nombres repetidos" ~: (nombresDeUsuarios redUsuariosConNombresRepetidos) ~?= ["Tobi","Valen", "Cumbio", "Mati_capo_49"]
    ]

usuario1 = (1, "Facu")
usuario2 = (2, "Jose")
usuario3 = (3, "Valen")
usuario4 = (4, "Tobi")
usuario5 = (5, "Cumbio")
usuario6 = (6, "Mati_capo_49")
usuario7 = (7, "Xx_CerealKiller_xX")
usuario8 = (8, "Cumbio")
usuario9 = (9, "Mati_capo_49")

relacionesVacia   = []
publicacionesVacia = []

usuariosVacia = []
redUsuariosVacia = (usuariosVacia, relacionesVacia, publicacionesVacia)

usuariosSinNombresRepetidos = [usuario1, usuario2, usuario3, usuario4, usuario5]
redUsuariosSinNombresRepetidos = (usuariosSinNombresRepetidos, relacionesVacia, publicacionesVacia)

usuariosConNombresRepetidos = [usuario9, usuario8, usuario4, usuario3, usuario5, usuario6]
redUsuariosConNombresRepetidos = (usuariosConNombresRepetidos, relacionesVacia, publicacionesVacia)