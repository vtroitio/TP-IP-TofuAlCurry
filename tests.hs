import Test.HUnit
import Solucion

main = do
    print("Test suite: nombresDeUsuarios")
    runTestTT todosLosTest

todosLosTest = test [
    testsuite_nombresDeUsuarios
    ]

testsuite_nombresDeUsuarios = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redB) ~?= ["Juan","Natalia","Pedro"] -- Ejemplo catedra
    ]

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = []
publicacionesA = []
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3]--, usuario5]
relacionesB = []
publicacionesB = []
redB = (usuariosB, relacionesB, publicacionesB)

