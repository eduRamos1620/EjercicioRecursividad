main = do
    putStrLn("----------------------------------")
    putStrLn("|             Menu               |")
    putStrLn("----------------------------------")
    putStrLn("1.- Serie Fibonacci")    
    putStrLn("2.- Presentar numeros del 1 al 10") 
    putStrLn("3.- Factorial") 
    putStrLn("4.- Desaparece numeros") 
    putStrLn("5.- Palindromos") 
    putStrLn("6.- Menu calculadora") 
    putStrLn("7.- Salir") 
    putStrLn("Selecciona una opcion: ") 
    op <- getLine
    menu (read op)

menu op = do
    case op of 
        1 -> fibonacci -- terminado
        2 -> numeros 1 -- terminado
        3 -> numfact -- termindo
        4 -> desaparece [0,1,2,3,4,5,6,7,8,9,10] -- terminado
        5 -> palindromos -- terminado
        6 -> calculadora -- terminado
        7 -> print("Terminado")
        _ -> print("Opcion Invalida")
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
fibonacci = do
    putStrLn("Ingresa la posicion que deseas")
    pos <- getLine
    
    print(fibo (read pos))
    main
    
fibo pos = do
    if pos == 0
        then do
            0
    else if pos == 1
        then do 
            1
    else
        fibo(pos-1)+fibo(pos-2)

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
numeros n = do
    if n <= 10
        then do
            print n
            numeros(n+1)
    else do
        putStrLn("termina")
        main
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 5!=120
numfact = do
    putStrLn("De que numero deseas el factorial?")
    fac <- getLine
    factorial (1)(read fac)(1)

factorial x y aux = do
    
    if x <= y
        then do
            print x
            factorial(x+1)(y)(aux*x)
    else
        putStrLn("termina")
    putStrLn("El factorial es "++show(aux))
    main
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++
desaparece lista = do
    if null lista
        then do
            putStrLn("Termina")
            main
    else do
        print(lista)
        let lista2 = init lista
        desaparece(lista2)
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++
palindromos = do
    putStrLn("Introduce el palindromo")
    pali <- getLine
    palindro (pali)
    
palindro pali = do
     if pali == reverse pali
        then do
            putStrLn("Es palindromo")
            main
        else do
            putStrLn("No es palindromo")
            main

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
calculadora = do
    putStrLn("1.- Suma")
    putStrLn("2.- Resta")
    putStrLn("3.- Multiplicacion")
    putStrLn("4.- Division")
    putStrLn("5.- Salir")
    putStrLn("Selecciona una opcion")
    n <- getLine
    caso (read n)

caso n = do
    case n of
        1 -> suma
        2 -> resta
        3 -> multi
        4 -> division
        5 -> print("Salir")
        _ -> print("Opcion no valida")
        
suma = do
    putStrLn("Ingresa primer numero")
    a <- getLine
    let c = read a::Int
    putStrLn("Ingresa segundo numero")
    b <- getLine
    let d = read b::Int
    let resultado = c+d
    putStrLn("El resultado es: "++show resultado)
    main

resta = do
    putStrLn("Ingresa primer numero")
    a <- getLine
    let c = read a::Int
    putStrLn("Ingresa segundo numero")
    b <- getLine
    let d = read b::Int
    let resultado = c-d
    putStrLn("El resultado es: "++show(resultado))
    main
    
multi = do
    putStrLn("Ingresa primer numero")
    a <- getLine
    let c = read a::Int
    putStrLn("Ingresa segundo numero")
    b <- getLine
    let d = read b::Int
    let resultado = c*d
    putStrLn("El resultado es: "++show(resultado))
    main
    
division = do
    putStrLn("Ingresa primer numero")
    a <- getLine
    let c = read a::Float
    putStrLn("Ingresa segundo numero")
    b <- getLine
    let d = read b::Float
    let resultado = c/d
    putStrLn("El resultado es: "++show(resultado))
    main
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
