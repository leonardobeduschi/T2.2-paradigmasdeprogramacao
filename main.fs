// trabalho 2.2 - paradigmas de programação
// professor: rodrigo lyra
// aluno: leonardo beduschi iunes

open System

// EXERCÍCIO 1
let rec Impares lista =
    match lista with
    | [] -> 1 // lista vazia
    | head :: tail ->
        let ProdutoTail = Impares tail // multiplica os valores restantes se forem ímpares
        match head % 2 with
        | 1 -> head * ProdutoTail // se for ímpar, multiplica o primeiro valor
        | _ -> ProdutoTail // se for par, não multiplica o primeiro valor


// EXERCÍCIO 2
// lê um número
let LerNumero mensagem =
    printf "%s" mensagem
    let entrada = Console.ReadLine() // ler a entrada do usuário
    match Int32.TryParse(entrada) with // tenta converter valor para inteiro
    | (true, numero) -> float numero
    | (false, _) -> 
        printfn "Inválido.."
        0.0

// lê dois números do usuário
let LerDoisNumeros () =
    let n1 = LerNumero "Digite o primeiro número: "
    let n2 = LerNumero "Digite o segundo número: "
    (n1, n2)

// calcula a potência
let rec potencia (b: float) (expoente: int) =
    match expoente with
    | 0 -> 1.0
    | _ -> b * potencia b (expoente - 1)


// calcula o resultado
let Resultado (n1: float) (n2: float) =
    match (n1, n2) with
    | (a, b) when a = b -> a * b // se os números forem iguais, multiplica
    | (a, b) -> potencia a (int b) // se os números forem diferentes, calcula a potência


//EXERCÍCIO 3
let Primo numero =
    let rec Divisao (n: int) (divisor: int) =
        match divisor with
        | d when d * d > n -> true // se "d" ao quadrado maior que "n" é primo
        | d when n % d = 0 -> false // se "n" é divisível por "d" não é primo
        | _ -> Divisao n (divisor + 1)

    match numero with
    | n when n <= 1 -> false // números menores ou iguais a 1 não são primos
    | 2 -> true // 2 é primo
    | _ -> Divisao numero 2


// EXERCÍCIO 4
let rec SomaPrimos lista =
    match lista with
    | [] -> 0 // lista vazia
    | head :: tail ->
        let SomaTail = SomaPrimos tail // soma de primos restantes
        match Primo head with
        | true -> head + SomaTail // soma se for primo
        | false -> SomaTail // ignora se não for primo
        

[<EntryPoint>]
let main argv =
    // teste exercício 1
    printfn "Exercício 1:"
    let lista = [1; 2; 3; 4; 5]
    let produto = Impares lista
    printfn "O produto dos valores ímpares é: %d" produto
    printfn ""

    // teste exercício 2
    printfn "Exercício 2:"
    let (num1, num2) = LerDoisNumeros()
    let resultado = Resultado num1 num2
    printfn "O resultado é: %f" resultado
    printfn ""

    // teste exercício 3
    printfn "Exercício 3:"
    let NumPrimo = LerNumero "Digite um número: "
    let VerificaPrimo = Primo (int NumPrimo)
    printfn "O número %f é primo? %b" NumPrimo VerificaPrimo
    printfn ""

    // teste exercício 4:
    printfn "Exercício 4:"
    let Lista = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
    let SomaLista = SomaPrimos Lista
    printfn "A soma dos primos em %A é: %d" Lista SomaLista
    printfn ""

    0 // return an integer exit code
