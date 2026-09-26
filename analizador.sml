
fun obtenerInfo (ruta) =
    let 
        val info = TextIO.openIn ruta
        val lineas = TextIO.inputAll info
    in 
        TextIO.closeIn info;
        lineas
    end

fun obtenerLineas (ruta) =
    let 
        val contenido = obtenerInfo (ruta);
        val array = String.fields (fn c => c = #"\n") contenido
        val resto = if List.length array > 0 
        then List.drop (array, 1) else []
    in 
        Array.fromList resto
    end

fun obtenerElems (vector) = 
    let
        val contenido = String.tokens(fn c => c = #",") vector
    in 
        contenido
    end

fun arrayLineas (ruta) =
    let 
        val linea = obtenerLineas(ruta)
        val lineasLista = Array.foldr (fn (x, acc) => x :: acc) [] linea
    in
        List.map obtenerElems lineasLista
    end

fun compararEnteros (num1, num2) =
    let
      val n1 = valOf (Int.fromString (List.last num1))
      val n2 = valOf (Int.fromString (List.last num2))
    in
      Int.compare (n1, n2)
    end

fun quicksort funcion [] = []
    | quicksort funcion (piv :: xs) =
        let
            val menores = List.filter (fn x => funcion (x, piv) = LESS) xs
            val mayores = List.filter (fn x => funcion (x, piv) <> LESS) xs
        in 
            quicksort funcion mayores @ [piv] @ quicksort funcion menores
        end

fun librosPopulares (ruta, inicio, fin) =
    let
      val filas = arrayLineas (ruta)
      val libros = quicksort compararEnteros filas
      val populares = List.filter (fn libro => let 
      val copias = valOf (Int.fromString (List.last libro))
        in copias >= inicio andalso copias <= fin end) libros
    in
      populares
    end



