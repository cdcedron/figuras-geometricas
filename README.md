# figuras-geometricas
# figuras geometricas con Haskell

# Decisiones de diseño
Para evitar posibles errores se decidió implementar las funciones de las consignas con el tipo de datos Maybe, lo cual hace que el usuario se tenga que encargar de extraer el resultado de la operación antes de continuar procesando su Song y su código deberá aceptar y manejar el caso en que se retorne Nothing.

Para la función unfold, usamos pattern matching con el tipo de dato Song, en donde unfold analiza la cancion dada buscando por Transpose_by y Repeat, y ejecutando transpose_song y repeat_song, respectivamente. transpose_song se encarga de transponer todas las notas dentro de la canción antes de continuar, y repeat_song espera a que haya terminado el unfold de la canción para generar una nueva canción conformada por la concatenación de la canción original la cantidad de veces que haga falta.

Para la función compute primero aplica unfold a la canción, y luego, la función auxiliar compute_aux, usando pattern matching, modifica la cancion usando funciones de Euterpea para eliminar los Concat y los Parallel, convirtiendo la cancion en un Music Pitch.

Para la función time, sumamos la Dur (duración de cada nota), sí la canción está compuesta de un sólo Fragment, suma todas las duraciones dentro de ese Fragment, pero sí la canción es más compleja, calcula en cada Fragment la suma de Dur, y luego las suma entre sí. La extracción de cada valor de Dur está a cargo de la función durac_nota, y la sumatoria de estos, a cargo de suma y sumarep, este último para los casos en donde canciones se repiten.

Para la función run hicimos pattern matching con el data type Command a, y haciendo una recursión, en donde sí la lista de comandos está vacía, se toma como si todos los comandos fueron ejecutados, y devuelve la lista final con todas las modificaciones. Como la operación Add es sólo agregar el elemento a la lista final, se ejecuta sin problemas. En use1 y use2, dado que en int dado debe ser igual o mayor a 0, y menor al length de la lista a devolver en el momento de la operación, sí no respetan estas condiciones, se devuelve Nothing.


# Dependencias
ghc version 8.0.2 Euterpea version 2.0.7

# ¿Cómo correr el lab?
Para ejecutar el laboratorio

cd src ghci song.hs

# Para salir, CTRL+Z.
