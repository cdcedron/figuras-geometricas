 Paradigmas de la Programación 
### Laboratorio 1
***Integrantes*** :
- Cedrón, Cesar
- Ceballos, Leonardo
- Pérez, David

Al principio no sabiamos por donde empezar, por lo que primero intentamos familiarizarnos con Graphics.Gloss, conocer un poco su estructura, tipos, y comenzamos a graficar algunas cosas basicas. Continuamos construyendo el data Dibujo, y al no saber como seguir decidimos hacer la interpretacion geometrica en Interp.hs (Donde al principio nos costo bastante entender los tipos 'FloatingPic' y 'Output') para poder a comenzar a graficar algunas cosas ya usando nuestro lenguaje. Seguimos con la implementacion de los combinadores donde no hubo problemas, pero al llegar a los esquemas para la manipulacion de figuras volvimos a tener problemas en comprender los tipos de cada esquema, pero con la ayuda de profesores y ayudantes en el lab los solucionamos, cuando quisimos comenzar a implementar las funciones siguientes nos dimos cuenta de que no sabiamos muy bien como funcionaba 'sem', pero poco a poco fuimos entendiendola y aprendimos un poco a usar las funciónes lambda y la eta-contraccion. Tuvimos algunos problemas al intentar implementar 'contar' usando sem, ya que no se nos ocurria como hacerla sin usar un contador como estabamos acostumbrados en c, pero al entender mejor 'sem' pudimos hacerlo (aunque sabemos que tiene un problema de eficiencia ya que realiza comparaciones de mas). Una vez implementadas las funciones pasamos a construir el grafico de Escher, empezamos a hacerlo solo mirando el enunciado e imaginandonos como iría quedando, lo que nos causo problemas que los solucionamos intentando seguir paso por paso el artículo de Henderson.

### Extras

Creamos el archivo Extra.hs donde hay algunas figuras extras.

En el branch extra_elegir_basicas:
Modificamos el Main.hs para que se pueda indicar por línea de comando el detalle y la basica que se quiera usar para el grafico de Escher.

En el branch carga_fish:
Modificamos Main.hs para cargar el BMP y Escher.hs para que sustituya las basicas por el fish.
