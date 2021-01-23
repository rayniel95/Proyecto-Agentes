# Version 1.1 Changelog:

- Se renombró el archivo que anteriormente se llamaba `evironment.pl` a `environment.pl`, causa por la cuál la línea para cargar el archivo en swipl pudiera dar error
- Se añadió en `main.pl` una función `simulate(S,X,Y,N,D,O,T,C,[]).` para realizar `S` simulaciones de 1 sola vez y dar las estadísticas de las mismas:
  - `S`: cantidad de simulaciones a realizar
  - `X,Y`: dimensiones del mapa
  - `N`: cantidad de niños en las simulaciones
  - `D`: porcentaje inicial de suciedad
  - `O`: porcentaje inicial de objetos
  - `T`: tiempo total de las simulaciones
  - `C`: tiempo en las simulaciones antes del cambio del mapa
- Si se desea que se escriban los mapas en cada iteración de cada simulación (modo verbose), quitar el comentario que contiene a la función `print` que hay en el archivo `main.pl` en la línea 125

# Version 1.0

Para correr el proyecto escriba la línea:

`swipl main.pl environment.pl utils.pl` 

en una terminal dentro de esta misma carpeta.

Una vez dentro de swi-prolog escriba:

`start(N,M,B,S,O,T).`

donde:

N es la cantidad de filas del mapa

M es la cantidad de columnas del mapa

B es la cantidad de niños en el mapa

S es el porcentaje inicial de suciedad en el mapa

O es el porcentaje de objetos en el mapa

T cantidad de unidades de tiempo antes de que cambie el entorno



La simulación, sin importar el T introducido, durará siempre 100 unidades de tiempo.

