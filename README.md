# El Sabio del Laberinto
Proyecto de programación funcional en Haskell para la asignatura Laboratorio de Lenguajes de Programación (CI-3661) en la Universidad Simón Bolívar, trimestre Septiembre-Diciembre 2018.

## Equipo
1. Gustavo Castellanos, 14-10192
2. Andres Torres, 14-11082

## Enunciado
[Ver aquí](https://drive.google.com/file/d/1BgBn4JgNn_JiXqFBgPhpFdbdn7ddB0Uf/view?usp=sharing)

## Detalles de Implementación
Para el manejo de cambios del contexto (del _Laberinto_ y de la _Ruta_) se utilizó el monad transformer [`StateT`](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html#g:3), de manera que fuese fácil usar las características del monad `State` en el monad `IO`.
El cliente se ejecuta en un _loop_ infinito que pide al usuario opciones y luego ejecuta la función correspondiente asociada a esa opción, mostrando en pantalla la información que solicitó.

El resto de detalles de implementación se encuentran a manera de documentación en formato Haddock dentro de los archivos del proyecto. Note que el archivo *makefile* genera automaticamente la documentación, en formato *html*, en una carpeta denominada *docs*.

## Comandos del makefile disponibles
Compilar correctamente el código fuente y generar la documentación:
```
make
```

Limpiar los archivos (borrar el código compilado, los objetos, y la documentación):
```
make clean
```