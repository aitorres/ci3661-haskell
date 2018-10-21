# El Sabio del Laberinto
Proyecto de programación funcional en Haskell para la asignatura Laboratorio de Lenguajes de Programación (CI-3661) en la Universidad Simón Bolívar, trimestre Septiembre-Diciembre 2018.

## Equipo
1. Gustavo Castellanos, 14-10192
2. Andres Torres, 14-11082

## Enunciado
[Ver aquí](https://drive.google.com/file/d/1BgBn4JgNn_JiXqFBgPhpFdbdn7ddB0Uf/view?usp=sharing)

## Detalles de Implementación
Para el manejo de cambios del contexto (del _Laberinto_ y de la _Ruta_) se utilizó el monad transformer [`StateT`](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html#g:3), de manera que fuese fácil usar las características del monad `State` en el monad `IO`.
