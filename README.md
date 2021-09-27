# Práctica 0: Repaso de Haskell

Deben completar las funciones faltantes de los archivos `Lists.hs`, `Trees.hs` y `Ciphers.hs` del directorio `src/LC`.

Una descripción de los ejercicios y los puntajes está en el archivo `practica0.pdf`.

Antes de empezar a programar, hay que descargar las dependencias del proyecto.

```sh
stack setup
```

## Pruebas

Su práctica debe pasar todas las pruebas

```sh
stack test
```

Para solo ejecutar las pruebas reacionadas a `<nombre>`, se puede usar

``` sh
stack test --test-arguments='--match "<nombre>"'
```

También puede ser útil usar `ghci` en su código para probar sus funciones de forma interactiva

``` sh
stack ghci
```

## Correr la práctica

Una vez que terminen la implementación, verifiquen que compile

```sh
stack build
```


Y que el programa escrito en `app/Main.hs` se pueda ejecutar

```sh
stack exec practica0
```

