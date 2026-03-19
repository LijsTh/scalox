# Scalox

Un interprete de lox en Scala 3.

## Requisitos Previos

- [JDK 11+](https://adoptium.net/)
- [sbt](https://www.scala-sbt.org/download.html)

## Ejecutar el Proyecto

```bash
# Compilar + ejecutar
sbt run

# Solo compilar
sbt compile

# Ejecutar el codigo y detecta cambios (modo watch)
sbt ~run
```

## Ejecutar Tests

```bash
# Ejecutar todos los tests
sbt test

# Ejecutar un test específico
sbt "testOnly utils.GreeterSuite"

# Ejecutar tests al detectar cambios (modo watch)
sbt ~test
```

> Este proyecto usa **Scala 3.8.2**
