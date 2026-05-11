# Scalox

Un interprete de lox en Scala 3.

## Requisitos Previos

Recomendación instalar con [sdkman](https://sdkman.io/)

- [JDK 11+](https://adoptium.net/)
- [sbt](https://www.scala-sbt.org/download.html)(opcional)

## Ejecutar el Proyecto

### Ejecutar como binario 

Para correrlo rapidamente sin sbt se puede utilizar [scala-cli](https://scala-cli.virtuslab.org/)

```bash
scala-cli package . -o scalox --assembly --power
```

Luego simplemente lo corremos con: 

```bash
./scalox 
```

### Ejecutar con sbt 

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

# Ejecutar tests al detectar cambios (modo watch)
sbt ~test
```

> Este proyecto usa **Scala 3.8.2**
