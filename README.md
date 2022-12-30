# TeoriaDeControl_2C2022

El repositorio contiene:
- Ejercicios realizados en clase durante la cursada.
- Practica realizada para afianzar conocimientos de forma "extra".
- TP Integrador.

## TP Integrador

El objetivo general del TP es la seleccion de un Sistema de Control aplicado a una solucion tecnologica (del area de Sistemas de Informacion), fundamentar su eleccion e identificar diferentes conceptos de la Ingenieria de Control.

Se desarrollo sobre un Sistema de Control para un Data Center, en particular:
- Sistema HVAC (incluye control de humedad).
- Control del nivel de horizontalidad de los racks.
- Redundancia de generadores AC.

La implementacion se realizo utilizando una placa perteneciente a la familia Arduino, dentro del ambito academico tambien en terminos de transductores, actuadores y tiempos de desarrollo.

## Observaciones

Sobre lo desarrollado en Haskell, se utilizo la biblioteca [Arduino Copilot](https://hackage.haskell.org/package/arduino-copilot), que permite programar para algunas placas Arduino utilizando dicho lenguaje. A su vez, esta biblioteca hace uso de [Copilot DSL](https://copilot-language.github.io/) (framework escrito en Haskell, basado en el uso de Streams) para transpilar a codigo en lenguaje C pensado para sistemas con requerimientos de tiempo real.
