module Main (main) where

{-# LANGUAGE RebindableSyntax #-}
 
import Copilot.Arduino.Uno
import qualified Copilot.Arduino.Library.Serial as Serial

baudRateComm :: Int
baudRateComm = 9600

kp :: Behavior Word8
kp = 2

equivalenciaGradosDe :: Behavior ADC  -> Behavior Float
equivalenciaGradosDe unValor = adaptarValor * (constant 500/1023)
    where adaptarValor :: Behavior Float
          adaptarValor = unsafeCast unValor

leerValorAnalogico :: IsAnalogInputPin unaEntrada => Pin unaEntrada -> Sketch (Behavior ADC)
leerValorAnalogico unaEntrada = do
    unaLectura <- input unaEntrada :: Sketch (Behavior ADC)
    return unaLectura

mostrarLecturaSerial :: String -> Behavior Float -> Sketch ()
mostrarLecturaSerial unTexto unaLectura = do
    Serial.baud baudRateComm
    let msg = [Serial.str unTexto, Serial.show unaLectura, Serial.str "\n"]
    Serial.device =: msg

main :: IO ()
main = arduino $ do 
    -- Leer valor de refencia
    -- Calcular error
    -- Control proporcional
    valorDeReferencia <- leerValorAnalogico a0
    lecturaTemperatura <- leerValorAnalogico a1

    let valorDeError = valorDeReferencia - lecturaTemperatura

    (mostrarLecturaSerial "Temperatura leida: ").equivalenciaGradosDe $ lecturaTemperatura
    (mostrarLecturaSerial "Valor del error: " ).equivalenciaGradosDe $ valorDeReferencia

    delay =: MilliSeconds (constant 1000)
