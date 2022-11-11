module Main (main) where

{-# LANGUAGE RebindableSyntax #-}
 
import Prelude hiding ((<=),(&&),(>=))
import Copilot.Arduino.Uno
import Copilot.Arduino.Internals as Internals
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

mostrarLecturaSerial :: (Internals.ShowCType a, Typed a) => String -> Behavior a -> Sketch ()
mostrarLecturaSerial unTexto unaLectura = do
    Serial.baud baudRateComm
    let msg = [Serial.str unTexto, Serial.show unaLectura, Serial.str "\n"]
    Serial.device =: msg

unaCondicion :: Behavior ADC -> Behavior Bool
unaCondicion unError = unError >= 0 && unError <= 255

deWord16HaciaWord8 :: Behavior Word16 -> Behavior Word8
deWord16HaciaWord8 unValor = unsafeCast unValor

-- 1023/255 aprox = 4
mapearValor :: Behavior ADC -> Behavior Word8
mapearValor unValor = (*4).deWord16HaciaWord8.adaptarValor $ unValor
        where adaptarValor :: Behavior Int16 -> Behavior Word16
              adaptarValor unValorPri = unsafeCast unValorPri

controlProporcional :: Behavior Word8 -> Behavior Word8
controlProporcional unError = kp * unError 

main :: IO ()
main = arduino $ do 
    -- Leer valor de refencia
    -- Calcular error
    -- Control proporcional
    valorDeReferencia <- leerValorAnalogico a0
    lecturaTemperatura <- leerValorAnalogico a1

    let valorDeError = valorDeReferencia - lecturaTemperatura

    (mostrarLecturaSerial "Temperatura leida: ").equivalenciaGradosDe $ lecturaTemperatura
    (mostrarLecturaSerial "Valor de referencia: " ).equivalenciaGradosDe $ valorDeReferencia
    mostrarLecturaSerial "Valor PWM: " valorDeError    

    pin5 =: (pwm . controlProporcional . mapearValor $ valorDeError)

    delay =: MilliSeconds (constant 1000)
