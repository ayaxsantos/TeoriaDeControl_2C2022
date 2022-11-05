#include "blink_no_delay.h"

///////////////////////////////////////////////

void setup() 
{
  pinMode(10,OUTPUT);

  tiempoI = 0;
  tiempoM = 0;
  tiempoActual = 0;

  esta_encendido = false;
}

void loop() 
{
  rutina_parpadeo_led();
}

void rutina_parpadeo_led()
{
  tiempoActual = millis();  
  
  if(calcular_tiempo_menor_a(5000,tiempoActual,tiempoI) && !esta_encendido)
  {
    digitalWrite(10,HIGH);
    esta_encendido = true;
    tiempoM = millis();           
  }  

  if(calcular_tiempo_menor_a(5000,tiempoActual,tiempoM) && esta_encendido)
  {
    digitalWrite(10,LOW);  
    esta_encendido = false;
    tiempoI = millis();
  }
}

bool calcular_tiempo_menor_a(unsigned long un_tiempo_limite, unsigned long tiempoF, unsigned long tiempoI)
{
  return (tiempoF - tiempoI) >= un_tiempo_limite;
}

///////////////////////////////////////////////
