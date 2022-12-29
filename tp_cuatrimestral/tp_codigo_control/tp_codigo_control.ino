#include <Servo.h>
#include <math.h>

#define PIN_AIRE_FRIO 2
#define PIN_HUMIDIFICADOR 4
#define PIN_LM35 A4

Servo servoIzqInf;
Servo servoIzqSup;
Servo servoDerSup;
Servo servoDerInf;

Servo servoDuctoAC;
Servo servoCompuertaVentilacion;

///////////////////////////////////////////////////

unsigned int kpHorizontalidad = 1;
unsigned int kiHorizontalidad = 0;

unsigned int valorRefHorizontalidad;
long deltaT = 0, tiempoI = 0, tiempoF = 0;
int errorAcumuladoHoriz = 0;

///////////////////////////////////////////////////

unsigned int valorReferenciaTemperatura;
unsigned int kpTemperatura = 1;
unsigned int kiTemperatura = 2;
int errorAcumuladoTemp = 0;

///////////////////////////////////////////////////

unsigned int valorReferenciaHumedad;
unsigned int kpHumedad = 1;

///////////////////////////////////////////////////

unsigned int valorReferenciaTension;

///////////////////////////////////////////////////

void controlDeTemperatura(float);
float leer_temperatura_lm35(float);
void controlHorizontalidad(uint8_t t, int index);
void controlRedundanciaGeneradores();

Servo servos[4] = {servoIzqInf, servoIzqSup, servoDerSup, servoDerInf };

///////////////////////////////////////////////////

void setup()
{
  Serial.begin(9600);
  pinMode(A0,INPUT);
  pinMode(A1,INPUT);
  pinMode(A2,INPUT);
  pinMode(A3,INPUT);

  servoIzqInf.attach(3);
  servoIzqSup.attach(5);
  servoDerSup.attach(6);
  servoDerInf.attach(9);

  servoIzqInf.write(0);
  servoIzqSup.write(0);
  servoDerSup.write(0);
  servoDerInf.write(0);

  valorRefHorizontalidad = 0;

  valorReferenciaTemperatura = (20 + 50.0) * (1023.0/500.0);
  valorReferenciaHumedad = 40;
  valorReferenciaTension = 220;

  servoDuctoAC.attach(10);
  servoDuctoAC.write(0);

  servoCompuertaVentilacion.attach(11);
  servoCompuertaVentilacion.write(0);

  pinMode(7,OUTPUT);
  pinMode(8,OUTPUT);

  digitalWrite(7,LOW);
  digitalWrite(8,HIGH);
}

void loop() 
{
  float temperatura = analogRead(PIN_LM35);

  controlDeTemperatura(temperatura);
  controlDeHumedad(temperatura);
  
  controlHorizontalidad(A0,0);
  controlHorizontalidad(A1,1);
  controlHorizontalidad(A2,2);
  controlHorizontalidad(A3,3);

  controlRedundanciaGeneradores();
}

///////////////////////////////////////////////////

void controlRedundanciaGeneradores()
{
  int tensionProveedor = analogRead(A5);
  
  // Tolerancia del 10%
  int tensionMapeada = map(tensionProveedor,0,1023,0,300);
  int error = valorReferenciaTension - tensionMapeada;

  Serial.print("Valor de Tension Mapeada : ");
  Serial.println(tensionMapeada);

  Serial.print("Valor del Error - Tension : ");
  Serial.println(error);

  if(abs(error) >= 22)
  {
    //PROVEEDOR
    digitalWrite(8,LOW);

    //GENERADOR
    digitalWrite(7,HIGH);
    return;
  }

  digitalWrite(7,LOW);
  digitalWrite(8,HIGH);
}

void controlHorizontalidad(uint8_t unPin, int index)
{
  deltaT = tiempoF - tiempoI;
  tiempoI = millis();
  srand(millis());

  // Generamos el valor de realimentacion dada la falta de un sensor
  int realimentacion = digitalRead(unPin);

  if(!realimentacion){
    servos[index].write(0);
    return;
  }

    

  int valor_aletorio = abs(rand()) % 25 + 1;
  int error = valorRefHorizontalidad - valor_aletorio;
  errorAcumuladoHoriz += deltaT * error;

  Serial.print("El valor de realimentacion f es:");
  Serial.println(valor_aletorio);

  int valorSalida = kpHorizontalidad * error + kiHorizontalidad * errorAcumuladoHoriz;
  valorSalida = map(valorSalida,0,25,0,180);

  Serial.print("El valor de SALIDA es:");
  Serial.println(valorSalida);

  servos[index].write(abs(valorSalida));

  tiempoF = millis();
}

void controlDeTemperatura(float temperatura)
{
  deltaT = abs(tiempoF - tiempoI); // Porque la simulación anda mal
  tiempoI = millis();

  float error = valorReferenciaTemperatura - temperatura;
  errorAcumuladoTemp += deltaT * error;

  Serial.print("El valor de temperatura es:");
  Serial.println(leer_temperatura_lm35(temperatura));

  int valorSalida = kpTemperatura * error + kiTemperatura * errorAcumuladoTemp;
  
  Serial.print("El valor de salida es:");
  Serial.println(valorSalida);
  valorSalida = map(valorSalida,-500,500,0,255);


  //Control aire acondicionado frio, ON - OFF
  //Control Compuerta aire acondicionado, Proporcional Integral
  if(error < 0)
  {
    digitalWrite(PIN_AIRE_FRIO,HIGH);   
    servoDuctoAC.write(abs(valorSalida));
  }
  else
  {
    digitalWrite(PIN_AIRE_FRIO,LOW);
    servoDuctoAC.write(0);
  }

  tiempoF = tiempoI;
}

void controlDeHumedad(float temperatura)
{
  float humedad = map(temperatura,100,200,95,5);
  
  float error = valorReferenciaHumedad - humedad;
  Serial.print("Error: ");
  Serial.println(error);

  Serial.print("El valor del porcentaje de humedad es:");
  Serial.print(humedad);
  Serial.println("%");

  int valorSalida = kpHumedad * error;
  valorSalida = map(valorSalida,5,95,0,255);
  Serial.print("Valor salida: ");
  Serial.println(valorSalida);

  //Control humidificador, ON - OFF
  //Control compuerta ventilacion - Proporcional
  if(error > 0)
  {
    digitalWrite(PIN_HUMIDIFICADOR,HIGH);  
    servoCompuertaVentilacion.write(0); 
  }
  else if(error < 0)
  {
    digitalWrite(PIN_HUMIDIFICADOR,LOW);
    servoCompuertaVentilacion.write(abs(valorSalida));
  }
  else
  {
    digitalWrite(PIN_HUMIDIFICADOR,LOW);
    servoCompuertaVentilacion.write(0);
  }
}

///////////////////////////////////////////////////

float leer_temperatura_lm35(float una_lectura)
{
  float temperatura = ( una_lectura * (500.0 / 1023.0) ) - 50.0;
  return temperatura;
}

///////////////////////////////////////////////////
