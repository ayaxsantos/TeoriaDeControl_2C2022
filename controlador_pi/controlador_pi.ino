#include "./controlador_pi.h"

#define SALIDA_LED 6
#define ENTRADA_REF A4
#define ENTRADA_TEMP A2
#define kp 25
#define ki 15

////////////////////////////////////////////////////

void setup()
{
    Serial.begin(9600);
    pinMode(SALIDA_LED,OUTPUT);   
}

int main(void)
{
    init();
    setup();
    
    rutina_control();

    return EXIT_SUCCESS;
}

////////////////////////////////////////////////////

void rutina_control()
{
    double una_temperatura = 0, un_valor_de_referencia = 0;
    double error = 0, error_acumulado = 0;
    unsigned long tiempo_i = 0, tiempo_f = 0, delta_t;

    //NOTA: NO DEBE CAMBIAR!!
    un_valor_de_referencia = leer_valor_de_referencia();

    while(true)
    {
        delta_t = tiempo_f - tiempo_i;
        tiempo_i = millis();

        una_temperatura = leer_temperatura_lm35();
        error = un_valor_de_referencia - una_temperatura;
        error_acumulado = error_acumulado + error * delta_t;

        Serial.print("El valor de temperatura es: ");
        Serial.println(una_temperatura);

        Serial.print("El valor de referencia es: ");
        Serial.println(un_valor_de_referencia);

        analogWrite(SALIDA_LED,kp * error + ki * error_acumulado);

        tiempo_f = millis();
    }
}

double leer_temperatura_lm35()
{
    double una_lectura = analogRead(ENTRADA_TEMP);
    equivalencia_a_grados_de(una_lectura);
    return una_lectura;
}

double leer_valor_de_referencia()
{
    double un_valor_de_referencia = analogRead(ENTRADA_REF);
    un_valor_de_referencia = map(un_valor_de_referencia,0,1023,-55,150);
    return un_valor_de_referencia;
}

void equivalencia_a_grados_de(double &un_valor)
{
    un_valor *= 5000/1023.0;
    un_valor /= 10;
}

////////////////////////////////////////////////////
