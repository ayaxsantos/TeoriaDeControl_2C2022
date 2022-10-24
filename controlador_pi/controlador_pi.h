#ifndef CONTROLADOR_PI
#define CONTROLADOR_PI

//////////////////////////////////////////////////

void setup();
void rutina_control();
double leer_temperatura_lm35();
double leer_valor_de_referencia();

void equivalencia_a_grados_de(double &un_valor);

//////////////////////////////////////////////////

#endif