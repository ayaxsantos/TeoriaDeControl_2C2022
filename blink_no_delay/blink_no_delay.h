#ifndef BLINK_NO_DELAY
#define BLINK_NO_DELAY

///////////////////////////////////////////

unsigned long tiempoI, tiempoM, tiempoActual;
bool esta_encendido;

///////////////////////////////////////////

bool calcular_tiempo_menor_a(unsigned long,unsigned long,unsigned long);
void rutina_parpadeo_led();

///////////////////////////////////////////

#endif
