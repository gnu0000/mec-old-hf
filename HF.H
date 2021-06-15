/*
 * hf.h
 *
 *
 *
 */

#define M_PI    3.14159265358979323846
#define M_E     2.7182818284590452354

#define ELEMENTS(array) (sizeof(array)/sizeof((array)[0]))
#define Real(v, x, y)  v[1 + (((x) * iSIZE) + (y)) * 2]
#define Imag(v, x, y)  v[2 + (((x) * iSIZE) + (y)) * 2]

typedef double   DBL;

double sgn1(double x);

