/*
 * hf.h
 *
 *
 *
 */
#define FALSE     0
#define TRUE      1
#define SHORT	   short
#define LONG	   long 
#define INT	      int	  
typedef int BOOL;
typedef unsigned char  *PSZ;
typedef unsigned short USHORT;
typedef unsigned int   UINT;
typedef unsigned long  ULONG;	
typedef unsigned char  UCHAR;
typedef unsigned char  CHAR;
typedef void     *PVOID;
typedef USHORT   *PUSHORT;
typedef UINT     *PUINT;
typedef INT      *PINT;
typedef ULONG    *PULONG;
typedef double   DBL;

#define M_PI    3.14159265358979323846
#define M_E     2.7182818284590452354

#define ELEMENTS(array) (sizeof(array)/sizeof((array)[0]))
#define Real(v, x, y)  v[1 + (((x) * iSIZE) + (y)) * 2]
#define Imag(v, x, y)  v[2 + (((x) * iSIZE) + (y)) * 2]

double sgn1(double x);

USHORT _cdecl Error (PSZ psz, ...);

