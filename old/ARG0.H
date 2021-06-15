/*
 * Arg0.h
 * Internal Header File for GnuLib
 *
 */


#define CHAR	  char 
#define SHORT	  short
#define LONG	  long 
#define INT	     int	  
typedef unsigned char  UCHAR;	
typedef unsigned short USHORT;
typedef unsigned long  ULONG;	
typedef unsigned char  *PSZ;
typedef unsigned short BOOL;
#define TRUE    1
#define FALSE   0


#define STRLEN    512
typedef PSZ  *PPSZ;

typedef struct instblk
   {
   USHORT   uIndex;
   PSZ      pszVal;
   } INST;
typedef INST *PINST;

typedef struct
   {
   PSZ      pszParam;
   BOOL     bMinimal;
   BOOL     bNoCase;
   CHAR     cValType;
   USHORT   uCount;
   PINST    pinst;
   } ARG;
typedef ARG  *PARG;

extern BOOL   bInit;
extern PARG   parg;
extern PINST  pinstFree;
extern USHORT uArgErr;
extern USHORT uGlobalIndex;
extern char   szArgErr[];

USHORT SetErr (USHORT uErr, PSZ pszErr);
USHORT Digest (PSZ pszCL);
USHORT GetStringVal (PPSZ ppsz, PSZ psz, BOOL bIgnoreSwitch);


