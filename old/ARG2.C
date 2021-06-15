/*
 * Arg-2.c
 *
 * Part of GnuArg.h
 * Part of the GnuLib library
 *
 * (C) 1993 Info Tech Inc.
 *
 * Craig Fitzgerald
 * 
 * This file provides error functions
 * for the Arg module
 */


#include <string.h>
#include <stdio.h>
#include "arg0.h"
#include "GnuArg.h"


// from arg
//
extern USHORT uArgErr;

char   szErrBuf[128];

char *Errors[] = {"No Error",                                     // 0
                  "Unknown Parameter, or, argument required for", // 1
                  "Parameter in bad form",                        // 2
                  "ArgBuildBlk must be called first",             // 3
                  "EOL Encountered before Param name read",       // 4
                  "Value expected but not found",                 // 5
                  "Requested Parameter not defined",              // 6
                  "Ambiguous Parameter",                          // 7
                  "Bad Parameter Value Type Flag for",            // 8
                  "Requested Parameter not Entered",              // 9
                  "Index too large",                              // 10
                  NULL};




PSZ ArgGetErr (void)
   {
   char  szTmp[STRLEN];
   PSZ   p;

   p = szArgErr;
   GetStringVal (&p, szTmp, TRUE);
   sprintf (szErrBuf, "%s : \"%s\"", Errors[uArgErr], szTmp);
   return szErrBuf;
   }



USHORT ArgIsErr (void)
   {
   return uArgErr;
   }




void ArgDump (void)
   {
   USHORT i,j ;

   printf ("ARG Cmd Line Parse Utility v1.0     Craig Fitzgerald      \n\n");

   for (i=0; parg[i].pszParam; i++)
      {
      printf ("bMin: %d   bNoCase: %d   cType: %c   uCount: %d   Param: %s\n", 
              parg[i].bMinimal, 
              parg[i].bNoCase, 
              parg[i].cValType, 
              parg[i].uCount, 
              parg[i].pszParam);

      for (j=0; j <parg[i].uCount && parg[i].pinst; j++)
         printf ("   Index: %d   Val : %s\n", 
                 parg[i].pinst[j].uIndex,  
                 parg[i].pinst[j].pszVal);
      printf ("\n");
      }
   for (i=0; pinstFree && pinstFree[i].pszVal; i++)
      {
      printf ("Free: Index: %d    Val: %s\n", 
              pinstFree[i].uIndex, 
              pinstFree[i].pszVal); 
      }

   }






