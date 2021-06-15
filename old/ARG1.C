/*
 * Arg-1.c
 *
 * Part of GnuArg.h
 * Part of the GnuLib library
 *
 * (C) 1993 Info Tech Inc.
 *
 * Craig Fitzgerald
 * 
 * This file provides additonal functions
 * for the Arg module
 */


#include <string.h>
#include "arg0.h"
#include "GnuArg.h"



USHORT ArgGetIndex (PSZ pszArg, USHORT uIndex)
   {
   USHORT i, j;
   SHORT  iComp;

   if (!bInit)
      {
      SetErr (3, NULL);
      return 0xFFFF;
      }

   if (!pszArg) /*--- free Args ---*/
      {
      for (i=0; pinstFree && pinstFree[i].pszVal; i++)
         if (i == uIndex)
            return pinstFree[i].uIndex;
      SetErr (6, pszArg);
      return 0xFFFF;
      }

   for (i=0; parg && parg[i].pszParam; i++)
      {
      if (parg[i].bNoCase)
         iComp = stricmp (parg[i].pszParam, pszArg);
      else
         iComp = strcmp (parg[i].pszParam, pszArg);
      if (iComp)
         continue;

      for (j=0; j< parg[i].uCount; j++)
         if (j == uIndex)
            return parg[i].pinst[j].uIndex;
      SetErr (9, pszArg);
      return 0xFFFF;
      }
   SetErr (6, pszArg);
   return 0xFFFF;
   }




USHORT ArgFillBlk2 (PSZ pszArgStr)
   {
   char szCpy [STRLEN];
   PSZ  pszCpy = szCpy;

   if (!bInit)
      return SetErr (3, NULL);
   if (!pszArgStr)
      return 0;

   strcpy (szCpy, pszArgStr);
   if (*pszCpy == '"')
      {
      pszCpy++;
      szCpy[strlen (szCpy) -1] = '\0';
      }
   strcat (pszCpy, " ");
   return Digest (pszCpy);
   }





USHORT ArgEnum (PSZ pszArg, PSZ pszVal, USHORT uIndex)
   {
   USHORT i, j;

   if (pszArg) pszArg[0] = '\0';
   if (pszVal) pszVal[0] = '\0';
   if (!bInit)
      return !SetErr (3, NULL);
   if (uIndex >= uGlobalIndex)
      return !SetErr (10, NULL);


   for (i=0; pinstFree && pinstFree[i].pszVal; i++)
      if (pinstFree[i].uIndex == uIndex)
         {
         if (pszArg) strcpy (pszArg, pinstFree[i].pszVal);
         return 1;
         }

   for (i=0; parg && parg[i].pszParam; i++)
      for (j=0; j< parg[i].uCount; j++)
         if (parg[i].pinst[j].uIndex == uIndex)
            {
            if (pszArg) strcpy (pszArg, parg[i].pszParam);
            if (parg[i].pinst[j].pszVal && pszVal)
               strcpy (pszVal, parg[i].pinst[j].pszVal);
            return 2;
            }

   SetErr (9, NULL);
   return FALSE;
   }


