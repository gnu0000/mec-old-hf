/*
 * Arg-0.c
 *
 * Part of GnuArg.h
 * Part of the GnuLib library
 *
 * (C) 1993 Info Tech Inc.
 *
 * Craig Fitzgerald
 * 
 * This module provides the base functions for
 * the Arg module
 * 
 */


#include <string.h>
#include "arg0.h"

#define STRLEN    512
#define SMSTRLEN  128
#define BIGSTRLEN 4096
#define VALLEN    128

BOOL   bInit   = FALSE;
PARG   parg;
PINST  pinstFree;
USHORT uGlobalIndex = 0;

USHORT uArgErr = 0;
char   szArgErr[SMSTRLEN];



/*******************************************************************/
/*******************************************************************/

USHORT SetErr (USHORT uErr, PSZ pszErr)
   {
   uArgErr = uErr;
   if (pszErr)
      strncpy (szArgErr, pszErr, SMSTRLEN-2);
   else 
      szArgErr[0] = '\0';
   szArgErr[SMSTRLEN-1] = '\0';
   return uErr;
   }



char Eat (PPSZ ppsz, PSZ pszEatList)
   {
   while (**ppsz && strchr (pszEatList, **ppsz))
      (*ppsz)++;
   return **ppsz;
   }



BOOL EatChar (PPSZ ppsz, CHAR c)
   {
   BOOL   bReturn;

   Eat (ppsz, " \t\n");
   if (bReturn = ((CHAR) **ppsz == (CHAR) c))
      (*ppsz)++;
   return bReturn;
   }



USHORT BuildArgInfo (PSZ pszArgStr)
   {
   USHORT i,uArgs = 0;
   CHAR   c, szParam[VALLEN];

   parg = (PARG) malloc (sizeof (ARG));
   parg->pszParam = NULL;

   while (1)
      {
      if (!Eat (&pszArgStr, " \t\n"))
         break;

      uArgs++;
      parg = (PARG) realloc (parg, sizeof (ARG) * (uArgs+1));
      parg[uArgs - 1].pinst = NULL;
      parg[uArgs - 1].uCount = 0;

      if (parg[uArgs-1].bMinimal = (*pszArgStr == '*'))
         pszArgStr++;

      if (parg[uArgs-1].bNoCase = (*pszArgStr == '^'))
         pszArgStr++;

      i = 0;
      if (!(szParam[i++] = *pszArgStr++))
         return SetErr (4, pszArgStr);

      while (*pszArgStr && !strchr ("@$=:?-%#& \t\n", *pszArgStr))
         szParam[i++] = *pszArgStr++;
      szParam[i] = '\0';

      parg[uArgs-1].pszParam = strdup (szParam);

      c = parg[uArgs-1].cValType = (CHAR)(*pszArgStr ? *pszArgStr: ' ');

      if (strchr ("\n\t\r\b", c))
         parg[uArgs-1].cValType = ' ';

      if (!*pszArgStr++)
         break;
      }
   parg[uArgs].pszParam = NULL;
   return 0; //mdh
   }



void Flatten (PPSZ argv, PSZ pszCmdLine)
   {
   *pszCmdLine = '\0';

   for (argv++; *argv != NULL; argv++)
      strcat (strcat (pszCmdLine, *argv), " ");
   }



void AddNewParam (USHORT i, PSZ pszVal)
   {
   parg[i].uCount++;
   parg[i].pinst = (PINST) realloc (parg[i].pinst, 
                                     sizeof (INST) * parg[i].uCount);
   parg[i].pinst[parg[i].uCount-1].uIndex = uGlobalIndex - 1;
   parg[i].pinst[parg[i].uCount-1].pszVal = (pszVal ? strdup (pszVal) : NULL);
   }



USHORT GetStringVal (PPSZ ppsz, PSZ psz, BOOL bIgnoreSwitch)
   {
   PSZ p = psz;

   *psz = '\0';
   Eat (ppsz, " \t\n");

   if (!bIgnoreSwitch && (**ppsz == '/' || **ppsz == '-'))
      return 5;

   while (**ppsz && !strchr (" \t\n", **ppsz))
      *psz++ = *(*ppsz)++;
   *psz = '\0';

   return !!(*p == '\0') * 2;
   }



/*
 * This fn expects the index of the matching arg blk
 *                 the the cmd line
 *                 the ptr into the cmd line just after the matched param 
 *
 * returns 0 if it added ok
 *         2 if bad form.
 *         5 if string val is bad or non existant
 */
USHORT AddMatchedParam (USHORT uIndex, PPSZ ppszCL, PSZ pszValPtr)
   {
   CHAR   szTmp[STRLEN];
   USHORT uError;

   switch (parg[uIndex].cValType)
      {
      case ' ':
         if (*pszValPtr != ' ')
            return 2;
         AddNewParam (uIndex, NULL);
         *ppszCL = pszValPtr;
         return 0;

      case '$':
         if (*pszValPtr != ' ')
            return 2;
         if (uError = GetStringVal (&pszValPtr, szTmp, 0))
            return uError;
         AddNewParam (uIndex, szTmp);
         *ppszCL = pszValPtr;
         return 0;

      case '=':
         if (!EatChar (&pszValPtr, '='))
            return 2;
         if (uError = GetStringVal (&pszValPtr, szTmp, 0))
            return uError;
         AddNewParam (uIndex, szTmp);
         *ppszCL = pszValPtr;
         return 0;

      case ':':
         if (!EatChar (&pszValPtr, ':'))
            return 2;
         if (uError = GetStringVal (&pszValPtr, szTmp, 0))
            return uError;
         AddNewParam (uIndex, szTmp);
         *ppszCL = pszValPtr;
         return 0;

      case '%':
         Eat (&pszValPtr, " \t\n:=");
         if (uError = GetStringVal (&pszValPtr, szTmp, 0))
            return uError;
         AddNewParam (uIndex, szTmp);
         *ppszCL = pszValPtr;
         return 0;

      case '#':
         Eat (&pszValPtr, " \t\n:=");
         if (uError = GetStringVal (&pszValPtr, szTmp, 1))
            return uError;
         AddNewParam (uIndex, szTmp);
         *ppszCL = pszValPtr;
         return 0;

      case '&':
         if (!EatChar (&pszValPtr, ':') && !EatChar (&pszValPtr, '='))
            return 2;
         if (uError = GetStringVal (&pszValPtr, szTmp, 1))
            return uError;
         AddNewParam (uIndex, szTmp);
         *ppszCL = pszValPtr;
         return 0;
         
//
// This works but
// -param val  really shouldn't work because it is too confusing
//
//      case '?':
//         Eat (&pszValPtr, " \t\n:=");
//         uError = GetStringVal (&pszValPtr, szTmp, 0);
//         AddNewParam (uIndex, (uError ? NULL : szTmp));
//         *ppszCL = pszValPtr;
//         if (uError && (*(pszValPtr -1) == ' ' || *(pszValPtr -1) == '\t'))
//            (*ppszCL)--;
//         return 0;

      case '?':
         {
         if (*pszValPtr == ' ')
            {
            /*--- no param val ---*/
            AddNewParam (uIndex, NULL);
            *ppszCL = pszValPtr;
            return 0;
            }
         if (!EatChar (&pszValPtr, ':') && !EatChar (&pszValPtr, '='))
            return 2;
         if (uError = GetStringVal (&pszValPtr, szTmp, 0))
            return uError;
         AddNewParam (uIndex, szTmp);
         *ppszCL = pszValPtr;
         return 0;
         }

      case '@':
         if (*pszValPtr == ' ')
            return 2;
         if (uError = GetStringVal (&pszValPtr, szTmp, 1))
            return uError;
         AddNewParam (uIndex, szTmp);
         *ppszCL = pszValPtr;
         return 0;

      case '-':
         AddNewParam (uIndex, NULL);
         *ppszCL = pszValPtr;
         return 0;
      }
   return 8;
   }



USHORT FindMinParam (PPSZ ppszCL)
   {
   USHORT i, uLen, uMatch, uCount = 0;
   SHORT  iComp;
//   PSZ    p1, p2;

//   uLen = strlen (*ppszCL);
//
//   if (p1 = strchr (*ppszCL, '='))
//      uLen = p1 - *ppszCL;
//
//   if (p2 = strchr (*ppszCL, ' '))
//      uLen = p2 - *ppszCL;
//
//   if (p1 && p2 && p1 < p2)
//      uLen = p1 - *ppszCL;

   for (uLen=0; (*ppszCL)[uLen]; uLen++)
      if (strchr (" :=", (*ppszCL)[uLen]))
         break;

   for (i=0; parg && parg[i].pszParam; i++)
      {
      if (parg[i].bNoCase)
         iComp = strnicmp (parg[i].pszParam, *ppszCL, uLen);
      else
         iComp = strncmp (parg[i].pszParam, *ppszCL, uLen);

      uCount += !iComp;
      if (!iComp) uMatch = i;
      }

   if (!uCount)
      return SetErr (1, *ppszCL);             // no match

   if (uCount > 1)
      return SetErr (7, *ppszCL);             // >1 match

   if (!parg[uMatch].bMinimal)
      return SetErr (1, *ppszCL);             // match not bMin

   if (!parg[uMatch].cValType == '@' ||
      !parg[uMatch].cValType == '-' )
      return SetErr (1, *ppszCL);             // cannot match @ or - types

   return AddMatchedParam (uMatch, ppszCL, *ppszCL + uLen);
   }



/* 
 *
 */
USHORT FindParam (PSZ *ppszCL, CHAR cType)
   {
   USHORT i, uLen, uRet;
   SHORT  iComp;

   for (i=0; parg && parg[i].pszParam; i++)
      {
      if (cType != parg[i].cValType)
         continue;
      if ((uLen = strlen (parg[i].pszParam)) > strlen (*ppszCL))
         continue;
      if (parg[i].bNoCase)
         iComp = strnicmp (parg[i].pszParam, *ppszCL, uLen);
      else
         iComp = strncmp (parg[i].pszParam, *ppszCL, uLen);

      if (iComp) continue;

      uRet = AddMatchedParam (i, ppszCL, *ppszCL + uLen);
      if (!uRet || uRet > 2)
         return SetErr (uRet, *ppszCL);
      }
   return SetErr (1, *ppszCL);
   }




void AddFreeParam (PPSZ ppszCL)
   {
   char     szStr[STRLEN];
   USHORT   i;

   GetStringVal (ppszCL, szStr, 1);
   for (i=0; pinstFree && pinstFree[i].pszVal; i++)
      ;
   pinstFree = (PINST) realloc (pinstFree, sizeof (INST) * (i+2));
   pinstFree[i].pszVal   = strdup (szStr);
   pinstFree[i].uIndex   = uGlobalIndex - 1;
   pinstFree[i+1].pszVal = NULL;
   }



USHORT Digest (PSZ pszCL)
   {
   USHORT   i, uRet;
   PSZ      pszLook;

   while (1)
      {
      if (!Eat (&pszCL, " \t\n"))
         break;

      if (*pszCL != '-' && *pszCL != '/')
         {
         uGlobalIndex++;
         AddFreeParam (&pszCL);
         continue;
         }
      pszCL++;

      while (1)
         {
         if (!*pszCL || *pszCL == ' ')
            break;

         uGlobalIndex++;

         pszLook = " $%#?=:@-&";

         for (i=0; *pszLook != '\0'; pszLook++)
            {
            uRet = FindParam (&pszCL, *pszLook);
            if (!uRet || uRet > 2)
               break;
            }

         if (uRet == 1)
            uRet = FindMinParam (&pszCL);

         if (!uRet) continue;

         return SetErr (uRet, pszCL);
         }
      }
   return SetErr (0, NULL);
   }




USHORT ArgBuildBlk (PSZ pszArgDef)
   {
   char szCpy [STRLEN];
   PSZ  pszCpy = szCpy;


   if (!pszArgDef)
      return SetErr (3, NULL);

   strcpy (szCpy, pszArgDef);
   if (*pszCpy == '"')
      {
      pszCpy++;
      szCpy[strlen (szCpy) -1] = '\0';
      }

   bInit    = TRUE;
   parg     = NULL;
   pinstFree = NULL;
   return BuildArgInfo (pszCpy);
   }


 
USHORT ArgFillBlk (PSZ argv[])
   {
   CHAR  szCmdLine [BIGSTRLEN];

   if (!bInit)
      return SetErr (3, NULL);
   Flatten (argv, szCmdLine);
   return Digest (szCmdLine);
   }



/*******************************************************************/



USHORT ArgIs (PSZ pszArg)
   {
   USHORT i;
   SHORT  iComp;

   if (!bInit)
      {
      SetErr (3, NULL);
      return 0;
      }

   if (!pszArg) /*--- free Args ---*/
      {
      for (i=0; pinstFree && pinstFree[i].pszVal; i++)
         ;
      return i;
      }
   for (i=0; parg && parg[i].pszParam; i++)
      {
      if (parg[i].bNoCase)
         iComp = stricmp (parg[i].pszParam, pszArg);
      else
         iComp = strcmp (parg[i].pszParam, pszArg);

      if (!iComp)
         return parg[i].uCount;
      }
   SetErr (6, pszArg);
   return 0;
   }



PSZ ArgGet (PSZ pszArg, USHORT uIndex)
   {
   USHORT i;
   SHORT  iComp;

   if (!bInit)
      {
      SetErr (3, NULL);
      return NULL;
      }

   if (!pszArg) /*--- free Args ---*/
      {
      for (i=0; pinstFree && pinstFree[i].pszVal; i++)
         if ((i == uIndex) || (uIndex == 0xFFFF && !pinstFree[i+1].pszVal))
            return pinstFree[i].pszVal;
      SetErr (6, pszArg);
      return NULL;
      }

   for (i=0; parg && parg[i].pszParam; i++)
      {
      if (parg[i].bNoCase)
         iComp = stricmp (parg[i].pszParam, pszArg);
      else
         iComp = strcmp (parg[i].pszParam, pszArg);
      if (iComp)
         continue;

      if (uIndex < parg[i].uCount)
         return parg[i].pinst[uIndex].pszVal;
      else if (uIndex == 0xFFFF && parg[i].uCount)
         return parg[i].pinst[parg[i].uCount-1].pszVal;

      SetErr (9, pszArg);
      return NULL;
      }
   SetErr (6, pszArg);
   return NULL;
   }


//cdecl main (int argc, char *argv[])
//   {
//   USHORT uRet; 
//   PSZ    psz, pArgStr;
//
//
//   pArgStr = getenv ("PARG");
//
//   printf ("%s\n\n", pArgStr);
//
//   if (uRet = ArgBuildBlk (pArgStr))
//      {
//      psz = ArgGetErr ();
//      printf ("Build Error : %s\n\n", psz);
//      }
//
//   if (uRet = ArgFillBlk (argv))
//      {
//      psz = ArgGetErr ();
//      printf ("Fill Arg Error : %s\n\n", psz);
//      }
//
//   Dump ();
//   return 0;
//   }




