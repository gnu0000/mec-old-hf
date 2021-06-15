#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <gnutype.h>
#include <gnuarg.h>
#include "hf.h"
#include "crater.h"
#include "rand.h"

#define pow4(x)        ((x)*(x)*(x)*(x))

#define outer_scale     (1.0-inner_scale*pow4(border))/pow4(1.0-border)
#define alpha           (35.25/180.0*M_PI)  /* sphere segment cutoff angle */
#define b2              (1.0/b1/b1/b1/b1)
#define b3              (1.0/b1)
#define crater_depth    0.85                /* 0.9 */
#define b1              10.0                /* constant for size distribution */
#define border          0.63
#define inner_scale     1
#define CRATER_COVERAGE 0.15
#define SQUEEZE 1.3


extern BIG fCRATER_DEN;                 /* surface crater density */


static BIG a1 = crater_depth, b, d;

/*
 * function to determine the z-elevation dependent 
 * on normalized squared
 * radial coordinate 
 */
BIG crater_profile (BIG nsq_rad)
   {
   static BIG c = 50.0;   /*100.0;*/  /* controls the wall thickeness */
   BIG radialcoord;

   radialcoord = sqrt(fabs(nsq_rad));

   if (radialcoord > b)  /* outer region gauss distribution */
      {
      return d*exp(-c*(radialcoord-b)*(radialcoord-b));
      } 
   else /* inner region sphere segment */
      {
      return a1 - sqrt(1.0 - nsq_rad);
      }
   }


/*
 * function to control the weight of the crater profile as opposed
 *  to the underlying terrain elevation 
 */
BIG dissolve(BIG nsq_rad)
   {
   if (nsq_rad > 0.6*0.6) 
      return 1.0-0.9*exp(-30.0*(sqrt(nsq_rad) - 0.6)*(sqrt(nsq_rad) - 0.6));
   else 
      return 0.1*exp(-25.0*(sqrt(nsq_rad) - 0.6)*(sqrt(nsq_rad) - 0.6));
   }



///*
// *y   x, y, xloc, yloc
// *    bWRAP, iSIZE
// *    a, weight
// *y   level_with level_pure
// */
//
//void Shift (int x, int y, int xloc, int yloc)
//   {
//   int k, xloc, yloc, cratersize;
//   int ii, jj;
//
//   ii = xloc + (x); 
//   jj = yloc + (y); 
//
//   if (bWRAP || ((ii >= 0) && (ii < iSIZE) && (jj >= 0) && (jj < iSIZE))) 
//      {
//      ii = (ii + iSIZE) % iSIZE;
//      jj = (jj + iSIZE) % iSIZE;
//      Real(a,ii,jj) = (shift  + (Real(a,ii,jj))* weight + 
//         (level_with + (Imag(a,ii,jj)-level_pure)/SQUEEZE) * (1.0-weight)); 
//      }
//   }


/* this macro calculates the coordinates, does clipping and modifies
 * the elevation at this spot due to shift and weight.
 * Imag() contains the crater-free underground. Real() contains the result.
 * level_with: average altitude of cratered surface in region
 * level_pure: average altitude of uncratered surface
 */
#define SHIFT(x,y) { \
    ii = xloc + (x); jj = yloc + (y); \
    if (bWRAP || ((ii >= 0) && (ii < iSIZE) && \
       (jj >= 0) && (jj < iSIZE))) {\
      ii = (ii + iSIZE) % iSIZE; \
      jj = (jj + iSIZE) % iSIZE; \
      Real(a,ii,jj) = (shift  + (Real(a,ii,jj))*weight + \
      (level_with + (Imag(a,ii,jj)-level_pure)/SQUEEZE) * (1.0-weight)); \
    } \
  }


void distribute_craters(float  *a, 
                        UINT   uCount, 
                        INT    iSIZE, 
                        BOOL   bWRAP, 
                        BIG    fCRATER_SCALE)
   {
   int i, j, xloc, yloc, cratersize;
   int sq_radius;
   BIG nsq_rad, weight;
   int lev_samples, samples;
   BIG level_with, level_pure;
   BIG craterscale;                 /* vertical crater scaling factor */
   BIG c,d2, shift;

   /* init constants */
   b = sin(alpha);
   d = a1 - cos(alpha);

   /* build a copy of the terrain */
   for (i = 0 ; i < iSIZE; i++)
      for (j = 0 ; j < iSIZE; j++)
         Imag(a,i,j) = Real(a,i,j);

   while (uCount) 
      {
      int ii, jj;

      xloc = ran1() * iSIZE;
      yloc = ran1() * iSIZE;
      c    = ran1() + b3; // in random order
      d2   = b2/c/c/c/c;
      
      cratersize  = 3 + (int)(d2 * CRATER_COVERAGE * iSIZE); // d2 in range 0-1
      craterscale = 
         (((fCRATER_SCALE * pow ((cratersize/(3+CRATER_COVERAGE*iSIZE)),0.9))
         /256*pow (iSIZE/256.0,0.1))
         /CRATER_COVERAGE*80);
   
      /* what is the mean height of this plot */
      samples = lev_samples = max((cratersize*cratersize)/5, 1);
      level_with = level_pure = 0.0;
      while (samples) 
         {
         i = ran1()*(2*cratersize) - cratersize;
         j = ran1()*(2*cratersize) - cratersize;

         if (i*i+j*j > cratersize*cratersize) 
            continue;

         ii = xloc + i; jj = yloc + j;

         /* handle wrapping details... */
         if ((bWRAP) || ((ii >= 0) && (ii < iSIZE) && (jj >= 0) && (jj < iSIZE))) 
            {
            ii = (ii + iSIZE) % iSIZE;
            jj = (jj + iSIZE) % iSIZE;

            level_with += Real(a, ii, jj);
            level_pure += Imag(a, ii, jj);
            samples--;
            }
         }
      level_with /= lev_samples;
      level_pure /= lev_samples;

      for (i = cratersize; i > 0; i--) 
         {
         for (j = i; j >= 0; j--) 
            {
            /* check if outside */
            sq_radius = i*i+j*j;
            nsq_rad = (BIG)sq_radius/cratersize/cratersize;
            if (nsq_rad > 1) 
               continue;

            /* inside the crater area */
            shift = craterscale*crater_profile(nsq_rad);
            weight = dissolve(nsq_rad);

            if (i==j || j==0) 
               {
               SHIFT(i,j); SHIFT(-j,i); SHIFT(-i,-j); SHIFT(j,-i);
               } 
            else 
               {
               SHIFT(i,j); SHIFT(-j,i); SHIFT(-i,-j); SHIFT(j,-i);
               SHIFT(j,i); SHIFT(-i,j); SHIFT(-j,-i); SHIFT(i,-j);
               }
            }
         }
      /* the center point */
      shift =  craterscale*crater_profile(0.0);
      weight = dissolve(0.0);
      SHIFT(0,0);

      if (!(uCount-- % 1000))
         printf(".");
      }
   }

