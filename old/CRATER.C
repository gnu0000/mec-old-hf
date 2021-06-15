#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include "hf.h"
#include "crater.h"
#include "rand.h"

#define alpha        (35.25/180.0*M_PI)  /* sphere segment cutoff angle */
#define crater_depth 0.85                /* 0.9 */

extern double fCRATER_DEN;                 /* surface crater density */

static double a1 = crater_depth, b, d;

/* function to determine the z-elevation dependent on normalized squared
 *  radial coordinate 
 */
double crater_profile (double nsq_rad)
   {
   static double c = 50.0;   /*100.0;*/  /* controls the wall thickeness */
   double radialcoord;

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


#define b1 10.0                 /* constant for size distribution */
#define b2 (1.0/b1/b1/b1/b1)
#define b3 (1.0/b1)

#define pow4(x) ((x)*(x)*(x)*(x))
#define border 0.63
#define inner_scale 1
#define outer_scale (1.0-inner_scale*pow4(border))/pow4(1.0-border)

/* function to control the weight of the crater profile as opposed
 *  to the underlying terrain elevation 
 */
double dissolve(double nsq_rad)
   {
   if (nsq_rad > 0.6*0.6) 
      return 1.0-0.9*exp(-30.0*(sqrt(nsq_rad) - 0.6)*(sqrt(nsq_rad) - 0.6));
   else 
      return 0.1*exp(-25.0*(sqrt(nsq_rad) - 0.6)*(sqrt(nsq_rad) - 0.6));
   }

#define CRATER_COVERAGE 0.15


#define DO_THE_WRAP(x)  (x) = ((x) + iSIZE) % iSIZE;

void distribute_craters(float *a, 
                        unsigned int how_many, 
                        int iSIZE, 
                        BOOL bWRAP, 
                        double fCRATER_SCALE)
   {
   int i, j, k, xloc, yloc, cratersize;
   int sq_radius;
   double nsq_rad, weight;
   int lev_samples, samples;
   double level_with, level_pure;
   double craterscale;                 /* vertical crater scaling factor */
   double c,d2, shift;

   /* init constants */
   b = sin(alpha);
   d = a1 - cos(alpha);

   /* build a copy of the terrain */
   for (i = 0 ; i < iSIZE; i++)
	   for (j = 0 ; j < iSIZE; j++)
	      Imag(a,i,j) = Real(a,i,j);

   k = how_many;
   while (k > 0) 
      {
   	int ii, jj;

	xloc = ran1() * iSIZE;
	yloc = ran1() * iSIZE;

#if 1
	/* in random order */
	c = ran1() + b3;
#else
	/* greatest first. So great craters never eliminate small ones */
	c = (double)(how_many-k+1)/how_many + b3;
#endif

	/* c is in the range b3 ... b3+1 */
	d2 = b2/c/c/c/c;             
	/* d2 is in the range 0 ... 1 */
	cratersize = 3 + (int)(d2 * CRATER_COVERAGE * iSIZE);

#define TEST 0  
      /* TEST=1 one big crater half cut by the lower edge,
		   TEST=2 two big overlapping craters
		   TEST=3 one big crater with two small ones inside
		 */

#if     TEST==1
yloc = cratersize = iSIZE/2-10; xloc = iSIZE-1;
k = 1;
#elif   TEST==2
if (k > 2) k = 2;
if (k == 2) {
  yloc = iSIZE/3;
  xloc = iSIZE/2;
  cratersize = iSIZE/3;
} else {
  yloc = 2*iSIZE/3;
  xloc = iSIZE/2;
  cratersize = iSIZE/3;
}
#elif   TEST==3
if (k > 3) k = 3;
if (k == 3) {
  yloc = iSIZE/2;
  xloc = iSIZE/2;
  cratersize = iSIZE/2;
} else
if (k == 2) {
  yloc = iSIZE/2 + 13;
  xloc = iSIZE/2;
  cratersize = 10;
} else {
  yloc = iSIZE/2 - 13;
  xloc = iSIZE/2;
  cratersize = 10;
}
#endif
	
/* macro to determine the height dependent on crater size */
#define CRATER_SCALE (((fCRATER_SCALE*pow((cratersize/(3+CRATER_COVERAGE*iSIZE)),0.9)) \
		       /256*pow(iSIZE/256.0,0.1))/CRATER_COVERAGE*80)

	craterscale = CRATER_SCALE;     /* vertical crater scaling factor */
	
	/* what is the mean height of this plot */
	samples = lev_samples = max((cratersize*cratersize)/5, 1);
	level_with = level_pure = 0.0;
	while (samples) {
	    /* */
	  /*  i = rand()%(2*cratersize+1) - cratersize;
	      j = rand()%(2*cratersize+1) - cratersize; */
       	      i = ran1()*(2*cratersize) - cratersize;
	      j = ran1()*(2*cratersize) - cratersize;

	    if (i*i+j*j > cratersize*cratersize) continue;
	    ii = xloc + i; jj = yloc + j;

	    /* handle wrapping details... */

	    if ((bWRAP) || 
	      ((ii >= 0) && (ii < iSIZE) && (jj >= 0) && (jj < iSIZE))) {
		DO_THE_WRAP(ii)
		DO_THE_WRAP(jj)
		level_with += Real(a, ii, jj);
		level_pure += Imag(a, ii, jj);
		samples--;
	    }

	}
	level_with /= lev_samples;
	level_pure /= lev_samples;

	/* Now lets create the crater. */


	/* In order to do it efficiently, we calculate for one octant
	 * only and use eightfold symmetry, if possible.
	 * Main diagonals and axes have a four fold symmetry only.
	 * The center point has to be treated single.
	 */

#define SQUEEZE 1.3

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
      DO_THE_WRAP(ii)  DO_THE_WRAP(jj) \
      Real(a,ii,jj) = (shift  + (Real(a,ii,jj))*weight + \
      (level_with + (Imag(a,ii,jj)-level_pure)/SQUEEZE) * (1.0-weight)); \
    } \
  }

/* macro to do four points at once. Points are rotated by 90 degrees. */
#define FOURFOLD(i,j)   SHIFT(i,j) SHIFT(-j,i) SHIFT(-i,-j) SHIFT(j,-i)

/* get eightfold symmetry by mirroring fourfold symmetry along the x==y axe */
#define EIGHTFOLD       FOURFOLD(i,j) FOURFOLD(j,i)


	/* The loop covers a triangle (except the center point)
	 * Eg cratersize is 3, coordinates are shown as i,j
	 *
	 *              3,3 j
	 *         2,2  3,2 |
	 *    1,1  2,1  3,1 v
	 * x  1,0  2,0  3,0
	 * ^          <-i
	 * |
	 * center point
	 *
	 * 2,1 , 3,2 and 3,1 have eightfold symmetry.
	 * 1,0 , 2,0 , 3,0 , 1,1 , 2,2 and 3,3 have fourfold symmetry.
	 */

	for (i = cratersize; i > 0; i--) {
	    for (j = i; j >= 0; j--) {

		/* check if outside */
		sq_radius = i*i+j*j;
		nsq_rad = (double)sq_radius/cratersize/cratersize;
		if (nsq_rad > 1) continue;

		/* inside the crater area */
		shift = craterscale*crater_profile(nsq_rad);
		weight = dissolve(nsq_rad);

		if (i==j || j==0) {
		    FOURFOLD(i,j)
		} else {
		    EIGHTFOLD
		}
	    }
	}
	/* the center point */
	shift =  craterscale*crater_profile(0.0);
	weight = dissolve(0.0);
	SHIFT(0,0)

	/* one crater added */
	k--;

	if ((how_many - k) % 1000 == 0)
	    printf(".");
	    fflush(stdout);
	 /* printf("%d craters of %d created\n", how_many - k, how_many); */
    }
} /* end distribute_craters() */

