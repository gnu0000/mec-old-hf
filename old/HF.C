/*
 * hf.c  1/2/96
 *
 * modified to heck by Craig Fitzgerald
 *
 */

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>  
#include <time.h>
#include <math.h>
#include <string.h>
#include "hf.h"
#include "rand.h"
#include "fft.h"
#include "crater.h"
#include "GnuArg.h" 

#define div(a,b)  ((b)*floor((a)/(b)) == (a))


CHAR szOUTFILE[256];                    // output tga file name
FILE *fpOUTFILE;                        // output tga file handle
INT  iSIZE;                             // size (x and y) of output file

DBL  fPOWER;                            // range exponent
DBL  fTURB[10], fTSCALE[10];            // turbulence (es)
INT  iTURBCOUNT;                        // number of turbulences

BOOL bCRATERS,    bLIMITS,   bPEAK;     //
BOOL bQUIET,      bWRAP,     bBANDPASS; //
BOOL bBANDREJECT, bHIGHPASS, bLOWPASS;  //
BOOL bNOR,        bNOG;                 //

int  iXSTART,     iYSTART;              // Peak stuf
DBL  fXFRAC,      fYFRAC;               // Peak location
DBL  fBP_CTR,     fBP_QUAL;             // BandPass vals
DBL  fBR_CTR,     fBR_QUAL;             // BandReject vals
DBL  fHP_CUT,     fHP_ORDER;            // HighPass vals
DBL  fLP_CUT,     fLP_ORDER;            // LowPass vals
DBL  fHI_LIMIT,   fLO_LIMIT;            // Limits vals
DBL  fCRATER_DEN, fCRATER_SCALE;        // Crater vals

UINT uSEED;                             // random number stuff
DBL  arand, gaussadd, gaussfac;         //


USHORT _cdecl Error (PSZ psz, ...)
   {
   va_list vlst;

   printf ("Error: ");
   va_start (vlst, psz);
   vprintf (psz, vlst);
   va_end (vlst);
   printf ("\n");
   exit (1);
   return 0;
   }

USHORT _cdecl prntf (PSZ psz, ...)
   {
   va_list vlst;

   if (bQUIET)
      return 0;

   va_start (vlst, psz);
   vprintf (psz, vlst);
   va_end (vlst);
   return 0;
   }



/************************************************************************/
/*                                                                      */
/*                                                                      */
/*                                                                      */
/************************************************************************/
#define MAX_PIXVAL 65535U


void fpc(FILE *fp, UCHAR c)
   {
   fwrite (&c, 1, 1, fp);
   }

void fpi(FILE *fp, USHORT u)
   {
   fwrite (&u, 2, 1, fp);
   }

/*
 *
 *
 */
FILE *OpenTGA(PSZ pszName, USHORT uXSize, USHORT uYSize)
   {
   FILE *fp;

   if (!(fp = fopen (pszName, "wb")))
      Error ("Cannot open output file %s", pszName);

   fpc(fp, 0);       // ID length
   fpc(fp, 0);       // CoMapType
   fpc(fp, 2);       // ImageType
   fpi(fp, 0);       // Index ?
   fpi(fp, 0);       // Length
   fpc(fp, 0);       // CoSize
   fpi(fp, 0);       // X_org
   fpi(fp, 0);       // Y_org
   fpi(fp, uXSize);  // X size
   fpi(fp, uYSize);  // Y size
   fpc(fp, 24);      // pixelsize
   fpc(fp, 0x20);    // descriptor byte b17
   return fp;
   }


/*
 *
 */
void WriteTGARow(FILE *fp, USHORT uXSize, float *row)
   {
   USHORT i, u;

   for (i=0; i<uXSize; i++)
      {
      u = (USHORT)(row[i] * (float)MAX_PIXVAL);
      fpc(fp, 0);                              // B
      fpc(fp, (UCHAR)(bNOG ? 0 : (u % 256)));  // G
      fpc(fp, (UCHAR)(bNOR ? 0 : (u >> 8 )));  // R
      }
   }

/************************************************************************/
/*                                                                      */
/*                                                                      */
/*                                                                      */
/************************************************************************/



DBL sgn1(DBL x)
   {
   return (x > 0 ? 1.0 : (x < 0 ? -1.0 : 0));
   }


/*
 *
 *
 */
void rescale(float **a, int iSIZE, float min, float max)
   {
   int   i,j;
   float r, rmin, rmax;
   float scale, offset;

   rmin = Real(a[0],0,0);
   rmax = rmin;
   for (i = 0; i < iSIZE; i++)       /* find current rmin, rmax */
      for (j = 0; j < iSIZE; j++) 
         {
         r = Real(a[0], i, j);
         rmin = min(rmin, r);
         rmax = max(rmax, r);
         }
   scale = (max-min) / (rmax - rmin);
   offset = min;

   /* rescale all data to lie in the range [min..max]  */
   for (i = 0; i < iSIZE; i++) 
      for (j = 0; j < iSIZE; j++) 
         Real(a[0], i, j) = (scale * (Real(a[0], i, j)-rmin))+ offset;
   }


/* 
 * Warn if iSIZE is prime or has large prime factor. 
 *
 */
void fac_warn(int mesh)
   {
   int fac, flimit,num, maxf;

   fac  = 2;
   num  = mesh;
   maxf = 0;
   flimit = (1+num/2);

   while (fac < flimit)
      {
      if (div(num,fac)) 
         {
         num /= fac;
         if (fac > maxf) 
            maxf = fac;
         } 
      else 
         fac++;
      } 
   if ((num > 1) && (mesh > 150)) 
      prntf("Warning: your iSIZE (%d) is a prime number. FFT will be slow!\n",mesh);
   else if (maxf > 150) 
      prntf("Warning: iSIZE %d has a large prime factor (%d). FFT will be slow!\n",mesh,maxf);
   }


/*
 *
 *
 */
void TwoValue (PSZ pszData, DBL *pdb1, DBL *pdb2, DBL dbDef2)
   {
   PSZ psz;

   *pdb1 = atof (pszData);
   psz   = strchr (pszData, ':');
   *pdb2 = (psz ? atof (psz+1) : dbDef2);

   if (!psz && dbDef2 == -1)
      Error("Variable must have 2 values separated by a colon\n");


printf ("%s ::: %f :: %f : %f\n", pszData, *pdb1, *pdb2, dbDef2);
   }



/************************************************************************/

#define Nrand 4   // Gauss() sample count

/*
 * INITGAUSS  --  Initialise random number generators.  As given in
 *  Peitgen & Saupe, page 77. 
 */
static void initgauss(UINT seed)
   {
   int stemp;

   arand    = pow(2.0, 15.0) - 1.0; // generator range
   gaussadd = sqrt(3.0 * Nrand);
   gaussfac = 2 * gaussadd / (Nrand * arand);
   stemp    = seed;
   seed_ran1(stemp);   /* seed the generator */
   }


/*
 * GAUSS  Return a Gaussian random number.  As given in Peitgen
 *	       & Saupe, page 77. 
 */
static DBL gauss()
   {
   int i;
   DBL sum = 0.0;

   for (i = 1; i <= Nrand; i++) 
      sum += (ran1() * 0x7FFF);
   return gaussfac * sum - gaussadd;
   }


/*
 * INITSEED  --  Generate initial random seed, if needed.  
 *
 */
static int initseed()
   {
   int i,stmp;

   stmp = time(NULL) ^ 0xF37C;
   seed_ran1(stmp);
   for (i=0;i<8;i++)
      ran1();
  
   return uSEED = (UINT)(1000000.0*ran1());
   }

/************************************************************************/


/*
 * FindPeak() --  find peak location 
 */
void FindPeak(float *a, int iSIZE, int *imax, int *jmax)
   {
   int   i, j;
   float r, rmax;

   rmax = Real(a,0,0);
   imax[0] = jmax[0] = 0;

   for (i = 0; i < iSIZE; i++)
      for (j = 0; j < iSIZE; j++) 
         if ((r = Real(a, i, j)) > rmax) 
            {
            rmax = r;
            imax[0] = i;
            jmax[0] = j;
            }
   }


/*
 * fill array with 1/f gaussian noise 
 */
static void ApplyTurbulence(float *a, int n, DBL h, DBL scale)
   {
   int x,y, k, i0, j0, rank, nx, ny, xcent, ycent;
   DBL rad, phase, rcos, rsin;

   prntf("Applying Turbulence  Turb:%lf Scale:%lf.\n", h, scale); 

   nx = ny = n;  /* horizontal and vertical dimension of array */
   xcent = (int)(nx / 2.0 - 0.5);  /* center dimensions of array */
   ycent = (int)(ny / 2.0 - 0.5);

   for (rank = 0; rank <= xcent; rank++) 
      {
      for (k=0;k<=rank;k++) /* fill quadrants 2 and 4  */
         {
         x = k; 
         y = rank;
         phase = 2 * M_PI * ((ran1() * 0x7FFF) / arand);
         if ((x == 0) && (y == 0)) 
            rad = 0; 
         else 
            rad = pow((DBL) (x*x + y*y), -(h+1) / 2) * gauss();
         rcos = rad * cos(phase)*scale; 
         rsin = rad * sin(phase)*scale;
         Real(a, x, y) += rcos; 
         Imag(a, x, y) += rsin;
         if (!((x == 0) && (y == 0))) 
            { 
            i0 = nx-x-1; j0 = ny-y-1;
            Real(a, i0,j0) += rcos;
            Imag(a, i0,j0) += rsin;
            }
         x = rank; 
         y = k;
         phase = 2 * M_PI * ((ran1() * 0x7FFF) / arand);
         if ((x == 0) && (y == 0)) 
            rad = 0; 
         else 
            rad = pow((DBL) (x*x + y*y), -(h+1) / 2) * gauss();
         rcos = rad * cos(phase)*scale; 
         rsin = rad * sin(phase)*scale;
         Real(a, x, y) += rcos;
         Imag(a, x, y) += rsin;
         if (!((x == 0) && (y == 0))) 
            { 
            i0 = nx-x-1; 
            j0 = ny-y-1;
            Real(a, i0,j0) += rcos;
            Imag(a, i0,j0) += rsin;
            }
         }
      for (k=0;k<=rank;k++)    /* now handle quadrants 1 and 3 */
         {
         x = k; 
         y = rank;
         phase = 2 * M_PI * ((ran1() * 0x7FFF) / arand);
         if ((x == 0) && (y == 0)) 
            rad = 0; 
         else 
            rad = pow((DBL) (x*x + y*y), -(h+1) / 2) * gauss();
         rcos = rad * cos(phase)*scale; 
         rsin = rad * sin(phase)*scale;
         Real(a, x, ny-y-1) += rcos; 
         Imag(a, x, ny-y-1) += rsin;
         Real(a, nx-x-1, y) += rcos;
         Imag(a, nx-x-1, y) += rsin;

         x = rank; 
         y = k;
         phase = 2 * M_PI * ((ran1() * 0x7FFF) / arand);
         if ((x == 0) && (y == 0)) 
            rad = 0; 
         else 
            rad = pow((DBL) (x*x + y*y), -(h+1) / 2) * gauss();
         rcos = rad * cos(phase)*scale; 
         rsin = rad * sin(phase)*scale;
         Real(a, x, ny-y-1) += rcos; 
         Imag(a, x, ny-y-1) += rsin;
         Real(a, nx-x-1, y) += rcos;
         Imag(a, nx-x-1, y) += rsin;
         }
      }
   Imag(a, nx / 2, 0) = 0;
   Imag(a, 0, ny / 2) = 0;
   Imag(a, nx / 2, ny / 2) = 0;
   }


/*
 * ApplyFilter()  -- filter array of noise  with peak or notch filter
 *                since this happens before the inverse FFT, it is
 *                a frequency-domain filter.      
 *                f_type 1 bpass, -1 breject, 2 lopass, -2 hipass
 *
 */
void ApplyFilter(float *a, int n, DBL center, DBL Q, int f_type)
   {
   int i, j, i0, j0;
   DBL rad, fac, p, sfac;
   int xsize, ysize;

   prntf("Applying %s filter [%lf : %lf].\n", 
             (f_type > 0 ? (f_type ==  2 ? "Loop Pass": "Band Pass") : 
                           (f_type == -2 ? "High Pass": "Band Reject")),
                           center, Q);

   xsize = ysize = n;
   center = (center == 0.0 ? -0.00001 : center); // avoid a singularity

   sfac = 1.0/sqrt((DBL)(xsize*xsize/4 + ysize*ysize/4));  
   for (i = 0; i <= n / 2; i++) 
      for (j = 0; j <= n / 2; j++) 
         {
         rad = (i+j ? sqrt((DBL) (i * i + j * j)) * sfac : 0);
         p = 1.0 / pow(Q * center, 2);
         if (abs(f_type)==1)
            fac = p / (p + pow((1.0-rad/center),2)) ; /* bandpass/rej. */
         else
            fac = 1.0 / (1.0 + pow((rad/center),Q) ); /* lo/hi-pass */
         if (f_type < 0) 
            fac = (1.0 - fac);  /* invert filter */
         Real(a, i, j) *= fac;
         Imag(a, i, j) *= fac;
         i0 = (i == 0) ? 0 : n - i;
         j0 = (j == 0) ? 0 : n - j;
         Real(a, i0, j0) *= fac;
         Imag(a, i0, j0) *= fac;
         }
   Imag(a, n / 2, 0) = 0;
   Imag(a, 0, n / 2) = 0;
   Imag(a, n / 2, n / 2) = 0;
   for (i = 1; i <= n / 2 - 1; i++)  // do quadrants 1 and 3
      for (j = 1; j <= n / 2 - 1; j++) 
         {
         rad = sfac * sqrt((DBL) (i * i + j * j));
         p = 1.0 / pow(Q * center, 2);
         if (abs(f_type)==1)
            fac = p / (p + pow((1.0-rad/center),2)) ; /* bandpass/rej. */
         else
            fac = 1.0 / (1.0 + pow((rad/center),Q) ); /* lo/hi-pass */
         if (f_type < 0) 
            fac = (1.0 - fac);  /* invert filter */
         Real(a, i, n - j) *= fac;
         Imag(a, i, n - j) *= fac;
         Real(a, n - i, j) *= fac;
         Imag(a, n - i, j) *= fac;
         }
   }


/*
 * SPECTRALSYNTH  --  Spectrally  synthesised  fractal  motion in two
 *	         	       dimensions.  This algorithm is given under  the
 *		                name   SpectralSynthesisFM2D  on  page  108  of
 *       		       Peitgen & Saupe. 
 */
static void Synthesize(float **x, UINT n)
   {
   int i,j,nsize[2];
   unsigned long bl;
   float *a;


   prntf("Initializing %d x %d array.\n", n, n);

   bl = ((((unsigned long) n) * n) + 1) * 2 * sizeof(float);
   a = (float *) malloc( (size_t) (n+4)*(n+4)*2 * sizeof(float));
   if (a == (float *) 0) 
   	Error("Cannot allocate %d x %d result array (%lu bytes).\n", n, n, bl);

   *x = a;
   for (j=0;j<n;j++)   
      for (i=0;i<n;i++)   
	      Real(a,i,j) = Imag(a,i,j) = 0;

   for(i=0;i<iTURBCOUNT;i++) 
      ApplyTurbulence(a, n, 3.0-fTURB[i], fTSCALE[i]);   /* put 1/f noise into array */
   
   if (bBANDPASS) 
      ApplyFilter(a,n,fBP_CTR,fBP_QUAL,1);   /* bandpass filtering */
   if (bBANDREJECT) 
      ApplyFilter(a,n,fBR_CTR,fBR_QUAL,-1);  /* bandreject filtering */
   if (bLOWPASS) 
      ApplyFilter(a,n,fLP_CUT,fLP_ORDER,2);  /* lowpass filter */
   if (bHIGHPASS) 
      ApplyFilter(a,n,fHP_CUT,fHP_ORDER,-2); /* highpass filter */

   nsize[0] = nsize[1] = n;                  /* Dimension of domain array */

   prntf("Calculating inverse 2D FFT\n");
   fftn (2, nsize, &Real(a,0,0), &Imag(a,0,0), -2, 1.0);
   }



/*
 * WriteFile -- Write the pixmap from [n x n] elevation array. 
 */
static void WriteFile(float *a, UINT n)
   {
   int   i, j, ii, jj;
   int   xsize, ysize;
   float *rowdata;
	
   xsize = ysize = n;

   if (!(rowdata = (float *)malloc(xsize*sizeof(float)))) 
      Error("Could not allocate memory.\n");

   prntf("Saving file %s\n", szOUTFILE);

   fpOUTFILE = OpenTGA (szOUTFILE, iSIZE, iSIZE);
   for (j = 0; j < ysize; j++) 
      {
      jj = (bPEAK ? (ysize + j + iYSTART) % ysize : j);
	   for (i = 0; i < xsize; i++) 
         {
         ii = (bPEAK ? (xsize + i + iXSTART) % xsize : i);
	      rowdata[i] = Real(a,ii,jj);  /* rowdata holds this row */
	      }
      WriteTGARow(fpOUTFILE, iSIZE, rowdata);
	   }
   fclose(fpOUTFILE);
   }




/*
 * PLANET  --  Make the fBm landscape.  
 */
static BOOL GenerateLandscape(int iSIZE)
   {
   float *a = (float *) 0;
   int i, j;
   DBL r;
   UINT num_craters;
   int imax=0, jmax=0;

   initgauss(uSEED);

	Synthesize(&a, iSIZE); // generate array via IFFT on 1/f scaled noise
	if (a == (float *) 0) 
	   return FALSE;

	if (bLIMITS) 
	   rescale(&a, iSIZE, fLO_LIMIT, fHI_LIMIT);
   else 
	   rescale(&a, iSIZE, 0, 1);

	if (fPOWER != 1.0) 
      {
      prntf ("Raising to %lf power\n", fPOWER);
	   for (i = 0; i < iSIZE; i++) 
		   for (j = 0; j < iSIZE; j++) 
		      if ((r = Real(a, i, j)) != 0)
		         Real(a, i, j) = sgn1(r)*pow(fabs(r), fPOWER);
	   }
	rescale(&a, iSIZE, -1, 1);  // rescale all data to range [-1..1]
    
   if (bCRATERS) 
      {
	   num_craters = (UINT) (700 * pow(((DBL)iSIZE)/256,0.7)*fCRATER_DEN);
	   prntf("Adding %d craters of depth %lf\n", num_craters, fCRATER_SCALE);
	   distribute_craters(a, num_craters, iSIZE, bWRAP, fCRATER_SCALE);
      }
	rescale(&a, iSIZE, 0, 1);            /* rescale to 0..1 */
	FindPeak(a, iSIZE, &imax, &jmax);    /* find peak location */
	
	iXSTART = imax - (int)((float)iSIZE * fXFRAC);
	iYSTART = jmax - (int)((float)iSIZE * fYFRAC);

   WriteFile(a, iSIZE);
   if (a != (float *) 0) 
   	free((char *) a);
   return TRUE;
   }



/*
 *
 *
 */
static void Usage (void)
   {
   prntf ("HF   Height Field generator utility  v1.0    %s %s\n\n", __DATE__, __TIME__);
   prntf ("USAGE: HF [options] outfile[.tga]\n\n");
   prntf ("WHERE: [options] are 0 or more of:\n\n");
   prntf ("      /Size=d ....... d is x and y size of resulting image (2^n is good) [256]\n");
   prntf ("      /Turb=d:s ..... d is turbulence(0-3), scale(0-n) can specify up to 10 sets\n");
   prntf ("      /Craters=d:s .. d=density, s=size [none]\n");
   prntf ("      /Peak=x:y ..... Highest peak in this local x & y are pct (0-1)\n");
   prntf ("      /Seed=d ....... d is random number seed (0-32k) [uses system clock]\n");
   prntf ("      /Power=n ...... n is elevation change power (0-3 or so) [1.2]\n");
   prntf ("      /Limit=l:h .... RangeLimit l=low(0-1) h=high(0-1) [0,1] only if Power<>1\n");
   prntf ("      /BPF=c:q ...... BandPassFilter   c=center(0-1) q=quality(.5 - 20narrow)\n");
   prntf ("      /BRF=c:q ...... BandRejectFilter c=center(0-1) q=quality(.5broad - 20)\n");
   prntf ("      /LPF=c:o ...... LowPassFilter c=cutoff(0-1) o=order(1soft - 1000hard)\n");
   prntf ("      /HPF=c:o ...... HighPassFilter c=cutoff(0-1) o=order(1soft - 1000hard)\n");
   prntf ("      /Quiet ........ Operate Quietly. \n");
   prntf ("      /WrapOff ...... turn off wrapping ability \n");
   prntf ("      /NoRed ........ turn off Red channel \n");
   prntf ("      /NoGreen ...... turn off Green channel \n");
   prntf (" \n");
   prntf (" Turb specifies the turbulence of the landscape.  Small values like .5 give\n");
   prntf (" rolling hills. Large values like 2.5 give rough mountains.  Specify 1 for\n"); 
   prntf (" scale.  You can have more than one of these parameters to give secondary\n");
   prntf (" turbulence. Use a small scale value for successive entries.  For example\n");
   prntf (" using  /Turb=.5:1 /Turb=2.5:.1 gives rocky terrain on smooth slopes.\n");
   prntf (" \n");
   prntf (" Craters adds cratters to the landscape. density of 1 is moderate cratering.\n");
   prntf (" Height of 1 is a reasonable crater verticle size.\n");
   prntf (" \n");
   prntf (" Power is used to apply contrast in elevation heights.  Height values are \n");
   prntf (" raised to this exponent value before being clamped to [0..1].  Very low\n");
   prntf (" values like .25 give a pillow like appearence with occasional dips. Very high\n");
   prntf (" values like 3 or more cause solitary mountains in low plains. Limit can be\n");
   prntf (" used to clamp values before the power is applied to them.\n");

   exit (0);
   }


/*
 * main
 * ----
 */
int main(int argc, char **argv)
   {
   int   i;

   if (ArgBuildBlk (" *^Debug  *^Quiet   *^Size%  *^Turbulence%"
                    " *^Power% *^Peak%   *^Seed%  *^Craters%"
                    " *^Limit# *^WrapOff *^Type%  *^Version"
                    " *^BPF%   *^BRF%    *^LPF%   *^HPF% ?"
                    " *^NoRed  *^NoGreen"))
      Error ("%s", ArgGetErr ());

   if (ArgFillBlk (argv))
      Error ("%s", ArgGetErr ());

   if (ArgIs ("?") || !ArgIs (NULL) || argc < 2)
      Usage ();

   strcpy (szOUTFILE, ArgGet (NULL, 0));
   if (!strrchr (szOUTFILE, '.'))
      strcat (szOUTFILE, ".TGA");

   iSIZE  = (ArgIs ("Size")  ? atof (ArgGet ("Size", 0))  : 256.0);
   fPOWER = (ArgIs ("Power") ? atof (ArgGet ("Power", 0)) : 1.2);
   uSEED  = (ArgIs ("Seed")  ? atof (ArgGet ("Seed", 0))  : initseed ());
   bWRAP  = !ArgIs ("WrapOff");
   bQUIET = ArgIs ("Quiet");
   bNOR   = ArgIs ("NoRed");
   bNOG   = ArgIs ("NoGreen");

   printf ("Seed = %d\n", uSEED);

   if (bBANDPASS = ArgIs ("BPF"))
      TwoValue (ArgGet ("BPF", 0), &fBP_CTR, &fBP_QUAL, -1.0);
   if (bBANDREJECT = ArgIs ("BRF"))
      TwoValue (ArgGet ("BRF", 0), &fBR_CTR, &fBR_QUAL, -1.0);
   if (bLOWPASS    = ArgIs ("LPF"))
      TwoValue (ArgGet ("LPF", 0), &fLP_CUT, &fLP_ORDER, -1.0);
   if (bHIGHPASS   = ArgIs ("HPF"))
      TwoValue (ArgGet ("HPF", 0), &fHP_CUT, &fHP_ORDER, -1.0);
   if (bLIMITS     = ArgIs ("Limit"))
      TwoValue (ArgGet ("Limit", 0), &fLO_LIMIT, &fHI_LIMIT, 1);
   if (bPEAK       = ArgIs ("Peak"))
      TwoValue (ArgGet ("Peak", 0), &fXFRAC, &fYFRAC, -1);
   if (bCRATERS    = ArgIs ("Craters"))
      TwoValue (ArgGet ("Craters", 0), &fCRATER_DEN, &fCRATER_SCALE, -1);

   if (iTURBCOUNT = ArgIs ("Turbulence"))
      {
      for (i=0; i < (ArgIs ("Turbulence")) && i < 10; i++)
         TwoValue (ArgGet ("Turbulence", i), fTURB+i, fTSCALE+i, 1.0);
      }
   else
      {
   	fTURB[0]   = 2.15;
   	fTSCALE[0] = 1.0;
   	iTURBCOUNT = 1;
      }
   fac_warn(iSIZE); // warn if iSIZE has large prime factor

   if (!bWRAP && bPEAK)
      Error("/WrapOff and /Peak options are mutually exclusive\n");
    
   if (!bWRAP && !bCRATERS) 
       prntf("/WrapOff option applies only to craters");

   GenerateLandscape(iSIZE);
   return prntf ("Done.");
   }

