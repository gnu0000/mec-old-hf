/*--------------------------------*-C-*---------------------------------*
 * File:
 *	fft.h
 *
 * ---------------------------------------------------------------------*
 * Re[]:	real value array
 * Im[]:	imaginary value array
 * nTotal:	total number of complex values
 * nPass:	number of elements involved in this pass of transform
 * nSpan:	nspan/nPass = number of bytes to increment pointer
 *		in Re[] and Im[]
 * isign:	exponent: +1= fwd transform  -1= rev transform
 * max_factors:	maximum value of any prime factor
 *    max_perm:	maximum number of prime factors
 * ----------------------------------------------------------------------
 * See the comments in the code for correct usage!
 */
#ifndef _FFT_H
#define _FFT_H

#define REAL float

extern void fft_free (void);

int fftn (int ndim, const int dims[],
         REAL Re [], REAL Im [],
         int iSign, double scaling);

/* float precision routine */
int fftradix (REAL Re[], REAL Im[],
              size_t nTotal, size_t nPass, size_t nSpan,
              int iSign, int max_factors, int max_perm);

#endif
