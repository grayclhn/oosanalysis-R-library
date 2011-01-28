/* C code to generate random variables from McCracken's
   'Gamma' for the expanding window */

#include <R.h>
#include <math.h>

void gamExpanding(int *nDraws, int *nGrain, int *nAdditionalRegs, 
		  double *PRratio, double *gamDraws1, double *gamDraws2) {
  int i, j, k;
  double dW, W, WOverS, ROverT, sinv_j;

  ROverT = 1 / (1 + *PRratio);

  // get the correct seed for the random number generator
  GetRNGstate();
  for (i = 0; i < *nDraws; i++) {

    gamDraws1[i] = 0;
    gamDraws2[i] = 0;

    for (k = 0; k < *nAdditionalRegs; k++) {
      // Initialize the first observation of the Browian Motion
      W = 0;

      for (j = 0; j < floor(*nGrain * ROverT) - 1; j ++) {
  	dW = norm_rand() / sqrt(*nGrain);
	W = W + dW;
      }

      WOverS = W * ((double) *nGrain) / ((double) j);
      for (j = floor(*nGrain * ROverT) - 1; j < *nGrain; j++) {
	
  	// draw the random increment
  	dW = norm_rand() / sqrt(*nGrain);

	// add the weighted increment to the stochastic integral
  	gamDraws1[i] = gamDraws1[i] + (WOverS * dW);
	gamDraws2[i] = gamDraws2[i] + (pow(WOverS,2) / *nGrain);

	// update the weighted browian motion
	W = W + dW;
	WOverS = W * ((double) *nGrain) / ((double) j + 1);
      }
    }
  }
  // Release the random seed and exit.
  PutRNGstate();
}
