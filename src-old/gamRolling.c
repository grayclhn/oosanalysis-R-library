/* C code to generate random variables from McCracken's
   'Gamma' for the expanding window */

#include <R.h>
#include <math.h>

void gamRolling(int *nDraws, int *nGrain, int *nAdditionalRegs, 
		  double *PRratio, double *gamDraws1, double *gamDraws2) {
  int i, j, k, lambdaScaled;
  double Wscaled, lambda;
  double dW[*nGrain];

  lambda = 1 / (1 + *PRratio);
  lambdaScaled = floor(lambda * *nGrain);

  // get the correct seed for the random number generator
  GetRNGstate();

  // loop over the simulations and the extra regressors
  for (i = 0; i < *nDraws; i++) {
    gamDraws1[i] = 0;
    gamDraws2[i] = 0;

    for (k = 0; k < *nAdditionalRegs; k++) {
      // create an array of the increments dW
      for (j = 0; j < *nGrain; j++) {
  	dW[j] = norm_rand() / sqrt(*nGrain);
      }

      // Initialize the first observations of the Browian Motion
      Wscaled = 0;
      for (j = 1; j < (lambdaScaled-1); j++) {
	Wscaled += dW[j];
      }
      Wscaled = Wscaled / lambda;

      // construct the Gamma stochastic integrals
      for (j = lambdaScaled - 1; j < *nGrain; j++) {
  	// calculate mccracken's gamma-1 and gamma-2
  	gamDraws1[i] = gamDraws1[i] + Wscaled * dW[j];
	gamDraws2[i] = gamDraws2[i] + pow(Wscaled, 2) / *nGrain;

	// add increment to brownian motion
	Wscaled = Wscaled + ((dW[j] - dW[j - lambdaScaled + 2]) / lambda);
      }
    }
  }
  // Release the random seed and exit.
  PutRNGstate();
}
