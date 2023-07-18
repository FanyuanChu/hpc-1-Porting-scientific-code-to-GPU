// sor_routines.h
#include "sor_params.h"
#include <omp.h>

void sor(float *p0, float *p1, float *rhs);
void sor_kernel(float *p0, float *p1, float *rhs, int i, int j, int k);



