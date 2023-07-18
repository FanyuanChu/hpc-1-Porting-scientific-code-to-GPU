// sor_routines.c

#include "sor_params.h"
#include "sor_routines.h"

void sor(float *p0, float *p1, float *rhs) {
    int i, j, k;
    #pragma omp parallel for
    for (i = 0; i < IM+2; i++) {
        for (j = 0; j < JM+2; j++) {
            for (k = 0; k < KM+2; k++) {
                sor_kernel(p0, p1, rhs, i, j, k);
            }
        }
    }
}

void sor_kernel(float *p0, float *p1, float *rhs, int i, int j, int k) {
    float cn1 = 1.0/3.0, cn2l = 0.5, cn2s = 0.5, cn3l = 0.5, cn3s = 0.5, cn4l = 0.5, cn4s = 0.5;
    float omega = 1.0, reltmp;
    if (i == IM+1) {
        p1[INDEX(i,j,k)] = p0[INDEX(i-IM,j,k)];
    } else if (i == 0) {
        p1[INDEX(i,j,k)] = p0[INDEX(i+IM,j,k)];
    } else if (j == JM+1) {
        p1[INDEX(i,j,k)] = p0[INDEX(i-1,j,k)];
    } else if (j == 0) {
        p1[INDEX(i,j,k)] = p0[INDEX(i,j,k)];
    } else if (k == 0) {
        p1[INDEX(i,j,k)] = p0[INDEX(i,j,k)];
    } else if (k == KM+1) {
        p1[INDEX(i,j,k)] = p0[INDEX(i,j,k)];
    } else {
        reltmp = omega*(cn1 *(cn2l*p0[INDEX(i+1,j,k)] + cn2s*p0[INDEX(i-1,j,k)] + cn3l*p0[INDEX(i,j+1,k)] + cn3s*p0[INDEX(i,j-1,k)] + cn4l*p0[INDEX(i,j,k+1)] + cn4s*p0[INDEX(i,j,k-1)] -rhs[INDEX(i,j,k)])-p0[INDEX(i,j,k)]);
        p1[INDEX(i,j,k)] = p0[INDEX(i,j,k)] + reltmp;
    }
}
