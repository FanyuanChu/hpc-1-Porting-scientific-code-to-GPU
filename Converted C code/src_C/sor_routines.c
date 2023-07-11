#include <stdio.h>

// Constants from sor_params.f95
#define IM 1000
#define JM 1000
#define KM 320

void sor_kernel(float p0[IM+2][JM+2][KM+2], float p1[IM+2][JM+2][KM+2], float rhs[IM+2][JM+2][KM+2], int i, int j, int k) {
    float cn1 = 1.0/3.0;
    float cn2l = 0.5, cn2s = 0.5;
    float cn3l = 0.5, cn3s = 0.5;
    float cn4l = 0.5, cn4s = 0.5;
    float omega = 1.0;
    float reltmp;

    if (i == IM+1) {
        p1[i][j][k] = p0[i-IM][j][k];
    } else if (i == 0) {
        p1[i][j][k] = p0[i+IM][j][k];
    } else if (j == JM+1) {
        p1[i][j][k] = p0[i-1][j][k];
    } else if (j == 0) {
        p1[i][j][k] = p0[i][j][k];
    } else if (k == 0) {
        p1[i][j][k] = p0[i][j][k];
    } else if (k == KM+1) {
        p1[i][j][k] = p0[i][j][k];
    } else {
        reltmp = omega*(cn1 *(cn2l*p0[i+1][j][k] + cn2s*p0[i-1][j][k] + cn3l*p0[i][j+1][k] + cn3s*p0[i][j-1][k] + cn4l*p0[i][j][k+1] + cn4s*p0[i][j][k-1] -rhs[i][j][k])-p0[i][j][k]);
        p1[i][j][k] = p0[i][j][k] +reltmp;
    }
}

void sor(float p0[IM+2][JM+2][KM+2], float p1[IM+2][JM+2][KM+2], float rhs[IM+2][JM+2][KM+2]) {
    int i, j, k;
    for (i = 0; i <= IM+1; i++) {
        for (j = 0; j <= JM+1; j++) {
            for (k = 0; k <= KM+1; k++) {
                sor_kernel(p0, p1, rhs, i, j, k);
            }
        }
    }
}
