//test_sor_unroll.c
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include "sor_params.h"
#include "sor_routines.h"
#include "test_sor_unroll.h"


void test_sor_unroll() {
    float *p0 = (float *)malloc((IM+2)*(JM+2)*(KM+2)*sizeof(float));
    float *p1 = (float *)malloc((IM+2)*(JM+2)*(KM+2)*sizeof(float));
    float *rhs = (float *)malloc((IM+2)*(JM+2)*(KM+2)*sizeof(float));

    int iter, niters = 12/UNROLL;
    int i, j, k;

    for (i = 0; i < IM+2; i++) {
        for (j = 0; j < JM+2; j++) {
            for (k = 0; k < KM+2; k++) {
                rhs[INDEX(i, j, k)] = 1.0;
                p0[INDEX(i, j, k)] = 1.0;
            }
        }
    }

    clock_t start, end;
    double cpu_time_used;
    start = clock();

    for (iter = 1; iter <= niters; iter++) {
        printf("%d\n", iter);
        sor(p0, p1, rhs);
        for (i = 0; i < IM+2; i++) {
            for (j = 0; j < JM+2; j++) {
                for (k = 0; k < KM+2; k++) {
                    p0[INDEX(i, j, k)] = p1[INDEX(i, j, k)];
                }
            }
        }
    }

    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("%f\n", cpu_time_used);
    printf("%f\n", p0[INDEX(IM/2, JM/2, KM/2)]);

    free(p0);
    free(p1);
    free(rhs);
}


int main() {
    test_sor_unroll();
    return 0;
}
