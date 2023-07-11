#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Constants from sor_params.f95
#define IM 1000
#define JM 1000
#define KM 320
#define UNROLL 4

// sor and sor_kernel functions from sor_routines.c
// ...
void sor(float p0[IM+2][JM+2][KM+2], float p1[IM+2][JM+2][KM+2], float rhs[IM+2][JM+2][KM+2]);


int main() {
    // Allocate memory for arrays
    float ***p0, ***p1, ***rhs;
    p0 = malloc((IM+2) * sizeof(*p0));
    p1 = malloc((IM+2) * sizeof(*p1));
    rhs = malloc((IM+2) * sizeof(*rhs));
    for (int i = 0; i <= IM+1; i++) {
        p0[i] = malloc((JM+2) * sizeof(**p0));
        p1[i] = malloc((JM+2) * sizeof(**p1));
        rhs[i] = malloc((JM+2) * sizeof(**rhs));
        for (int j = 0; j <= JM+1; j++) {
            p0[i][j] = malloc((KM+2) * sizeof(***p0));
            p1[i][j] = malloc((KM+2) * sizeof(***p1));
            rhs[i][j] = malloc((KM+2) * sizeof(***rhs));
        }
    }

    // Initialize rhs and p0
    for (int i = 0; i <= IM+1; i++) {
        for (int j = 0; j <= JM+1; j++) {
            for (int k = 0; k <= KM+1; k++) {
                rhs[i][j][k] = 1.0;
                p0[i][j][k] = 1.0;
            }
        }
    }

    // Loop over iterations
    clock_t begin = clock();
    for (int iter = 0; iter < 12/UNROLL; iter++) {
        printf("Iteration: %d\n", iter);
        sor(p0, p1, rhs);
        // Copy p1 to p0
        for (int i = 0; i <= IM+1; i++) {
            for (int j = 0; j <= JM+1; j++) {
                for (int k = 0; k <= KM+1; k++) {
                    p0[i][j][k] = p1[i][j][k];
                }
            }
        }
    }
    clock_t end = clock();
    double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

    // Print result and time spent
    printf("%f\n", p0[IM/2][JM/2][KM/2]);
    printf("Time spent: %f\n", time_spent);

    // Deallocate memory
    for (int i = 0; i <= IM+1; i++) {
        for (int j = 0; j <= JM+1; j++) {
            free(p0[i][j]);
            free(p1[i][j]);
            free(rhs[i][j]);
        }
        free(p0[i]);
        free(p1[i]);
        free(rhs[i]);
    }
    free(p0);
    free(p1);
    free(rhs);

    return 0;
}
