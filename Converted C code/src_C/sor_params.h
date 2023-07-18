// sor_params.h
#define IM 1000
#define JM 1000
#define KM 320
#define UNROLL 4
#define INDEX(i, j, k) ((i)*((JM+2)*(KM+2)) + (j)*(KM+2) + (k))