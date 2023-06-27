struct sor_routines {

void sor(float p0[im+2][jm+2][km+2], float p1[im+2][jm+2][km+2], float rhs[im+2][jm+2][km+2]) {
    int i, j, k;
    #pragma omp parallel for private(i, j, k)
    for(i = 0; i <= im+1; i++) {
        for(j = 0; j <= jm+1; j++) {
            for(k = 0; k <= km+1; k++) {
                sor_kernel(p0, p1, rhs, i, j, k);
            }
        }
    }
}

void sor_kernel(float p0[im+2][jm+2][km+2], float p1[im+2][jm+2][km+2], float rhs[im+2][jm+2][km+2], int i, int j, int k) {
    float cn1 = 1.0/3.0;
    float cn2l = 0.5, cn2s = 0.5;
    float cn3l = 0.5, cn3s = 0.5;
    float cn4l = 0.5, cn4s = 0.5;
    float omega = 1.0;
    float reltmp;

    if (i==im+1) {
        p1[i][j][k] = p0[i-im][j][k];
    } else if (i==0) {
        p1[i][j][k] = p0[i+im][j][k];
    } else if (j==jm+1) {
        p1[i][j][k]=p0[i-1][j][k];
    } else if (j==0) {
        p1[i][j][k]=p0[i][j][k];
    } else if (k==0) {
        p1[i][j][k]=p0[i][j][k];
    } else if (k==km+1) {
        p1[i][j][k]=p0[i][j][k];
    } else {
        reltmp = omega*(cn1 *(cn2l*p0[i+1][j][k] + cn2s*p0[i-1][j][k] + cn3l*p0[i][j+1][k] + cn3s*p0[i][j-1][k] + cn4l*p0[i][j][k+1] + cn4s*p0[i][j][k-1] - rhs[i][j][k]) - p0[i][j][k]);
        p1[i][j][k] = p0[i][j][k] + reltmp;    
    }
}

};
