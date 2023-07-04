#include <stdio.h>

#define CN1___SOR_MAP_22_SCAL 0.333333333333333
#define CN2L___SOR_MAP_22_SCAL 0.5
#define CN2S___SOR_MAP_22_SCAL 0.5
#define CN3L___SOR_MAP_22_SCAL 0.5
#define CN3S___SOR_MAP_22_SCAL 0.5
#define CN4L___SOR_MAP_22_SCAL 0.5
#define CN4S___SOR_MAP_22_SCAL 0.5
#define OMEGA___SOR_MAP_22_SCAL 1
#define CN1___SOR_MAP_46_SCAL 0.333333333333333
#define CN2L___SOR_MAP_46_SCAL 0.5
#define CN2S___SOR_MAP_46_SCAL 0.5
#define CN3L___SOR_MAP_46_SCAL 0.5
#define CN3S___SOR_MAP_46_SCAL 0.5
#define CN4L___SOR_MAP_46_SCAL 0.5
#define CN4S___SOR_MAP_46_SCAL 0.5
#define OMEGA___SOR_MAP_46_SCAL 1
#define CN1___SOR_MAP_70_SCAL 0.333333333333333
#define CN2L___SOR_MAP_70_SCAL 0.5
#define CN2S___SOR_MAP_70_SCAL 0.5
#define CN3L___SOR_MAP_70_SCAL 0.5
#define CN3S___SOR_MAP_70_SCAL 0.5
#define CN4L___SOR_MAP_70_SCAL 0.5
#define CN4S___SOR_MAP_70_SCAL 0.5
#define OMEGA___SOR_MAP_70_SCAL 1


void stage_kernel_1(int global_id_0, float* p0_0, float* rhs_0, float* p3_1) {
    int i___sor_map_70_scal;
    int j___sor_map_70_scal;
    int k___sor_map_70_scal;
    float reltmp___sor_map_70_scal;
    int i_range___sor_map_70_scal;
    int j_range___sor_map_70_scal;
    int k_range___sor_map_70_scal;
    int i_rel___sor_map_70_scal;
    int j_rel___sor_map_70_scal;
    int k_rel___sor_map_70_scal;


    float p0_0[853129];
    float rhs_0[853129];
    float p3_1[853129];
  
