#include <math.h>

#define S1 { 103, 10405, 10407, 10506, 10507, 10508, 10607, 10609, 20911 }

typedef struct {
    int i_range;
    int j_range;
    int k_range;
    int i_rel;
    int j_rel;
    int k_rel;
    int i;
    int j;
    int k;
    double reltmp;
} sor_map;

void stage_kernel_1(int global_id_0, double* p0_0, double* rhs_0, double* p2_1) {
    const double cn1_sor_map_scal=0.333333333333333;
    const double cn2l_sor_map_scal=0.5;
    const double cn2s_sor_map_scal=0.5;
    const double cn3l_sor_map_scal=0.5;
    const double cn3s_sor_map_scal=0.5;
    const double cn4l_sor_map_scal=0.5;
    const double cn4s_sor_map_scal=0.5;
    const int omega_sor_map_scal=1;

    sor_map sor_map_21_scal, sor_map_45_scal;
    double p1_s_0_f_comp_p2_1_2[9];
    double rhs_0_f_comp_p2_1_2;
    int s1[] = S1;
    double svec_p2_1_3[9][9];
    double svec_p2_1_4[9];
    int idx = global_id_0;
    int s_idx_1, s_idx_2;
    int i_f_maps_p2_1_0;

    // RF4A Begin Inline
    for (s_idx_1 = 0; s_idx_1 < 9; ++s_idx_1) {
        for (s_idx_2 = 0; s_idx_2 < 9; ++s_idx_2) {
            if (idx+s1[s_idx_1]+s1[s_idx_2] >= 0 && idx+s1[s_idx_1]+s1[s_idx_2] <= 853127) {
                svec_p2_1_3[s_idx_1][s_idx_2] = p0_0[idx+s1[s_idx_1]+s1[s_idx_2]];
            } else {
                svec_p2_1_3[s_idx_1][s_idx_2] = p0_0[idx];
            }
        }
    }

    for (s_idx_1 = 0; s_idx_1 < 9; ++s_idx_1) {
        if (idx+s1[s_idx_1] >= 0 && idx+s1[s_idx_1] <= 853127) {
            svec_p2_1_4[s_idx_1] = rhs_0[idx+s1[s_idx_1]];
        } else {
            svec_p2_1_4[s_idx_1] = rhs_0[idx];
        }
    }

    // RF4A End Inline
    for (i_f_maps_p2_1_0 = 0; i_f_maps_p2_1_0 < 9; ++i_f_maps_p2_1_0) {
        p1_s_0_f_comp_p2_1_2[i_f_maps_p2_1_0] = cn4s_sor_map_scal * svec_p2_1_4[i_f_maps_p2_1_0] - cn4l_sor_map_scal * svec_p2_1_3[i_f_maps_p2_1_0][i_f_maps_p2_1_0];
    }

    rhs_0_f_comp_p2_1_2 = svec_p2_1_4[4] + svec_p2_1_3[4][4] * cn3s_sor_map_scal;

    for (i_f_maps_p2_1_0 = 0; i_f_maps_p2_1_0 < 9; ++i_f_maps_p2_1_0) {
        p1_s_0_f_comp_p2_1_2[i_f_maps_p2_1_0] -= svec_p2_1_3[i_f_maps_p2_1_0][4] * rhs_0_f_comp_p2_1_2;
    }

    p1_s_0_f_comp_p2_1_2[4] = rhs_0_f_comp_p2_1_2;

    for (i_f_maps_p2_1_0 = 0; i_f_maps_p2_1_0 < 9; ++i_f_maps_p2_1_0) {
        p2_1[idx+s1[i_f_maps_p2_1_0]] = omega_sor_map_scal * p1_s_0_f_comp_p2_1_2[i_f_maps_p2_1_0] + (1-omega_sor_map_scal) * p0_0[idx+s1[i_f_maps_p2_1_0]];
    }
}

