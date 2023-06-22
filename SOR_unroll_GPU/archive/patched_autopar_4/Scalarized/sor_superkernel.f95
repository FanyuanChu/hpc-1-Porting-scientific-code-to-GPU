module singleton_module_sor_superkernel

contains

subroutine sor_superkernel_scal(global_id,p0,rhs,p4,state_ptr)
 integer :: i
 integer :: i_rel
 integer :: j
 integer :: j_range
 integer :: j_rel
 integer :: k
 integer :: k_range
 integer :: k_rel
 real, dimension(0:101,0:101,0:81), intent(in) :: p0
 real, dimension(0:101,0:101,0:81), intent(in) :: p0
 real, dimension(0:101,0:101,0:81), intent(in) :: p0
 real, dimension(0:101,0:101,0:81), intent(in) :: p0
 real, dimension(0:101,0:101,0:81), intent(in) :: p0
 real, dimension(0:101,0:101,0:81), intent(in) :: p0
 real, dimension(0:101,0:101,0:81), intent(in) :: p0
 real, dimension(0:101,0:101,0:81), intent(in) :: p0
 real, dimension(0:101,0:101,0:81), intent(in) :: p0
 real, dimension(0:101,0:101,0:81), intent(in) :: p1
 real, dimension(0:101,0:101,0:81), intent(in) :: p1
 real, dimension(0:101,0:101,0:81), intent(in) :: p1
 real, dimension(0:101,0:101,0:81), intent(in) :: p1
 real, dimension(0:101,0:101,0:81), intent(in) :: p1
 real, dimension(0:101,0:101,0:81), intent(in) :: p1
 real, dimension(0:101,0:101,0:81), intent(in) :: p1
 real, dimension(0:101,0:101,0:81), intent(in) :: p1
 real, dimension(0:101,0:101,0:81), intent(in) :: p1
 real, dimension(0:101,0:101,0:81), intent(in) :: p2
 real, dimension(0:101,0:101,0:81), intent(in) :: p2
 real, dimension(0:101,0:101,0:81), intent(in) :: p2
 real, dimension(0:101,0:101,0:81), intent(in) :: p2
 real, dimension(0:101,0:101,0:81), intent(in) :: p2
 real, dimension(0:101,0:101,0:81), intent(in) :: p2
 real, dimension(0:101,0:101,0:81), intent(in) :: p2
 real, dimension(0:101,0:101,0:81), intent(in) :: p2
 real, dimension(0:101,0:101,0:81), intent(in) :: p2
 real, dimension(0:101,0:101,0:81), intent(in) :: p3
 real, dimension(0:101,0:101,0:81), intent(in) :: p3
 real, dimension(0:101,0:101,0:81), intent(in) :: p3
 real, dimension(0:101,0:101,0:81), intent(in) :: p3
 real, dimension(0:101,0:101,0:81), intent(in) :: p3
 real, dimension(0:101,0:101,0:81), intent(in) :: p3
 real, dimension(0:101,0:101,0:81), intent(in) :: p3
 real, dimension(0:101,0:101,0:81), intent(in) :: p3
 real, dimension(0:101,0:101,0:81), intent(in) :: p3
 real, dimension(0:101,0:101,0:81), intent(out) :: p4
 real, dimension(0:101,0:101,0:81), intent(in) :: rhs
 integer, intent(in) :: global_id
!  use module_sor_superkernel_init
 real, dimension(0:101,0:101,0:81), intent(in) :: p0
 real, dimension(0:101,0:101,0:81), intent(in) :: rhs
 real, dimension(0:101,0:101,0:81) :: p1
 real, dimension(0:101,0:101,0:81) :: p2
 real, dimension(0:101,0:101,0:81) :: p3
 real, dimension(0:101,0:101,0:81), intent(out) :: p4
 integer :: state
 integer, intent(In) :: state_ptr
 integer, parameter :: st_sor_superkernel_map_23=0
 integer, parameter :: st_sor_superkernel_map_47=1
 integer, parameter :: st_sor_superkernel_map_71=2
 integer, parameter :: st_sor_superkernel_map_95=3
 state = state_ptr 
!  SUPERKERNEL BODY
 select case(state)
 case (st_sor_superkernel_map_23)
 j_range = ((101 - 0) + 1)
 k_range = ((81 - 0) + 1)
 i_rel = (global_id / (j_range * k_range))
 i = (i_rel + 0)
 j_rel = ((global_id - (i_rel * (j_range * k_range))) / k_range)
 j = (j_rel + 0)
 k_rel = ((global_id - (i_rel * (j_range * k_range))) - (j_rel * k_range))
 k = (k_rel + 0)
call sor_map_23(global_id,p0_i_j_km1,p0_i_jm1_k,p0_im100_j_k,p0_im1_j_k,p0_i_j_k,p0_ip1_j_k, &
      p0_ip100_j_k,p0_i_jp1_k,p0_i_j_kp1,rhs_i_j_k,p1_i_j_k)
 case (st_sor_superkernel_map_47)
 j_range = ((101 - 0) + 1)
 k_range = ((81 - 0) + 1)
 i_rel = (global_id / (j_range * k_range))
 i = (i_rel + 0)
 j_rel = ((global_id - (i_rel * (j_range * k_range))) / k_range)
 j = (j_rel + 0)
 k_rel = ((global_id - (i_rel * (j_range * k_range))) - (j_rel * k_range))
 k = (k_rel + 0)
call sor_map_47(global_id,p1_i_j_km1,p1_i_jm1_k,p1_im100_j_k,p1_im1_j_k,p1_i_j_k,p1_ip1_j_k, &
      p1_ip100_j_k,p1_i_jp1_k,p1_i_j_kp1,rhs_i_j_k,p2_i_j_k)
 case (st_sor_superkernel_map_71)
 j_range = ((101 - 0) + 1)
 k_range = ((81 - 0) + 1)
 i_rel = (global_id / (j_range * k_range))
 i = (i_rel + 0)
 j_rel = ((global_id - (i_rel * (j_range * k_range))) / k_range)
 j = (j_rel + 0)
 k_rel = ((global_id - (i_rel * (j_range * k_range))) - (j_rel * k_range))
 k = (k_rel + 0)
call sor_map_71(global_id,p2_i_j_km1,p2_i_jm1_k,p2_im100_j_k,p2_im1_j_k,p2_i_j_k,p2_ip1_j_k, &
      p2_ip100_j_k,p2_i_jp1_k,p2_i_j_kp1,rhs_i_j_k,p3_i_j_k)
 case (st_sor_superkernel_map_95)
 j_range = ((101 - 0) + 1)
 k_range = ((81 - 0) + 1)
 i_rel = (global_id / (j_range * k_range))
 i = (i_rel + 0)
 j_rel = ((global_id - (i_rel * (j_range * k_range))) / k_range)
 j = (j_rel + 0)
 k_rel = ((global_id - (i_rel * (j_range * k_range))) - (j_rel * k_range))
 k = (k_rel + 0)
call sor_map_95(global_id,p3_i_j_km1,p3_i_jm1_k,p3_im100_j_k,p3_im1_j_k,p3_i_j_k,p3_ip1_j_k, &
      p3_ip100_j_k,p3_i_jp1_k,p3_i_j_kp1,rhs_i_j_k,p4_i_j_k)
 end select
end subroutine sor_superkernel

end module singleton_module_sor_superkernel

