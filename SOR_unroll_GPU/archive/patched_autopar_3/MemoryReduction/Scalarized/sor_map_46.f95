module singleton_module_sor_map_46

contains

subroutine sor_map_46_scal(global_id,p1_i_j_km1,p1_i_jm1_k,p1_im100_j_k,p1_im1_j_k,p1_i_j_k, &
      p1_ip1_j_k,p1_ip100_j_k,p1_i_jp1_k,p1_i_j_kp1,rhs_i_j_k,p2_i_j_k)
 real, parameter :: cn1=0.333333333333333
 real, parameter :: cn2l=0.5
 real, parameter :: cn2s=0.5
 real, parameter :: cn3l=0.5
 real, parameter :: cn3s=0.5
 real, parameter :: cn4l=0.5
 real, parameter :: cn4s=0.5
 integer, parameter :: omega=1
!    ! Local vars: i,j,k,reltmp
 integer :: i
 integer :: j
 integer :: k
 real :: reltmp
!    ! ParallelFortran: Synthesised loop variable decls
 integer :: i_range
 integer :: j_range
 integer :: k_range
 integer :: i_rel
 integer :: j_rel
 integer :: k_rel
!  READ
 real, intent(In) :: p1_i_j_k
 real, intent(In) :: p1_i_j_km1
 real, intent(In) :: p1_i_j_kp1
 real, intent(In) :: p1_i_jm1_k
 real, intent(In) :: p1_i_jp1_k
 real, intent(In) :: p1_im100_j_k
 real, intent(In) :: p1_im1_j_k
 real, intent(In) :: p1_ip100_j_k
 real, intent(In) :: p1_ip1_j_k
 real, intent(In) :: rhs_i_j_k
!  WRITTEN
 real, intent(Out) :: p2_i_j_k
!  READ & WRITTEN
!  globalIdDeclaration
 integer, intent(in) :: global_id
!  globalIdInitialisation
!      call get_global_id(global_id,0)
!  ptrAssignments_fseq
!    ! ParallelFortran: Synthesised loop variables
 i_range = ((101 - 0) + 1)
 j_range = ((101 - 0) + 1)
 k_range = ((81 - 0) + 1)
 i_rel = (global_id / (j_range * k_range))
 i = (i_rel + 0)
 j_rel = ((global_id - (i_rel * (j_range * k_range))) / k_range)
 j = (j_rel + 0)
 k_rel = ((global_id - (i_rel * (j_range * k_range))) - (j_rel * k_range))
 k = (k_rel + 0)
!    ! ParallelFortran: Original code
 if (i == 100 + 1) then
 p2_i_j_k = p1_im100_j_k
else if ( i == 0 ) then
 p2_i_j_k = p1_ip100_j_k
else if ( j == 100 + 1 ) then
 p2_i_j_k = p1_im1_j_k
else if ( j == 0 ) then
 p2_i_j_k = p1_i_j_k
 else if ( k == 0 ) then
 p2_i_j_k = p1_i_j_k
 else if ( k == 80 + 1 ) then
 p2_i_j_k = p1_i_j_k
 else
 reltmp = 1.0 * (1.0 / 3.0 * (0.5 * p1_ip1_j_k + 0.5 * p1_im1_j_k + 0.5 * p1_i_jp1_k + 0.5 *  &
      p1_i_jm1_k + 0.5 * p1_i_j_kp1 + 0.5 * p1_i_j_km1 - rhs_i_j_k) - p1_i_j_k)
 p2_i_j_k = p1_i_j_k + reltmp
 end if
end subroutine sor_map_46_scal

end module singleton_module_sor_map_46

