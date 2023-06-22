module singleton_module_sor_map_47
contains

subroutine sor_map_47_scal(global_id,p1_i_j_km1,p1_i_jm1_k,p1_im100_j_k,p1_im1_j_k,p1_i_j_k,p1_ip1_j_k,p1_ip100_j_k,p1_i_jp1_k,p1_i_j_kp1,rhs_i_j_k,p2_i_j_k)
      implicit none
      real, parameter :: cn1___sor_map_47_scal=0.333333333333333
      real, parameter :: cn2l___sor_map_47_scal=0.5
      real, parameter :: cn2s___sor_map_47_scal=0.5
      real, parameter :: cn3l___sor_map_47_scal=0.5
      real, parameter :: cn3s___sor_map_47_scal=0.5
      real, parameter :: cn4l___sor_map_47_scal=0.5
      real, parameter :: cn4s___sor_map_47_scal=0.5
      integer, parameter :: omega___sor_map_47_scal=1
!     ! Local vars: i,j,k,reltmp
 integer :: i___sor_map_47_scal
 integer :: j___sor_map_47_scal
 integer :: k___sor_map_47_scal
 real :: reltmp___sor_map_47_scal
!     ! ParallelFortran: Synthesised loop variable decls
 integer :: i_range___sor_map_47_scal
 integer :: j_range___sor_map_47_scal
 integer :: k_range___sor_map_47_scal
 integer :: i_rel___sor_map_47_scal
 integer :: j_rel___sor_map_47_scal
 integer :: k_rel___sor_map_47_scal
!   READ
 real, intent(in) :: p1_i_j_k
 real, intent(in) :: p1_i_j_km1
 real, intent(in) :: p1_i_j_kp1
 real, intent(in) :: p1_i_jm1_k
 real, intent(in) :: p1_i_jp1_k
 real, intent(in) :: p1_im100_j_k
 real, intent(in) :: p1_im1_j_k
 real, intent(in) :: p1_ip100_j_k
 real, intent(in) :: p1_ip1_j_k
 real, intent(in) :: rhs_i_j_k
!   WRITTEN
 real, intent(out) :: p2_i_j_k
!   READ & WRITTEN
!   globalIdDeclaration
 integer, intent(in) :: global_id
!   globalIdInitialisation
!       call get_global_id(global_id,0)
!   ptrAssignments_fseq
!     ! ParallelFortran: Synthesised loop variables
 i_range___sor_map_47_scal = ((101 - 0) + 1)
 j_range___sor_map_47_scal = ((101 - 0) + 1)
 k_range___sor_map_47_scal = ((81 - 0) + 1)
 i_rel___sor_map_47_scal = (global_id / (j_range___sor_map_47_scal * k_range___sor_map_47_scal))
 i___sor_map_47_scal = (i_rel___sor_map_47_scal + 0)
 j_rel___sor_map_47_scal = ((global_id - (i_rel___sor_map_47_scal * (j_range___sor_map_47_scal * k_range___sor_map_47_scal))) / k_range___sor_map_47_scal)
 j___sor_map_47_scal = (j_rel___sor_map_47_scal + 0)
 k_rel___sor_map_47_scal = ((global_id - (i_rel___sor_map_47_scal * (j_range___sor_map_47_scal * k_range___sor_map_47_scal))) - (j_rel___sor_map_47_scal * k_range___sor_map_47_scal))
 k___sor_map_47_scal = (k_rel___sor_map_47_scal + 0)
!     ! ParallelFortran: Original code
 if (i___sor_map_47_scal == 100 + 1) then
 p2_i_j_k = p1_im100_j_k
else if ( i___sor_map_47_scal == 0 ) then
 p2_i_j_k = p1_ip100_j_k
else if ( j___sor_map_47_scal == 100 + 1 ) then
 p2_i_j_k = p1_im1_j_k
else if ( j___sor_map_47_scal == 0 ) then
 p2_i_j_k = p1_i_j_k
 else if ( k___sor_map_47_scal == 0 ) then
 p2_i_j_k = p1_i_j_k
 else if ( k___sor_map_47_scal == 80 + 1 ) then
 p2_i_j_k = p1_i_j_k
 else
 reltmp___sor_map_47_scal = 1.0 * (1.0 / 3.0 * (0.5 * p1_ip1_j_k + 0.5 * p1_im1_j_k + 0.5 * p1_i_jp1_k + 0.5 *        p1_i_jm1_k + 0.5 * p1_i_j_kp1 + 0.5 * p1_i_j_km1 - rhs_i_j_k) - p1_i_j_k)
 p2_i_j_k = p1_i_j_k + reltmp___sor_map_47_scal
 end if
end subroutine sor_map_47_scal
end module singleton_module_sor_map_47
