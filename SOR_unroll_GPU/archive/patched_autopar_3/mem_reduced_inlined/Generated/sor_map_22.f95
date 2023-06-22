module singleton_module_sor_map_22

contains

subroutine sor_map_22(global_id_0,p0_s_0,rhs_0,p1_1)
use singleton_module_sor_map_22, only : sor_map_22_scal
 implicit none
!      BEGIN ex-sub decls sor_map_22_scal
 real, parameter :: cn1___sor_map_22_scal=0.333333333333333
 real, parameter :: cn2l___sor_map_22_scal=0.5
 real, parameter :: cn2s___sor_map_22_scal=0.5
 real, parameter :: cn3l___sor_map_22_scal=0.5
 real, parameter :: cn3s___sor_map_22_scal=0.5
 real, parameter :: cn4l___sor_map_22_scal=0.5
 real, parameter :: cn4s___sor_map_22_scal=0.5
 integer, parameter :: omega___sor_map_22_scal=1
!     ! Local vars: i,j,k,reltmp
 integer :: i___sor_map_22_scal
 integer :: j___sor_map_22_scal
 integer :: k___sor_map_22_scal
 real :: reltmp___sor_map_22_scal
!     ! ParallelFortran: Synthesised loop variable decls
 integer :: i_range___sor_map_22_scal
 integer :: j_range___sor_map_22_scal
 integer :: k_range___sor_map_22_scal
 integer :: i_rel___sor_map_22_scal
 integer :: j_rel___sor_map_22_scal
 integer :: k_rel___sor_map_22_scal
!      END ex-sub decls sor_map_22_scal
 integer, intent(in) :: global_id_0
 real, dimension(1:9), intent(in) :: p0_s_0
 real, intent(in) :: rhs_0
 real, intent(out) :: p1_1
!    ! Temp vars
!    ! Call to the original scalarised subroutine
!      BEGIN inlined call to sor_map_22_scal
!   READ
!   WRITTEN
!   READ & WRITTEN
!   globalIdDeclaration
!   globalIdInitialisation
!       call get_global_id(global_id_0,0)
!   ptrAssignments_fseq
!     ! ParallelFortran: Synthesised loop variables
 i_range___sor_map_22_scal = ((101 - 0) + 1)
 j_range___sor_map_22_scal = ((101 - 0) + 1)
 k_range___sor_map_22_scal = ((81 - 0) + 1)
 i_rel___sor_map_22_scal = (global_id_0 / (j_range___sor_map_22_scal * k_range___sor_map_22_scal))
 i___sor_map_22_scal = (i_rel___sor_map_22_scal + 0)
 j_rel___sor_map_22_scal = ((global_id_0 - (i_rel___sor_map_22_scal * (j_range___sor_map_22_scal *  &
      k_range___sor_map_22_scal))) / k_range___sor_map_22_scal)
 j___sor_map_22_scal = (j_rel___sor_map_22_scal + 0)
 k_rel___sor_map_22_scal = ((global_id_0 - (i_rel___sor_map_22_scal * (j_range___sor_map_22_scal *  &
      k_range___sor_map_22_scal))) - (j_rel___sor_map_22_scal * k_range___sor_map_22_scal))
 k___sor_map_22_scal = (k_rel___sor_map_22_scal + 0)
!     ! ParallelFortran: Original code
 if (i___sor_map_22_scal == 100 + 1) then
 p1_1 = p0_s_0(3)
else if ( i___sor_map_22_scal == 0 ) then
 p1_1 = p0_s_0(7)
else if ( j___sor_map_22_scal == 100 + 1 ) then
 p1_1 = p0_s_0(4)
else if ( j___sor_map_22_scal == 0 ) then
 p1_1 = p0_s_0(5)
 else if ( k___sor_map_22_scal == 0 ) then
 p1_1 = p0_s_0(5)
 else if ( k___sor_map_22_scal == 80 + 1 ) then
 p1_1 = p0_s_0(5)
 else
 reltmp___sor_map_22_scal = 1.0 * (1.0 / 3.0 * (0.5 * p0_s_0(6) + 0.5 * p0_s_0(4) + 0.5 * p0_s_0(8)  &
      + 0.5 * p0_s_0(2) + 0.5 * p0_s_0(9) + 0.5 * p0_s_0(1) - rhs_0) - p0_s_0(5))
 p1_1 = p0_s_0(5) + reltmp___sor_map_22_scal
 end if
!      END inlined call to sor_map_22_scal
end subroutine sor_map_22

end module singleton_module_sor_map_22

