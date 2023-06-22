module module_sor_superkernel_superkernel


    contains


subroutine sor_superkernel_map_23(p0,rhs,p1)

     real( 4), parameter :: cn1 = 0.333333333333333
     real( 4), parameter :: cn2l = 0.5
     real( 4), parameter :: cn2s = 0.5
     real( 4), parameter :: cn3l = 0.5
     real( 4), parameter :: cn3s = 0.5
     real( 4), parameter :: cn4l = 0.5
     real( 4), parameter :: cn4s = 0.5
     real, parameter :: omega = 1
    ! Local vars: i,j,k,reltmp
    integer :: i
    integer :: j
    integer :: k
    real :: reltmp
    ! ParallelFortran: Synthesised loop variable decls
    integer :: i_range
    integer :: j_range
    integer :: k_range
    integer :: i_rel
    integer :: j_rel
    integer :: k_rel
! READ
    real, dimension(0:101,0:101,0:81), intent(In) :: p0
    real, dimension(0:101,0:101,0:81), intent(In) :: rhs
! WRITTEN
    real, dimension(0:101,0:101,0:81), intent(Out) :: p1
! READ & WRITTEN
! globalIdDeclaration
    integer :: global_id
! globalIdInitialisation
    call get_global_id(global_id,0)
! ptrAssignments_fseq

    ! ParallelFortran: Synthesised loop variables
    i_range = ((101 - 0) + 1)
    j_range = ((101 - 0) + 1)
    k_range = ((81 - 0) + 1)
    i_rel = (global_id / (j_range * k_range))
    i = (i_rel + 0)
    j_rel = ((global_id - (i_rel * (j_range * k_range))) / k_range)
    j = (j_rel + 0)
    k_rel = ((global_id - (i_rel * (j_range * k_range))) - (j_rel * k_range))
    k = (k_rel + 0)


    ! ParallelFortran: Original code
if ( i == 100 + 1 ) then
p1(i,j,k) = p0(i - 100,j,k)
else if ( i == 0 ) then
p1(i,j,k) = p0(i + 100,j,k)
else if ( j == 100 + 1 ) then
p1(i,j,k) = p0(i - 1,j,k)
else if ( j == 0 ) then
 p1(i,j,k) = p0(i,j,k)
 else if ( k == 0 ) then
 p1(i,j,k) = p0(i,j,k)
 else if ( k == 80 + 1 ) then
 p1(i,j,k) = p0(i,j,k)
 else
 reltmp = 1.0 * (1.0 / 3.0 * (0.5 * p0(i + 1,j,k) + 0.5 * p0(i - 1,j,k) + 0.5 * p0(i,j + 1, &
      k) + 0.5 * p0(i,j - 1,k) + 0.5 * p0(i,j,k + 1) + 0.5 * p0(i,j,k - 1) - rhs(i,j,k)) - p0(i,j, &
      k))
 p1(i,j,k) = p0(i,j,k) + reltmp
 end if

end subroutine sor_superkernel_map_23


subroutine sor_superkernel_map_47(p1,rhs,p2)

     real( 4), parameter :: cn1 = 0.333333333333333
     real( 4), parameter :: cn2l = 0.5
     real( 4), parameter :: cn2s = 0.5
     real( 4), parameter :: cn3l = 0.5
     real( 4), parameter :: cn3s = 0.5
     real( 4), parameter :: cn4l = 0.5
     real( 4), parameter :: cn4s = 0.5
     real, parameter :: omega = 1
    ! Local vars: i,j,k,reltmp
    integer :: i
    integer :: j
    integer :: k
    real :: reltmp
    ! ParallelFortran: Synthesised loop variable decls
    integer :: i_range
    integer :: j_range
    integer :: k_range
    integer :: i_rel
    integer :: j_rel
    integer :: k_rel
! READ
    real, dimension(0:101,0:101,0:81), intent(In) :: p1
    real, dimension(0:101,0:101,0:81), intent(In) :: rhs
! WRITTEN
    real, dimension(0:101,0:101,0:81), intent(Out) :: p2
! READ & WRITTEN
! globalIdDeclaration
    integer :: global_id
! globalIdInitialisation
    call get_global_id(global_id,0)
! ptrAssignments_fseq

    ! ParallelFortran: Synthesised loop variables
    i_range = ((101 - 0) + 1)
    j_range = ((101 - 0) + 1)
    k_range = ((81 - 0) + 1)
    i_rel = (global_id / (j_range * k_range))
    i = (i_rel + 0)
    j_rel = ((global_id - (i_rel * (j_range * k_range))) / k_range)
    j = (j_rel + 0)
    k_rel = ((global_id - (i_rel * (j_range * k_range))) - (j_rel * k_range))
    k = (k_rel + 0)


    ! ParallelFortran: Original code
if ( i == 100 + 1 ) then
p2(i,j,k) = p1(i - 100,j,k)
else if ( i == 0 ) then
p2(i,j,k) = p1(i + 100,j,k)
else if ( j == 100 + 1 ) then
p2(i,j,k) = p1(i - 1,j,k)
else if ( j == 0 ) then
 p2(i,j,k) = p1(i,j,k)
 else if ( k == 0 ) then
 p2(i,j,k) = p1(i,j,k)
 else if ( k == 80 + 1 ) then
 p2(i,j,k) = p1(i,j,k)
 else
 reltmp = 1.0 * (1.0 / 3.0 * (0.5 * p1(i + 1,j,k) + 0.5 * p1(i - 1,j,k) + 0.5 * p1(i,j + 1, &
      k) + 0.5 * p1(i,j - 1,k) + 0.5 * p1(i,j,k + 1) + 0.5 * p1(i,j,k - 1) - rhs(i,j,k)) - p1(i,j, &
      k))
 p2(i,j,k) = p1(i,j,k) + reltmp
 end if

end subroutine sor_superkernel_map_47


subroutine sor_superkernel_map_71(p2,rhs,p3)

     real( 4), parameter :: cn1 = 0.333333333333333
     real( 4), parameter :: cn2l = 0.5
     real( 4), parameter :: cn2s = 0.5
     real( 4), parameter :: cn3l = 0.5
     real( 4), parameter :: cn3s = 0.5
     real( 4), parameter :: cn4l = 0.5
     real( 4), parameter :: cn4s = 0.5
     real, parameter :: omega = 1
    ! Local vars: i,j,k,reltmp
    integer :: i
    integer :: j
    integer :: k
    real :: reltmp
    ! ParallelFortran: Synthesised loop variable decls
    integer :: i_range
    integer :: j_range
    integer :: k_range
    integer :: i_rel
    integer :: j_rel
    integer :: k_rel
! READ
    real, dimension(0:101,0:101,0:81), intent(In) :: p2
    real, dimension(0:101,0:101,0:81), intent(In) :: rhs
! WRITTEN
    real, dimension(0:101,0:101,0:81), intent(Out) :: p3
! READ & WRITTEN
! globalIdDeclaration
    integer :: global_id
! globalIdInitialisation
    call get_global_id(global_id,0)
! ptrAssignments_fseq

    ! ParallelFortran: Synthesised loop variables
    i_range = ((101 - 0) + 1)
    j_range = ((101 - 0) + 1)
    k_range = ((81 - 0) + 1)
    i_rel = (global_id / (j_range * k_range))
    i = (i_rel + 0)
    j_rel = ((global_id - (i_rel * (j_range * k_range))) / k_range)
    j = (j_rel + 0)
    k_rel = ((global_id - (i_rel * (j_range * k_range))) - (j_rel * k_range))
    k = (k_rel + 0)


    ! ParallelFortran: Original code
if ( i == 100 + 1 ) then
p3(i,j,k) = p2(i - 100,j,k)
else if ( i == 0 ) then
p3(i,j,k) = p2(i + 100,j,k)
else if ( j == 100 + 1 ) then
p3(i,j,k) = p2(i - 1,j,k)
else if ( j == 0 ) then
 p3(i,j,k) = p2(i,j,k)
 else if ( k == 0 ) then
 p3(i,j,k) = p2(i,j,k)
 else if ( k == 80 + 1 ) then
 p3(i,j,k) = p2(i,j,k)
 else
 reltmp = 1.0 * (1.0 / 3.0 * (0.5 * p2(i + 1,j,k) + 0.5 * p2(i - 1,j,k) + 0.5 * p2(i,j + 1, &
      k) + 0.5 * p2(i,j - 1,k) + 0.5 * p2(i,j,k + 1) + 0.5 * p2(i,j,k - 1) - rhs(i,j,k)) - p2(i,j, &
      k))
 p3(i,j,k) = p2(i,j,k) + reltmp
 end if

end subroutine sor_superkernel_map_71


subroutine sor_superkernel_map_95(p3,rhs,p4)

     real( 4), parameter :: cn1 = 0.333333333333333
     real( 4), parameter :: cn2l = 0.5
     real( 4), parameter :: cn2s = 0.5
     real( 4), parameter :: cn3l = 0.5
     real( 4), parameter :: cn3s = 0.5
     real( 4), parameter :: cn4l = 0.5
     real( 4), parameter :: cn4s = 0.5
     real, parameter :: omega = 1
    ! Local vars: i,j,k,reltmp
    integer :: i
    integer :: j
    integer :: k
    real :: reltmp
    ! ParallelFortran: Synthesised loop variable decls
    integer :: i_range
    integer :: j_range
    integer :: k_range
    integer :: i_rel
    integer :: j_rel
    integer :: k_rel
! READ
    real, dimension(0:101,0:101,0:81), intent(In) :: p3
    real, dimension(0:101,0:101,0:81), intent(In) :: rhs
! WRITTEN
    real, dimension(0:101,0:101,0:81), intent(Out) :: p4
! READ & WRITTEN
! globalIdDeclaration
    integer :: global_id
! globalIdInitialisation
    call get_global_id(global_id,0)
! ptrAssignments_fseq

    ! ParallelFortran: Synthesised loop variables
    i_range = ((101 - 0) + 1)
    j_range = ((101 - 0) + 1)
    k_range = ((81 - 0) + 1)
    i_rel = (global_id / (j_range * k_range))
    i = (i_rel + 0)
    j_rel = ((global_id - (i_rel * (j_range * k_range))) / k_range)
    j = (j_rel + 0)
    k_rel = ((global_id - (i_rel * (j_range * k_range))) - (j_rel * k_range))
    k = (k_rel + 0)


    ! ParallelFortran: Original code
if ( i == 100 + 1 ) then
p4(i,j,k) = p3(i - 100,j,k)
else if ( i == 0 ) then
p4(i,j,k) = p3(i + 100,j,k)
else if ( j == 100 + 1 ) then
p4(i,j,k) = p3(i - 1,j,k)
else if ( j == 0 ) then
 p4(i,j,k) = p3(i,j,k)
 else if ( k == 0 ) then
 p4(i,j,k) = p3(i,j,k)
 else if ( k == 80 + 1 ) then
 p4(i,j,k) = p3(i,j,k)
 else
 reltmp = 1.0 * (1.0 / 3.0 * (0.5 * p3(i + 1,j,k) + 0.5 * p3(i - 1,j,k) + 0.5 * p3(i,j + 1, &
      k) + 0.5 * p3(i,j - 1,k) + 0.5 * p3(i,j,k + 1) + 0.5 * p3(i,j,k - 1) - rhs(i,j,k)) - p3(i,j, &
      k))
 p4(i,j,k) = p3(i,j,k) + reltmp
 end if

end subroutine sor_superkernel_map_95


subroutine sor_superkernel_superkernel(p0,rhs,p1,p2,p3,p4,state_ptr)
! use module_sor_superkernel_superkernel_init
  real, dimension(0:101,0:101,0:81), intent(In) :: p0
  real, dimension(0:101,0:101,0:81), intent(In) :: rhs
  real, dimension(0:101,0:101,0:81), intent(InOut) :: p1
  real, dimension(0:101,0:101,0:81), intent(InOut) :: p2
  real, dimension(0:101,0:101,0:81), intent(InOut) :: p3
  real, dimension(0:101,0:101,0:81), intent(Out) :: p4

  integer :: state
  integer :: state_ptr
integer, parameter :: ST_SOR_SUPERKERNEL_MAP_23 = 0 !  sor_superkernel_map_23
integer, parameter :: ST_SOR_SUPERKERNEL_MAP_47 = 1 !  sor_superkernel_map_47
integer, parameter :: ST_SOR_SUPERKERNEL_MAP_71 = 2 !  sor_superkernel_map_71
integer, parameter :: ST_SOR_SUPERKERNEL_MAP_95 = 3 !  sor_superkernel_map_95
  state = state_ptr ! state 
! SUPERKERNEL BODY
  select case(state)
    case (ST_SOR_SUPERKERNEL_MAP_23)
      call sor_superkernel_map_23(p0,rhs,p1)
    case (ST_SOR_SUPERKERNEL_MAP_47)
      call sor_superkernel_map_47(p1,rhs,p2)
    case (ST_SOR_SUPERKERNEL_MAP_71)
      call sor_superkernel_map_71(p2,rhs,p3)
    case (ST_SOR_SUPERKERNEL_MAP_95)
      call sor_superkernel_map_95(p3,rhs,p4)
  end select
end subroutine sor_superkernel_superkernel
end module module_sor_superkernel_superkernel