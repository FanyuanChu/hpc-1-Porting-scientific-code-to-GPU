program main
use module_sor_superkernel, only : sor_superkernel
!    ! integer :: global_id
!    ! common /ocl/ global_id
!    ! Declarations
      implicit none
    integer :: i
    integer :: j
    integer :: k
    integer :: global_id_0
    real, dimension(1:853128) :: p0_0
    real, dimension(1:853128) :: rhs_0
    real, dimension(1:853128) :: p4_1
      integer, parameter :: st_stage_kernel_1=1
    integer :: state_ptr
      integer, parameter :: niters=10
    integer :: iter
#ifdef TIMING
    integer :: clock_rate
    integer, dimension(0:1) :: timestamp
#endif
#ifdef TIMING
    call system_clock(timestamp(0), clock_rate)
#endif
!    ! Loops over stage calls
    state_ptr = st_stage_kernel_1
    do iter = 1, niters
    print *, iter
    do global_id_0 = 1, 853128
      call sor_superkernel(global_id_0, p0_0, rhs_0, p4_1,state_ptr)
    end do
    end do
#ifdef TIMING
    call system_clock(timestamp(1), clock_rate)
    print '(f6.3)',(timestamp(1)-timestamp(0))/ real(clock_rate)
#endif
end program main  
