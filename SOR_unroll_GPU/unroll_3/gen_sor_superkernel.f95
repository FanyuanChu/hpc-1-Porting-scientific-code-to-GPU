program main
#ifdef WITH_OPENMP
use omp_lib
#endif
use singleton_module_sor_superkernel, only : sor_superkernel
!    ! integer :: global_id
!    ! common /ocl/ global_id
!    ! Declarations
      implicit none
     integer, parameter :: im=100
     integer, parameter :: jm=100
     integer, parameter :: km=80
    integer :: i
    integer :: j
    integer :: k
    integer :: global_id_0
    real, dimension(1:853128) :: p0_0
    real, dimension(1:853128) :: rhs_0
    real, dimension(1:853128) :: p3_1
      integer, parameter :: st_stage_kernel_1=1
    integer :: state_ptr
      integer, parameter :: niters=10 !300
    integer :: iter
#ifdef TIMING
    integer :: clock_rate
    integer, dimension(0:1) :: timestamp
#endif

    do i = 1,(im+1)*(jm+1)*(km+1)
        rhs_0(i) = 1.0
        p0_0(i) = 1.0
    end do


#ifdef TIMING
    call system_clock(timestamp(0), clock_rate)
#endif
!    ! Loops over stage calls
    state_ptr = st_stage_kernel_1
    do iter = 1, niters
    print *, iter
#ifdef WITH_OPENMP
!$OMP PARALLEL DO
#endif    
    do global_id_0 = 1, 853128
      call sor_superkernel(global_id_0, p0_0, rhs_0, p3_1,state_ptr)
    end do
#ifdef WITH_OPENMP
!$OMP END PARALLEL DO
#endif    
    end do
#ifdef TIMING
    call system_clock(timestamp(1), clock_rate)
    print '(f6.3)',(timestamp(1)-timestamp(0))/ real(clock_rate)
#endif
#ifdef CHECKSUM
    print *, p0_0((im+2)*(jm+2)*(km+2)/2+(jm+2)*(km+2)/2+(km+2)/2)
#endif
end program main  
