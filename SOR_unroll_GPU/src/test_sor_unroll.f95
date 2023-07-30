! test_sor_unroll.f95 
program test_sor_unroll
    use sor_params
    use sor_routines

#ifndef DYN_ALLOC
    real, dimension(0:im+1,0:jm+1,0:km+1), device :: p0
    real, dimension(0:im+1,0:jm+1,0:km+1), device :: p1
    real, dimension(0:im+1,0:jm+1,0:km+1), device :: rhs
#else
    real, device, allocatable  :: p0(:,:,:)  
    real, device, allocatable  :: p1(:,:,:)  
    real, device, allocatable  :: rhs(:,:,:)  
#endif
    integer :: iter, niters

    integer :: i,j,k
    integer :: clock_rate  !!! ADDED
    integer, dimension(0:1) :: timestamp  !!! ADDED

    do i = 0,im+1
    do j = 0,jm+1
    do k = 0,km+1
        rhs(i,j,k) = 1.0
        p0(i,j,k) = 1.0
    end do
    end do
    end do

    niters = 12/UNROLL

    call system_clock(timestamp(0), clock_rate)  !!! ADDED

    do iter = 1,niters
        print *,iter
        call sor (p0,p1,rhs)
    #if UNROLL==1
        p0=p1
    #endif
    end do

    call system_clock(timestamp(1), clock_rate)  !!! ADDED
    print '(f8.3)',(timestamp(1)-timestamp(0))/ real(clock_rate)  !!! ADDED

    ! Copy data from device to host and print
    real, dimension(0:im+1,0:jm+1,0:km+1) :: p0_host
    p0_host = p0
    print *, p0_host(im/2,jm/2,km/2)
end program test_sor_unroll
