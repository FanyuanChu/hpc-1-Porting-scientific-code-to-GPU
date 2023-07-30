! test_sor_unroll.f95 
program test_sor_unroll
    use sor_params
    use sor_routines  !!! ADDED
#ifndef DYN_ALLOC
    real, dimension(0:im+1,0:jm+1,0:km+1), device :: p0  !!! MODIFIED
    real, dimension(0:im+1,0:jm+1,0:km+1), device :: p1  !!! MODIFIED
    real, dimension(0:im+1,0:jm+1,0:km+1), device :: rhs  !!! MODIFIED
#else
    real, device, allocatable  :: p0(:,:,:)  !!! MODIFIED
    real, device, allocatable  :: p1(:,:,:)  !!! MODIFIED
    real, device, allocatable  :: rhs(:,:,:)  !!! MODIFIED
#endif
    integer :: iter, niters

    integer :: i,j,k

    do i = 0,im+1
    do j = 0,jm+1
    do k = 0,km+1
        rhs(i,j,k) = 1.0
        p0(i,j,k) = 1.0
    end do
    end do
    end do

    niters = 12/UNROLL

    do iter = 1,niters
        print *,iter
        call sor (p0,p1,rhs)
    #if UNROLL==1
        p0=p1
    #endif
    end do

    ! print *, p0(im/2,jm/2,km/2)  !!! MODIFIED
end program test_sor_unroll
