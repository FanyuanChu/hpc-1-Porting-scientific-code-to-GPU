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
    real, dimension(0:im+1,0:jm+1,0:km+1) :: p0_host  ! Moved up
    integer :: i,j,k


    print *, "Initializing arrays..."
    do i = 0,im+1
    do j = 0,jm+1
    do k = 0,km+1
        rhs(i,j,k) = 1.0
        p0(i,j,k) = 1.0
    end do
    end do
    end do
    print *, "Arrays initialized."

    niters = 12/UNROLL
    print *, "Starting iterations..."
    do iter = 1,niters
        print *, "Iteration ", iter
        call sor (p0,p1,rhs)
    #if UNROLL==1
        p0=p1
    #endif
    end do
    print *, "Iterations completed."

    ! Copy data from device to host and print
    print *, "Copying data from device to host..."
    p0_host = p0
    print *, "Data copied."
    print *, "Value at center: ", p0_host(im/2,jm/2,km/2)
end program test_sor_unroll
