program test_sor_unroll
    use sor_params
    use sor_routines
    use cudafor    ! Add this line to use CUDA in your Fortran code

    real, dimension(:,:,:), allocatable, device :: p0
    real, dimension(:,:,:), allocatable, device :: p1
    real, dimension(:,:,:), allocatable, device :: rhs
    real, dimension(:,:,:), allocatable :: p0_host
    integer :: iter, niters
    integer :: i,j,k

    allocate(p0_host(0:im+1,0:jm+1,0:km+1))
    allocate(p0(0:im+1,0:jm+1,0:km+1))
    allocate(p1(0:im+1,0:jm+1,0:km+1))
    allocate(rhs(0:im+1,0:jm+1,0:km+1))

    do i = 0,im+1
        do j = 0,jm+1
            do k = 0,km+1
                p0_host(i,j,k) = 1.0
            end do
        end do
    end do

    p0 = p0_host
    rhs = p0

    niters = 12/4

    do iter = 1,niters
        call sor (p0, p1, rhs)
        p0 = p1
    end do

    p0_host = p0

    print *, p0_host(im/2,jm/2,km/2)

    deallocate(p0, p1, rhs, p0_host)
end program test_sor_unroll
