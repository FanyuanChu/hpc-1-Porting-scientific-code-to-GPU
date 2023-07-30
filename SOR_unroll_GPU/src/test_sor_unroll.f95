program test_sor_unroll
    use sor_params
    use sor_routines
    use cudafor

    real, dimension(:,:,:), allocatable, device :: p0
    real, dimension(:,:,:), allocatable, device :: p1
    real, dimension(:,:,:), allocatable, device :: rhs
    real, dimension(:,:,:), allocatable :: p0_host
    integer :: iter, niters
    integer :: i,j,k
    integer(kind=8) :: size

    allocate(p0_host(0:im+1,0:jm+1,0:km+1))

    size = sizeof(real)*(im+2)*(jm+2)*(km+2)
    call cudaMalloc(p0, size)
    call cudaMalloc(p1, size)
    call cudaMalloc(rhs, size)

    do i = 0,im+1
        do j = 0,jm+1
            do k = 0,km+1
                p0_host(i,j,k) = 1.0
            end do
        end do
    end do

    call cudaMemcpy(p0, p0_host, size, cudaMemcpyHostToDevice)
    rhs = p0

    niters = 12

    do iter = 1,niters
        call sor (p0, p1, rhs)
        p0 = p1
    end do

    call cudaMemcpy(p0_host, p0, size, cudaMemcpyDeviceToHost)

    print *, p0_host(im/2,jm/2,km/2)

    call cudaFree(p0)
    call cudaFree(p1)
    call cudaFree(rhs)
    deallocate(p0_host)
end program test_sor_unroll
