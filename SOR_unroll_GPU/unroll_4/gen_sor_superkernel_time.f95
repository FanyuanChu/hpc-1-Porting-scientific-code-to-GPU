! gen_sor_superkernel.f95
program main
    use cudafor
    use singleton_module_sor_superkernel, only : sor_superkernel
    implicit none

    integer, parameter :: im=1000
    integer, parameter :: jm=1000
    integer, parameter :: km=320
    integer, parameter :: st_stage_kernel_1=1
    integer, parameter :: niters=12
    integer, parameter :: UNROLL = 4
    !integer, parameter :: UNROLL = 3
    !integer, parameter :: UNROLL = 2
    !integer, parameter :: UNROLL = 1
    integer, parameter :: blockSize = 256

    integer, device :: state_ptr_dev
    integer :: iter, numBlocks, n, index
    integer :: istat
    real(4) :: elapsedTimeTotal, elapsedTimeInit, elapsedTimeCopyToDevice, elapsedTimeCompute, elapsedTimeCopyToHost
    type(cudaEvent) :: startTotal, stopTotal, startInit, stopInit, startCopyToDevice, stopCopyToDevice, startCompute, stopCompute, startCopyToHost, stopCopyToHost

    real, dimension(:), allocatable, device :: p0_0_dev, rhs_0_dev, p4_1_dev
    ! real, dimension(1:853128) :: p0_0, rhs_0, p4_1
    real, dimension(1:(im+1)*(jm+1)*(km+1)) :: p0_0, rhs_0, p4_1
    real, dimension(:), allocatable :: p0_0_host, rhs_0_host, p4_1_host

    n = (im+1)*(jm+1)*(km+1)
    numBlocks = (n + blockSize - 1) / blockSize
    ! numBlocks = n

    allocate(p0_0_dev(n), rhs_0_dev(n), p4_1_dev(n))

    ! Time initialization on the host
    istat = cudaEventCreate(startInit)
    istat = cudaEventCreate(stopInit)
    istat = cudaEventRecord(startInit, 0)
    
    allocate(p0_0_host(n))
    allocate(rhs_0_host(n))
    allocate(p4_1_host(n))
    p0_0_host = 1.0
    rhs_0_host = 1.0

    istat = cudaEventRecord(stopInit, 0)
    istat = cudaEventSynchronize(stopInit)
    istat = cudaEventElapsedTime(elapsedTimeInit, startInit, stopInit)

    ! Time copying data to the device
    istat = cudaEventCreate(startCopyToDevice)
    istat = cudaEventCreate(stopCopyToDevice)
    istat = cudaEventRecord(startCopyToDevice, 0)

    p0_0_dev = p0_0_host
    rhs_0_dev = rhs_0_host

    istat = cudaEventRecord(stopCopyToDevice, 0)
    istat = cudaEventSynchronize(stopCopyToDevice)
    istat = cudaEventElapsedTime(elapsedTimeCopyToDevice, startCopyToDevice, stopCopyToDevice)

    state_ptr_dev = st_stage_kernel_1

    ! Time the computation on the device
    istat = cudaEventCreate(startCompute)
    istat = cudaEventCreate(stopCompute)
    istat = cudaEventRecord(startCompute, 0)

    niters = 12 / UNROLL
    do iter = 1, niters
        print *, iter
        ! call sor_superkernel<<<853128, 1>>>(p0_0_dev, rhs_0_dev, p4_1_dev, state_ptr_dev)
        call sor_superkernel<<<numBlocks, blockSize>>>(p0_0_dev, rhs_0_dev, p4_1_dev, state_ptr_dev)
    end do

    istat = cudaEventRecord(stopCompute, 0)
    istat = cudaEventSynchronize(stopCompute)
    istat = cudaEventElapsedTime(elapsedTimeCompute, startCompute, stopCompute)

    ! Time copying data from device to host
    istat = cudaEventCreate(startCopyToHost)
    istat = cudaEventCreate(stopCopyToHost)
    istat = cudaEventRecord(startCopyToHost, 0)

    p4_1 = p4_1_dev
    p4_1_host = p4_1_dev

    istat = cudaEventRecord(stopCopyToHost, 0)
    istat = cudaEventSynchronize(stopCopyToHost)
    istat = cudaEventElapsedTime(elapsedTimeCopyToHost, startCopyToHost, stopCopyToHost)

    ! Print results
    !print *, 'Final result1-100:'
    !print *, 'p4_1:', p4_1(1:100)
    !print *, 'Final result:'
    !print *, 'p4_1:', p4_1(8400:8700)
    !print *, 'Final result10000-10100:'
    !print *, 'p4_1:', p4_1(10000:10100)
    !print *, 'Final result:'
    !print *, 'p4_1:', p4_1(20000:20100)

    index = (im+2)*(jm+2)*(km+2)/2+(jm+2)*(km+2)/2+(km+2)/2
    print *, 'Index:', index
    print *, 'Value at index:', p4_1_host(index)
    print *, p4_1((im+2)*(jm+2)*(km+2)/2+(jm+2)*(km+2)/2+(km+2)/2)

    ! Print timings
    print *, 'Initialization time: ', elapsedTimeInit * 1.0e-3, ' seconds.'
    print *, 'Time to copy data to the device: ', elapsedTimeCopyToDevice * 1.0e-3, ' seconds.'
    print *, 'Time for computation on the device: ', elapsedTimeCompute * 1.0e-3, ' seconds.'
    print *, 'Time to copy data from device to host: ', elapsedTimeCopyToHost * 1.0e-3, ' seconds.'
    print *, 'Total execution time: ', (elapsedTimeInit + elapsedTimeCopyToDevice + elapsedTimeCompute + elapsedTimeCopyToHost) * 1.0e-3, ' seconds.'

    ! Deallocate the memory
    deallocate(p0_0_host)
    deallocate(rhs_0_host)
    deallocate(p4_1_host)
end program main
