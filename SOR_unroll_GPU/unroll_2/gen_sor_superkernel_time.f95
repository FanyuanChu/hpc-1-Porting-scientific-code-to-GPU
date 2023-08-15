! gen_sor_superkernel_time.f95
program main
  use cudafor
  use singleton_module_sor_superkernel, only : sor_superkernel

  implicit none

  integer, parameter :: im=100, jm=100, km=80
  integer :: n, numBlocks, iter, index, istat, state_ptr_dev
  integer, parameter :: blockSize = 256, niters=100
  real, dimension(:), allocatable, device :: p0_0_dev, rhs_0_dev, p2_1_dev
  real, dimension(:), allocatable :: p0_0_host, rhs_0_host, p2_1_host
  real, dimension(1:853128) :: p0_0, rhs_0, p2_1
  real(4) :: timeInit, timeCopyToDevice, timeCompute, timeCopyToHost, timeTotal
  type(cudaEvent) :: startInit, stopInit
  type(cudaEvent) :: startCopyToDevice, stopCopyToDevice
  type(cudaEvent) :: startCompute, stopCompute
  type(cudaEvent) :: startCopyToHost, stopCopyToHost
  type(cudaEvent) :: startTotal, stopTotal

  ! Initialization of CUDA events
  istat = cudaEventCreate(startInit)
  istat = cudaEventCreate(stopInit)
  istat = cudaEventCreate(startCopyToDevice)
  istat = cudaEventCreate(stopCopyToDevice)
  istat = cudaEventCreate(startCompute)
  istat = cudaEventCreate(stopCompute)
  istat = cudaEventCreate(startCopyToHost)
  istat = cudaEventCreate(stopCopyToHost)
  istat = cudaEventCreate(startTotal)
  istat = cudaEventCreate(stopTotal)

  ! Start total execution time recording
  istat = cudaEventRecord(startTotal, 0)

  n = (im+1)*(jm+1)*(km+1)
  numBlocks = (n + blockSize - 1) / blockSize

  allocate(p0_0_dev(n), rhs_0_dev(n), p2_1_dev(n))

  ! Measure initialization time on the host
  istat = cudaEventRecord(startInit, 0)
  allocate(p0_0_host(n))
  allocate(rhs_0_host(n))
  allocate(p2_1_host(n))
  p0_0_host = 1.0
  rhs_0_host = 1.0
  istat = cudaEventRecord(stopInit, 0)
  istat = cudaEventSynchronize(stopInit)

  ! Measure data copy time from host to device
  istat = cudaEventRecord(startCopyToDevice, 0)
  p0_0_dev = p0_0_host
  rhs_0_dev = rhs_0_host
  istat = cudaEventRecord(stopCopyToDevice, 0)
  istat = cudaEventSynchronize(stopCopyToDevice)

  state_ptr_dev = st_stage_kernel_1

  ! Measure computation time on the device
  istat = cudaEventRecord(startCompute, 0)
  do iter = 1, niters
    print *, iter
    call sor_superkernel<<<853128, 1>>>(p0_0_dev, rhs_0_dev, p2_1_dev, state_ptr_dev)
  end do
  istat = cudaEventRecord(stopCompute, 0)
  istat = cudaEventSynchronize(stopCompute)

  ! Measure data copy time from device to host
  istat = cudaEventRecord(startCopyToHost, 0)
  p2_1 = p2_1_dev
  p2_1_host = p2_1_dev
  istat = cudaEventRecord(stopCopyToHost, 0)
  istat = cudaEventSynchronize(stopCopyToHost)

  ! Stop total execution time recording
  istat = cudaEventRecord(stopTotal, 0)
  istat = cudaEventSynchronize(stopTotal)

  ! Fetch and print the times
  istat = cudaEventElapsedTime(timeInit, startInit, stopInit)
  istat = cudaEventElapsedTime(timeCopyToDevice, startCopyToDevice, stopCopyToDevice)
  istat = cudaEventElapsedTime(timeCompute, startCompute, stopCompute)
  istat = cudaEventElapsedTime(timeCopyToHost, startCopyToHost, stopCopyToHost)
  istat = cudaEventElapsedTime(timeTotal, startTotal, stopTotal)

  print *, 'Initialization time: ', timeInit * 1.0E-3, ' seconds.'
  print *, 'Time to copy data to the device: ', timeCopyToDevice * 1.0E-3, ' seconds.'
  print *, 'Time for computation on the device: ', timeCompute * 1.0E-3, ' seconds.'
  print *, 'Time to copy data from device to host: ', timeCopyToHost * 1.0E-3, ' seconds.'
  print *, 'Total execution time: ', timeTotal * 1.0E-3, ' seconds.'

  ! Additional code
  print *, 'Final result1-100:'
  print *, 'p2_1:', p2_1(1:100)
  print *, 'Final result:'
  print *, 'p2_1:', p2_1(8400:8700)
  print *, 'Final result10000-10100:'
  print *, 'p2_1:', p2_1(10000:10100)
  print *, 'Final result:'
  print *, 'p2_1:', p2_1(20000:20100)

  index = (im+2)*(jm+2)*(km+2)/2+(jm+2)*(km+2)/2+(km+2)/2
  print *, 'Index:', index
  print *, 'Value at index:', p2_1_host(index)

  print *, p2_1((im+2)*(jm+2)*(km+2)/2+(jm+2)*(km+2)/2+(km+2)/2)

  ! Deallocate memory
  deallocate(p0_0_host, rhs_0_host, p2_1_host)

end program main
