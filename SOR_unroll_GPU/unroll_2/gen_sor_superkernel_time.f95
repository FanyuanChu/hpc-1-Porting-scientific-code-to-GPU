! gen_sor_superkernel_time.f95
program main
  use cudafor
  use singleton_module_sor_superkernel, only : sor_superkernel
  implicit none
  integer, parameter :: im=100
  integer, parameter :: jm=100
  integer, parameter :: km=80
  integer :: i
  integer :: j
  integer :: k
  integer :: global_id_0
  real, dimension(:), allocatable, device :: p0_0_dev, rhs_0_dev, p2_1_dev
  real, dimension(1:853128) :: p0_0, rhs_0, p2_1
  integer, parameter :: st_stage_kernel_1=1
  integer, device :: state_ptr_dev
  integer, parameter :: niters=100
  integer :: iter
  integer, parameter :: blockSize = 256
  integer :: numBlocks
  integer :: n
  type(cudaEvent) :: start, stop
  real :: elapsedTime
  integer :: istat

  real, dimension(:), allocatable :: p0_0_host, rhs_0_host, p2_1_host
  integer :: index

  n = (im+1)*(jm+1)*(km+1)
  numBlocks = (n + blockSize - 1) / blockSize

  allocate(p0_0_dev(n), rhs_0_dev(n), p2_1_dev(n))
  
  ! Initialize on the host
  allocate(p0_0_host(n))
  allocate(rhs_0_host(n))
  allocate(p2_1_host(n))
  p0_0_host = 1.0
  rhs_0_host = 1.0

  ! Copy initialized data to the device
  p0_0_dev = p0_0_host
  rhs_0_dev = rhs_0_host
  
  state_ptr_dev = st_stage_kernel_1

  ! Creating events for timing
  istat = cudaEventCreate(start)
  istat = cudaEventCreate(stop)

  ! Time the kernel execution
  istat = cudaEventRecord(start, 0)
  do iter = 1, niters
    print *, iter
    call sor_superkernel<<<853128, 1>>>(p0_0_dev, rhs_0_dev, p2_1_dev, state_ptr_dev)
  end do
  istat = cudaEventRecord(stop, 0)
  istat = cudaDeviceSynchronize()
  istat = cudaEventElapsedTime(elapsedTime, start, stop)
  elapsedTime = elapsedTime / (niters*1.0e3)
  print *, "Average kernel execution time per iteration: ", elapsedTime, " seconds."

  p2_1 = p2_1_dev
  p2_1_host = p2_1_dev  ! Copy data from device to host after computation

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

  ! Deallocate the memory
  deallocate(p0_0_host)
  deallocate(rhs_0_host)
  deallocate(p2_1_host)

end program main
