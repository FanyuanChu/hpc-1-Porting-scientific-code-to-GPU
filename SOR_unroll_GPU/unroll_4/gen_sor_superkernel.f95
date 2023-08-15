! gen_sor_superkernel.f95
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
  real, dimension(:), allocatable, device :: p0_0_dev, rhs_0_dev, p4_1_dev
  real, dimension(1:853128) :: p0_0, rhs_0, p4_1
  integer, parameter :: st_stage_kernel_1=1
  integer, device :: state_ptr_dev
  integer, parameter :: niters=10
  integer :: iter
  integer :: clock_rate
  integer, dimension(0:1) :: timestamp
  integer, parameter :: blockSize = 256
  integer :: numBlocks
  integer :: n

  real, dimension(:), allocatable :: p0_0_host, rhs_0_host, p4_1_host
  integer :: index

  n = (im+1)*(jm+1)*(km+1)
  numBlocks = (n + blockSize - 1) / blockSize

  allocate(p0_0_dev(n), rhs_0_dev(n), p4_1_dev(n))
  
  ! Initialize on the host
  allocate(p0_0_host(n))
  allocate(rhs_0_host(n))
  allocate(p4_1_host(n))
  p0_0_host = 1.0
  rhs_0_host = 1.0

  ! Copy initialized data to the device
  p0_0_dev = p0_0_host
  rhs_0_dev = rhs_0_host
  
  state_ptr_dev = st_stage_kernel_1
  call system_clock(timestamp(0), clock_rate)
  do iter = 1, niters
    print *, iter
    call sor_superkernel<<<853128, 1>>>(p0_0_dev, rhs_0_dev, p4_1_dev, state_ptr_dev)
  end do
  call system_clock(timestamp(1), clock_rate)
  print '(f6.3)',(timestamp(1)-timestamp(0))/ real(clock_rate)
  p4_1 = p4_1_dev

  p4_1_host = p4_1_dev  ! Copy data from device to host after computation

  print *, 'Final result1-100:'
  print *, 'p4_1:', p4_1(1:100)
  print *, 'Final result:'
  print *, 'p4_1:', p4_1(8400:8700)
  print *, 'Final result10000-10100:'
  print *, 'p4_1:', p4_1(10000:10100)
  print *, 'Final result:'
  print *, 'p4_1:', p4_1(20000:20100)

  index = (im+2)*(jm+2)*(km+2)/2+(jm+2)*(km+2)/2+(km+2)/2
  print *, 'Index:', index
  print *, 'Value at index:', p4_1_host(index)

  print *, p4_1((im+2)*(jm+2)*(km+2)/2+(jm+2)*(km+2)/2+(km+2)/2)

  ! Deallocate the memory
  deallocate(p0_0_host)
  deallocate(rhs_0_host)
  deallocate(p4_1_host)

end program main
