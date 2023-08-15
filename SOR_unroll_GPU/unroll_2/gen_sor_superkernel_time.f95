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
  real, dimension(:), allocatable, device :: p0_0_dev, rhs_0_dev, p2_1_dev
  real, dimension(1:853128) :: p0_0, rhs_0, p2_1
  integer, parameter :: st_stage_kernel_1=1
  integer, device :: state_ptr_dev
  integer, parameter :: niters=100
  integer :: iter
  integer, parameter :: blockSize = 256
  integer :: numBlocks
  integer :: n
  real, dimension(:), allocatable :: p0_0_host, rhs_0_host, p2_1_host
  integer :: index

  type(cudaEvent_t) :: start_event_total, stop_event_total, start_event_copy, stop_event_copy, start_event_compute, stop_event_compute
  real :: elapsed_time_total, elapsed_time_copy, elapsed_time_compute
  integer :: istat

  n = (im+1)*(jm+1)*(km+1)
  numBlocks = (n + blockSize - 1) / blockSize

  allocate(p0_0_dev(n), rhs_0_dev(n), p2_1_dev(n))
  
  ! Initialize on the host
  allocate(p0_0_host(n))
  allocate(rhs_0_host(n))
  allocate(p2_1_host(n))
  p0_0_host = 1.0
  rhs_0_host = 1.0

  ! Record total start time
  istat = cudaEventCreate(start_event_total)
  istat = cudaEventCreate(stop_event_total)
  istat = cudaEventRecord(start_event_total, 0)

  ! Record copy start time
  istat = cudaEventCreate(start_event_copy)
  istat = cudaEventCreate(stop_event_copy)
  istat = cudaEventRecord(start_event_copy, 0)

  ! Copy initialized data to the device
  p0_0_dev = p0_0_host
  rhs_0_dev = rhs_0_host
  
  ! Record copy stop time
  istat = cudaEventRecord(stop_event_copy, 0)
  istat = cudaEventSynchronize(stop_event_copy)

  ! Record compute start time
  istat = cudaEventCreate(start_event_compute)
  istat = cudaEventCreate(stop_event_compute)
  istat = cudaEventRecord(start_event_compute, 0)

  state_ptr_dev = st_stage_kernel_1
  do iter = 1, niters
    print *, iter
    call sor_superkernel<<<853128, 1>>>(p0_0_dev, rhs_0_dev, p2_1_dev, state_ptr_dev)
  end do

  ! Record compute stop time
  istat = cudaEventRecord(stop_event_compute, 0)
  istat = cudaEventSynchronize(stop_event_compute)

  ! Record total stop time
  istat = cudaEventRecord(stop_event_total, 0)
  istat = cudaEventSynchronize(stop_event_total)

  p2_1 = p2_1_dev
  p2_1_host = p2_1_dev  ! Copy data from device to host after computation

  ! Compute elapsed times
  istat = cudaEventElapsedTime(elapsed_time_total, start_event_total, stop_event_total)
  istat = cudaEventElapsedTime(elapsed_time_copy, start_event_copy, stop_event_copy)
  istat = cudaEventElapsedTime(elapsed_time_compute, start_event_compute, stop_event_compute)

  ! Print elapsed times
  print *, "Total time elapsed: ", elapsed_time_total / 1000.0, " seconds."
  print *, "Time for data copy: ", elapsed_time_copy / 1000.0, " seconds."
  print *, "Time for computation: ", elapsed_time_compute / 1000.0, " seconds."

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
