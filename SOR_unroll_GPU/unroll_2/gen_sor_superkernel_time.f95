program main
  use cudafor
  use singleton_module_sor_superkernel, only : sor_superkernel
  implicit none
  integer, parameter :: im=100
  integer, parameter :: jm=100
  integer, parameter :: km=80
  integer :: i, j, k, global_id_0, iter
  real, dimension(:), allocatable, device :: p0_0_dev, rhs_0_dev, p2_1_dev
  real, dimension(1:853128) :: p0_0, rhs_0, p2_1
  integer, parameter :: st_stage_kernel_1=1
  integer, device :: state_ptr_dev
  integer, parameter :: niters=100
  integer, parameter :: blockSize = 256
  integer :: numBlocks, n
  real, dimension(:), allocatable :: p0_0_host, rhs_0_host, p2_1_host
  integer :: index
  type(cudaEvent) :: start_total, stop_total, start_init, stop_init, start_copy_to_device, stop_copy_to_device, start_compute, stop_compute, start_copy_from_device, stop_copy_from_device
  real :: time_total, time_init, time_copy_to_device, time_compute, time_copy_from_device

  n = (im+1)*(jm+1)*(km+1)
  numBlocks = (n + blockSize - 1) / blockSize
  allocate(p0_0_dev(n), rhs_0_dev(n), p2_1_dev(n))

  istat = cudaEventCreate(start_total)
  istat = cudaEventCreate(stop_total)
  istat = cudaEventRecord(start_total, 0)

  ! Measure initialization time
  istat = cudaEventCreate(start_init)
  istat = cudaEventCreate(stop_init)
  istat = cudaEventRecord(start_init, 0)

  allocate(p0_0_host(n))
  allocate(rhs_0_host(n))
  allocate(p2_1_host(n))
  p0_0_host = 1.0
  rhs_0_host = 1.0

  istat = cudaEventRecord(stop_init, 0)
  istat = cudaEventSynchronize(stop_init)

  ! Measure copy data time from host to device
  istat = cudaEventCreate(start_copy_to_device)
  istat = cudaEventCreate(stop_copy_to_device)
  istat = cudaEventRecord(start_copy_to_device, 0)

  p0_0_dev = p0_0_host
  rhs_0_dev = rhs_0_host

  istat = cudaEventRecord(stop_copy_to_device, 0)
  istat = cudaEventSynchronize(stop_copy_to_device)

  state_ptr_dev = st_stage_kernel_1

  ! Measure compute time
  istat = cudaEventCreate(start_compute)
  istat = cudaEventCreate(stop_compute)
  istat = cudaEventRecord(start_compute, 0)

  do iter = 1, niters
    call sor_superkernel<<<853128, 1>>>(p0_0_dev, rhs_0_dev, p2_1_dev, state_ptr_dev)
  end do

  istat = cudaEventRecord(stop_compute, 0)
  istat = cudaEventSynchronize(stop_compute)

  ! Measure copy data time from device to host
  istat = cudaEventCreate(start_copy_from_device)
  istat = cudaEventCreate(stop_copy_from_device)
  istat = cudaEventRecord(start_copy_from_device, 0)

  p0_0 = p0_0_dev
  p2_1_host = p2_1_dev

  istat = cudaEventRecord(stop_copy_from_device, 0)
  istat = cudaEventSynchronize(stop_copy_from_device)

  istat = cudaEventRecord(stop_total, 0)
  istat = cudaEventSynchronize(stop_total)

  ! Calculate and print times
  istat = cudaEventElapsedTime(time_total, start_total, stop_total)
  istat = cudaEventElapsedTime(time_init, start_init, stop_init)
  istat = cudaEventElapsedTime(time_copy_to_device, start_copy_to_device, stop_copy_to_device)
  istat = cudaEventElapsedTime(time_compute, start_compute, stop_compute)
  istat = cudaEventElapsedTime(time_copy_from_device, start_copy_from_device, stop_copy_from_device)

  print *, "Total time elapsed on GPU: ", time_total / 1000.0, " seconds."
  print *, "Initialization time: ", time_init / 1000.0, " seconds."
  print *, "Copy data to device time: ", time_copy_to_device / 1000.0, " seconds."
  print *, "Compute time on GPU: ", time_compute / 1000.0, " seconds."
  print *, "Copy data from device to host time: ", time_copy_from_device / 1000.0, " seconds."

  ! Continue with the rest of the code
  ! print *, 'Final result1-100:'
  ! print *, 'p2_1:', p2_1(1:100)
  ! print *, 'Final result:'
  !print *, 'p2_1:', p2_1(8400:8700)
  !print *, 'Final result10000-10100:'
  !print *, 'p2_1:', p2_1(10000:10100)
  !print *, 'Final result:'
  !print *, 'p2_1:', p2_1(20000:20100)

  index = (im+2)*(jm+2)*(km+2)/2+(jm+2)*(km+2)/2+(km+2)/2
  print *, 'Index:', index
  print *, 'Value at index:', p2_1_host(index)

  print *, p2_1((im+2)*(jm+2)*(km+2)/2+(jm+2)*(km+2)/2+(km+2)/2)

  ! Deallocate the memory
  deallocate(p0_0_host)
  deallocate(rhs_0_host)
  deallocate(p2_1_host)

end program main
