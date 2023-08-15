! gen_sor_superkernel.f95
program main
  use cudafor
  use singleton_module_sor_superkernel, only : sor_superkernel
  implicit none
  
  integer, parameter :: im=100, jm=100, km=80
  integer :: i, j, k, global_id_0, iter, istat
  real, device, allocatable :: p0_0_dev(:), rhs_0_dev(:), p2_1_dev(:)
  real, allocatable :: p0_0_host(:), rhs_0_host(:), p2_1_host(:)
  real :: p0_0(853128), rhs_0(853128), p2_1(853128)
  integer, parameter :: st_stage_kernel_1=1
  integer, device :: state_ptr_dev
  integer, parameter :: niters=100
  integer, parameter :: blockSize = 256
  integer :: numBlocks, n, index
  type(cudaEvent) :: start_event_total, stop_event_total, start_event_init, stop_event_init, start_event_copy_to_device, stop_event_copy_to_device, start_event_compute, stop_event_compute, start_event_copy_to_host, stop_event_copy_to_host
  real(4) :: elapsed_time_total, elapsed_time_init, elapsed_time_copy_to_device, elapsed_time_compute, elapsed_time_copy_to_host

  ! Start total timer
  istat = cudaEventCreate(start_event_total)
  istat = cudaEventRecord(start_event_total, 0)

  n = (im+1)*(jm+1)*(km+1)
  numBlocks = (n + blockSize - 1) / blockSize

  allocate(p0_0_dev(n), rhs_0_dev(n), p2_1_dev(n))
  
  ! Initialization on the host
  istat = cudaEventCreate(start_event_init)
  istat = cudaEventRecord(start_event_init, 0)
  
  allocate(p0_0_host(n))
  allocate(rhs_0_host(n))
  allocate(p2_1_host(n))
  p0_0_host = 1.0
  rhs_0_host = 1.0
  
  istat = cudaEventRecord(stop_event_init, 0)
  istat = cudaEventSynchronize(stop_event_init)
  istat = cudaEventElapsedTime(elapsed_time_init, start_event_init, stop_event_init)
  
  ! Time to copy data to the device
  istat = cudaEventCreate(start_event_copy_to_device)
  istat = cudaEventRecord(start_event_copy_to_device, 0)
  p0_0_dev = p0_0_host
  rhs_0_dev = rhs_0_host
  istat = cudaEventRecord(stop_event_copy_to_device, 0)
  istat = cudaEventSynchronize(stop_event_copy_to_device)
  istat = cudaEventElapsedTime(elapsed_time_copy_to_device, start_event_copy_to_device, stop_event_copy_to_device)

  state_ptr_dev = st_stage_kernel_1
  
  ! Time for computation
  istat = cudaEventCreate(start_event_compute)
  istat = cudaEventRecord(start_event_compute, 0)
  do iter = 1, niters
    print *, iter
    call sor_superkernel<<<853128, 1>>>(p0_0_dev, rhs_0_dev, p2_1_dev, state_ptr_dev)
  end do
  istat = cudaEventRecord(stop_event_compute, 0)
  istat = cudaEventSynchronize(stop_event_compute)
  istat = cudaEventElapsedTime(elapsed_time_compute, start_event_compute, stop_event_compute)

  ! Time to copy data from device to host
  istat = cudaEventCreate(start_event_copy_to_host)
  istat = cudaEventRecord(start_event_copy_to_host, 0)
  p2_1 = p2_1_dev
  p2_1_host = p2_1_dev
  istat = cudaEventRecord(stop_event_copy_to_host, 0)
  istat = cudaEventSynchronize(stop_event_copy_to_host)
  istat = cudaEventElapsedTime(elapsed_time_copy_to_host, start_event_copy_to_host, stop_event_copy_to_host)

  ! End total timer
  istat = cudaEventRecord(stop_event_total, 0)
  istat = cudaEventSynchronize(stop_event_total)
  istat = cudaEventElapsedTime(elapsed_time_total, start_event_total, stop_event_total)

  ! Print times
  print *, "Initialization time: ", elapsed_time_init / 1000.0, " seconds."
  print *, "Time to copy data to the device: ", elapsed_time_copy_to_device / 1000.0, " seconds."
  print *, "Time for computation on the device: ", elapsed_time_compute / 1000.0, " seconds."
  print *, "Time to copy data from device to host: ", elapsed_time_copy_to_host / 1000.0, " seconds."
  print *, "Total execution time: ", elapsed_time_total / 1000.0, " seconds."

  ! Rest of the print statements and deallocations...

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

  deallocate(p0_0_host)
  deallocate(rhs_0_host)
  deallocate(p2_1_host)
end program main
