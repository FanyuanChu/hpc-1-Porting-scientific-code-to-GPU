program test_sor_unroll
  use cudafor
  use sor_params
  use sor_routines

  integer, parameter :: UNROLL = 4
  !integer, parameter :: UNROLL = 3
  !integer, parameter :: UNROLL = 2
  !integer, parameter :: UNROLL = 1
  real, device, allocatable :: p0(:,:,:), p1(:,:,:), p2(:,:,:), p3(:,:,:), p4(:,:,:), rhs(:,:,:)
  real, allocatable :: rhs_host(:,:,:), p0_host(:,:,:)
  integer :: iter, niters
  integer :: i, j, k
  real :: init_time, copy_to_device_time, comp_time, copy_from_device_time, total_time
  type (cudaEvent) :: init_start_event, init_stop_event, copy_to_device_start_event, copy_to_device_stop_event, comp_start_event, comp_stop_event, copy_from_device_start_event, copy_from_device_stop_event, total_start_event, total_stop_event
  integer :: istat
  real :: sample_device, sample_host

  ! Create all events
  istat = cudaEventCreate(init_start_event)
  istat = cudaEventCreate(init_stop_event)
  istat = cudaEventCreate(copy_to_device_start_event)
  istat = cudaEventCreate(copy_to_device_stop_event)
  istat = cudaEventCreate(comp_start_event)
  istat = cudaEventCreate(comp_stop_event)
  istat = cudaEventCreate(copy_from_device_start_event)
  istat = cudaEventCreate(copy_from_device_stop_event)
  istat = cudaEventCreate(total_start_event)
  istat = cudaEventCreate(total_stop_event)

  istat = cudaEventRecord(total_start_event, 0)

  ! Initialization on the host
  istat = cudaEventRecord(init_start_event, 0)
  allocate(rhs_host(0:im+1,0:jm+1,0:km+1))
  allocate(p0_host(0:im+1,0:jm+1,0:km+1))
  rhs_host = 1.0
  p0_host = 1.0
  istat = cudaEventRecord(init_stop_event, 0)
  istat = cudaEventSynchronize(init_stop_event)
  istat = cudaEventElapsedTime(init_time, init_start_event, init_stop_event)

  ! Allocate device memory
  allocate(p0(0:im+1,0:jm+1,0:km+1))
  allocate(p1(0:im+1,0:jm+1,0:km+1))
  allocate(p2(0:im+1,0:jm+1,0:km+1))
  allocate(p3(0:im+1,0:jm+1,0:km+1))
  allocate(p4(0:im+1,0:jm+1,0:km+1))
  allocate(rhs(0:im+1,0:jm+1,0:km+1))

  ! Copy data to the device
  istat = cudaEventRecord(copy_to_device_start_event, 0)
  rhs = rhs_host
  p0 = p0_host
  istat = cudaEventRecord(copy_to_device_stop_event, 0)
  istat = cudaEventSynchronize(copy_to_device_stop_event)
  istat = cudaEventElapsedTime(copy_to_device_time, copy_to_device_start_event, copy_to_device_stop_event)

  niters = 12 / UNROLL

  ! Record the start time for computation
  istat = cudaEventRecord(comp_start_event, 0)

  do iter = 1, niters
    print *, iter
    call sor(p0, p1, rhs)
    call sor(p1, p2, rhs)
    call sor(p2, p3, rhs)
    call sor(p3, p4, rhs)
    p0 = p4
  end do

  ! End the computation time recording
  istat = cudaEventRecord(comp_stop_event, 0)
  istat = cudaEventSynchronize(comp_stop_event)
  istat = cudaEventElapsedTime(comp_time, comp_start_event, comp_stop_event)

  ! Get a sample value from the device
  sample_device = p0(im/2,jm/2,km/2)

  ! Copy from device to host
  istat = cudaEventRecord(copy_from_device_start_event, 0)
  sample_host = sample_device  ! Copy the value to the host
  istat = cudaEventRecord(copy_from_device_stop_event, 0)
  istat = cudaEventSynchronize(copy_from_device_stop_event)
  istat = cudaEventElapsedTime(copy_from_device_time, copy_from_device_start_event, copy_from_device_stop_event)

  ! Record the total time
  istat = cudaEventRecord(total_stop_event, 0)
  istat = cudaEventSynchronize(total_stop_event)
  istat = cudaEventElapsedTime(total_time, total_start_event, total_stop_event)

  ! Printing results
  print *, sample_host
  print *, "Initialization time: ", init_time / 1000.0, " seconds."
  print *, "Time to copy data to the device: ", copy_to_device_time / 1000.0, " seconds."
  print *, "Time for computation on the device: ", comp_time / 1000.0, " seconds."
  print *, "Time to copy data from device to host: ", copy_from_device_time / 1000.0, " seconds."
  print *, "Total execution time: ", total_time / 1000.0, " seconds."

  ! Deallocate memory
  deallocate(p0)
  deallocate(p1)
  deallocate(p2)
  deallocate(p3)
  deallocate(p4)
  deallocate(rhs)
  deallocate(rhs_host)
  deallocate(p0_host)

  ! Destroy all events
  istat = cudaEventDestroy(init_start_event)
  istat = cudaEventDestroy(init_stop_event)
  istat = cudaEventDestroy(copy_to_device_start_event)
  istat = cudaEventDestroy(copy_to_device_stop_event)
  istat = cudaEventDestroy(comp_start_event)
  istat = cudaEventDestroy(comp_stop_event)
  istat = cudaEventDestroy(copy_from_device_start_event)
  istat = cudaEventDestroy(copy_from_device_stop_event)
  istat = cudaEventDestroy(total_start_event)
  istat = cudaEventDestroy(total_stop_event)

end program
