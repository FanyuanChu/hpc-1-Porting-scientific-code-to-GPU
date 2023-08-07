program test_sor_unroll
  use cudafor
  use sor_params
  use sor_routines

  integer, parameter :: UNROLL = 4
  real, device, allocatable :: p0(:,:,:), p1(:,:,:), p2(:,:,:), p3(:,:,:), p4(:,:,:), rhs(:,:,:)
  real, allocatable :: rhs_host(:,:,:), p0_host(:,:,:)
  integer :: iter, niters
  integer :: i,j,k
  real :: elapsed_time
  type (cudaEvent) :: start_event, stop_event
  integer :: istat

  ! Allocate device memory
  allocate(p0(0:im+1,0:jm+1,0:km+1))
  allocate(p1(0:im+1,0:jm+1,0:km+1))
  allocate(p2(0:im+1,0:jm+1,0:km+1))
  allocate(p3(0:im+1,0:jm+1,0:km+1))
  allocate(p4(0:im+1,0:jm+1,0:km+1))
  allocate(rhs(0:im+1,0:jm+1,0:km+1))

  ! Initialization on the host
  allocate(rhs_host(0:im+1,0:jm+1,0:km+1))
  allocate(p0_host(0:im+1,0:jm+1,0:km+1))
  rhs_host = 1.0
  p0_host = 1.0

  ! Copy data to the device
  rhs = rhs_host
  p0 = p0_host

  ! Create events
  istat = cudaEventCreate(start_event)
  istat = cudaEventCreate(stop_event)

  niters = 12 / UNROLL

  ! Record the start time
  istat = cudaEventRecord(start_event, 0)

  do iter = 1, niters
    print *, iter
    call sor(p0, p1, rhs)
    call sor(p1, p2, rhs)
    call sor(p2, p3, rhs)
    call sor(p3, p4, rhs)
    p0 = p4
  end do

  ! Record the stop time
  istat = cudaEventRecord(stop_event, 0)
  istat = cudaEventSynchronize(stop_event)

  ! Calculate the elapsed time
  istat = cudaEventElapsedTime(elapsed_time, start_event, stop_event)

  ! Printing a sample value
  print *, p0(im/2,jm/2,km/2)
  print *, "Time elapsed: ", elapsed_time / 1000.0, " seconds."

  ! Deallocate device memory
  deallocate(p0)
  deallocate(p1)
  deallocate(p2)
  deallocate(p3)
  deallocate(p4)
  deallocate(rhs)
  deallocate(rhs_host)
  deallocate(p0_host)

  ! Destroy events
  istat = cudaEventDestroy(start_event)
  istat = cudaEventDestroy(stop_event)

end program
