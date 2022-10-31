!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! This example initialises the library and checks the validity of the
!!! communicators
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program comm_test

  use mpi
  use decomp_2d

  implicit none

  integer :: ierr

  integer, parameter :: nx = 5, ny = 7, nz = 9
  integer :: p_row, p_col, p_beam

  call MPI_Init(ierr)

  p_row = 1
  p_col = 1
  p_beam = 1
  call run_test(nx, ny, nz, &
       p_row, p_col, p_beam)

  call MPI_Finalize(ierr)
  
contains

  subroutine run_test(nx, ny, nz, p_row, p_col, p_beam)

    integer, intent(in) :: nx, ny, nz
    integer, intent(inout) :: p_row, p_col, p_beam

    call decomp_2d_init(nx, ny, nz, p_row, p_col, p_beam)
    call decomp_2d_finalize()
    
  end subroutine run_test
  
end program comm_test
