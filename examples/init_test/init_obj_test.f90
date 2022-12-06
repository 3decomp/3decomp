!!!! init_obj_test.f90
!!!  Tests initialising the 2decomp&fft library with
!!!  objects.
!!!=====================================================

module decomp2d_extents

  type grid_extents
     integer, dimension(:), allocatable :: g
   contains
     final :: free_grid_extents
  end type grid_extents
  
  type, extends(grid_extents) :: compute_grid_extents
  end type compute_grid_extents

  type, extends(grid_extents) :: processor_grid_extents
  end type processor_grid_extents

contains

  ! Make a grid extents object of specified dimensionality
  subroutine make_grid_extents(ndim, grid)

    integer, intent(in) :: ndim
    class(grid_extents), intent(inout) :: grid

    allocate(grid%g(ndim))
    grid%g(:) = 0
    
  end subroutine make_grid_extents

  ! Set the extent of a grid in a given dimension
  subroutine set_grid_extents(dim, n, grid)

    integer, intent(in) :: dim                 ! Which dimension to set
    integer, intent(in) :: n                   ! Grid_Extents size
    class(grid_extents), intent(inout) :: grid ! The grid_extents

    grid%g(dim) = n

  end subroutine set_grid_extents

  ! Get the extent of a grid in a given dimension
  integer function get_grid_extents(dim, grid)

    integer, intent(in) :: dim
    class(grid_extents), intent(in) :: grid

    get_grid_extents = grid%g(dim)

  end function get_grid_extents

  ! Get the dimensionality of a grid
  integer function get_grid_dimension(grid)

    class(grid_extents), intent(in) :: grid

    get_grid_dimension = size(grid%g)

  end function get_grid_dimension

  ! Free contents of a grid object.
  ! Used as a type-bound finaliser.
  subroutine free_grid_extents(grid)

    type(grid_extents), intent(inout) :: grid

    deallocate(grid%g)
    
  end subroutine free_grid_extents

end module decomp2d_extents

program init_obj_test

  use MPI
  use decomp_2d
  use decomp2d_extents

  use init_utils
  
  implicit none

  integer, parameter :: nx = 5
  integer, parameter :: ny = 6
  integer, parameter :: nz = 7
  integer, parameter :: nexpect = nx * ny * nz

  integer :: p_row, p_col

  integer :: ierr

  call MPI_Init(ierr)
  
  p_row = 0; p_col = 0
  call run(p_row, p_col)

  call MPI_Finalize(ierr)
  
contains

  subroutine run(p_row, p_col)

    integer, intent(inout) :: p_row, p_col
    class(grid_extents), allocatable :: cgrid, pgrid

    allocate(compute_grid_extents :: cgrid)
    allocate(processor_grid_extents :: pgrid)
    
    call make_grid_extents(3, cgrid)
    call set_grid_extents(1, nx, cgrid)
    call set_grid_extents(2, ny, cgrid)
    call set_grid_extents(3, nz, cgrid)

    call make_grid_extents(2, pgrid)
    call set_grid_extents(1, p_row, pgrid)
    call set_grid_extents(2, p_col, pgrid)

    call decomp_2d_init_new(cgrid, pgrid)

    call check_axis("X", nexpect)
    call check_axis("Y", nexpect)
    call check_axis("Z", nexpect)

    call check_grid(pgrid)

    deallocate(cgrid)
    deallocate(pgrid)
    
    call decomp_2d_finalize()
    
  end subroutine run

  ! Generic wrapper for testing initialisation objects.
  subroutine check_grid(grid)

    class(grid_extents), intent(in) :: grid

    select type(grid)
    type is(processor_grid_extents)
       call check_processor_grid(grid)
    class default
       print *, "ERROR: unhandled type!"
       stop 1
    end select

  end subroutine check_grid

  ! Type-specific test of processor grid.
  ! After initialisation the processor grid should have values whose product = the number of MPI
  ! ranks.
  subroutine check_processor_grid(pgrid)

    type(processor_grid_extents), intent(in) :: pgrid

    integer :: i
    integer :: p

    p = 1
    do i = 1, get_grid_dimension(pgrid)
       p = p * get_grid_extents(i, pgrid)
    end do

    if (p /= nproc) then
       print *, "ERROR: processor grid not set correctly!"
       stop 1
    end if
    
  end subroutine check_processor_grid

  ! Generic wrapper for the new initialisation prototype.
  subroutine decomp_2d_init_new(cgrid, pgrid)

    class(grid_extents), intent(in) :: cgrid
    class(grid_extents), intent(inout) :: pgrid

    select type(cgrid)
    type is(compute_grid_extents)
       select type(pgrid)
       type is(processor_grid_extents)
          call decomp_2d_init_concrete(cgrid, pgrid)
       class default
          print *, "ERROR: incorrect pgrid type!"
       end select
    class default
       print *, "ERROR: incorrect cgrid type!"
    end select
    
  end subroutine decomp_2d_init_new

  ! Concrete implementation for the new initialisation prototype.
  ! Currently this simply wraps the old initialisation subroutine, extracting and passing the
  ! required information from the initialisation objects.
  subroutine decomp_2d_init_concrete(cgrid, pgrid)

    type(compute_grid_extents), intent(in) :: cgrid
    type(processor_grid_extents), intent(inout) :: pgrid

    associate(nx => get_grid_extents(1, cgrid), &
         ny => get_grid_extents(2, cgrid), &
         nz => get_grid_extents(3, cgrid), &
         p_row => pgrid%g(1), &
         p_col => pgrid%g(2))
      call decomp_2d_init(nx, ny, nz, p_row, p_col)
    end associate

  end subroutine decomp_2d_init_concrete
  
end program init_obj_test
