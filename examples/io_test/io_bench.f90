program io_bench

   use decomp_2d
   use decomp_2d_io
   use MPI

   implicit none

   integer, parameter :: nx = 100, ny = 100, nz = 100
   integer :: p_row = 0, p_col = 0

   real(mytype), allocatable, dimension(:, :, :) :: u1

   double precision :: t1, t2
   integer :: ierror

   call MPI_INIT(ierror)
   call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierror)
   call MPI_COMM_RANK(MPI_COMM_WORLD, nrank, ierror)
   call decomp_2d_init(nx, ny, nz, p_row, p_col)

   call alloc_x(u1, .true.)
   call random_number(u1)

   t1 = MPI_WTIME()
   call decomp_2d_write_one(1, u1, 'io.dat')
   t2 = MPI_WTIME()

   if (nrank == 0) write (*, *) 'I/O time: ', t2 - t1

   deallocate (u1)
   call decomp_2d_finalize
   call MPI_FINALIZE(ierror)

end program io_bench

