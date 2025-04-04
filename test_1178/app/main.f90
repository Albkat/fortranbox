program main
   use xtb_coloumb_klopmanohno, only: getColoumbDerivsCluster
   implicit none

   call getColoumbDerivsCluster(get_dim())

contains

   !> check the CLI arguments
   !> calculation dimension (default 1000), or -1 if not valid
   integer function get_dim()
      
      implicit none
      !! command
      intrinsic :: command_argument_count, get_command_argument
      
      !> local variables
      integer :: i, nargs, iostat

      !> buffer argument storage
      character(len=100) :: arg

      nargs = command_argument_count()

      if (nargs == 0) then
         get_dim = 1000
      else
         do i= 1, nargs
            call get_command_argument(i, arg)
            if (trim(arg) == "--dim") then
               if (i == nargs) then
                  get_dim = -1
               else
                  call get_command_argument(i+1, arg)
                  read(arg, *, iostat=iostat) get_dim
                  if (iostat /= 0 .or. get_dim <= 0) then 
                     get_dim = -1
                  else
                     return
                  endif
               end if
            endif
         enddo
      end if

      if (get_dim == -1) then
         error stop "Invalid dimension is provided"
      end if


   end function get_dim

end program main
