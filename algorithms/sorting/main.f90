program algs
   !use sorting
   use test1
   use iso_fortran_env, only: int32, wp => real64
   implicit none

   ! Test the quicksort algorithm
   call test_sort(9)
contains
   subroutine test_sort(n)

      integer, intent(in) :: n
      integer :: i
      real(wp), dimension(n) :: a
      integer :: arr(n)
      
      call random_seed()
      do i = 1, n
         call random_number(a(i))
      end do

      arr = int(a*100)
      
      print *, "before", arr
      !call quick(arr,1,size(arr))
      call merges(arr)
      print *, "after",arr
   
   end subroutine test_sort



end program algs
