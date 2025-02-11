module sorting
   use iso_fortran_env, only : wp => real64
   implicit none
   public :: sort
   interface sort
      module procedure :: mergesort
      module procedure :: quicksort
   endinterface

contains
   
   recursive subroutine mergesort(array)
      integer, dimension(:), intent(inout)  :: array
      integer, allocatable :: left(:), right(:)
      integer :: mid
      integer :: i, j, k

      if (size(array) > 1) then
         mid = int(size(array)/2)
         allocate(left(mid))
         allocate(right(size(array)-mid))
         left  = array(:mid)
         right = array(mid+1:)
         call mergesort(left)
         call mergesort(right)

         i=1
         j=1
         k=1
         
         do while(i <= size(left) .and. j <= size(right))
            if(left(i) < right(j)) then
               array(k) = left(i)
               i = i+1
            else
               array(k) = right(j)
               j=j+1
            endif
            k=k+1
         enddo

         do while(i <= size(left))
            array(k) = left(i)
            i=i+1
            k=k+1
         enddo
         
         do while(j <= size(right))
            array(k) = right(j)
            j=j+1
            k=k+1
         enddo

         print*,array
               
      endif
      
      if(allocated(left)) deallocate(left)
      if(allocated(right)) deallocate(right)

   end subroutine  mergesort

   recursive subroutine quicksort(array, first, last)
      integer, dimension(*) :: array
      integer, intent(in) :: first, last
      integer :: i, j
      integer :: pivot, temp,mid

      ! 1. Choose a pivot value !
      mid = array((first+last)/2)
      if (mid>array(first)) then
         if (mid<array(last)) then
            pivot = mid
         else
            if (array(first)>array(last)) then
               pivot = array(first)
            else
               pivot = array(last)
            end if
         end if
      else
         if (mid>array(last)) then
            pivot = mid
         else
            if (array(first)<array(last)) then
               pivot = array(first)
            else
               pivot = array(last)
            end if
         end if
      end if

      i = first
      j = last

      ! 2. Partition the array into two sub-arrays !
      do while (i < j) ! until the pointers cross
         
         ! 2.1 Find the first element from the left that is greater than the pivot
         do while (array(i) < pivot)
            i = i + 1
         end do
         
         ! 2.2 Find the first element from the right that is less than the pivot
         do while (array(j) > pivot)
            j = j - 1
         end do
         
         if (i < j) then
            ! 2.3 Swap the elements
            temp = array(i); array(i) = array(j); array(j) = temp
            i = i + 1
            j = j - 1
         end if
      end do

      ! 3. Recursively sort the sub-arrays
      if (first < i-1) call quicksort(array, first, i-1) ! left sub-array
      if (j+1 < last) call quicksort(array, j+1, last) ! right sub-array

   end subroutine quicksort


end module sorting
