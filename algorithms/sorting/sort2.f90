module test1
   use iso_fortran_env, only : wp => real64
   implicit none
   private
   public :: quick, merges
contains
recursive subroutine quick(array, start_, end_)
   integer, dimension(*) :: array
   integer, intent(in) :: start_, end_
   integer :: pivot, temp
   integer :: i,j

   pivot = array((start_+end_)/2)
   if (pivot>array(start_)) then
      if (pivot>array(end_)) then
         if (array(end_)>array(start_)) then
            pivot=array(end_)
         else
            pivot=array(start_)
         endif
      endif
   else 
      if (pivot<array(end_)) then  
         if (array(end_)>array(start_)) then
            pivot=array(start_)
         else
            pivot=array(end_)
         endif
      endif
   endif



   i=start_
   j=end_

   do 
      ! find value that is bigger on left
      do while(array(i)<pivot)
         i=i+1
      enddo
      ! find value that is smaller on right
      do while(array(j)>pivot)
         j=j-1
      enddo
      if(i>=j) exit
      
      ! swap
      temp=array(j); array(j) = array(i); array(i) = temp
      i=i+1
      j=j-1
   
   enddo

   if(start_< i-1) call quick(array,start_,i-1)
   if(j+1 < end_) call quick(array, j+1, end_)

end subroutine quick

recursive subroutine merges(arr)
   integer, dimension(:), intent(inout) :: arr
   integer, allocatable, dimension(:) :: left, right
   integer :: mid, full
   integer :: i,j,k 
   if (size(arr)>1) then
      full= size(arr) 
      mid = int(size(arr) / 2)
      allocate(left(mid))
      allocate(right(full-mid))

      left=arr(:mid)
      right= arr(mid+1:)
      call merges(left)
      call merges(right)
      
      i=1
      j=1
      k=1
      do while (i<=size(left) .and. j<=size(right))
         if(left(i)<right(j)) then
            arr(k) = left(i)
            i=i+1
         else
            arr(k) = right(j)
            j=j+1
         endif
         k=k+1
      enddo

      do while (i<=size(left))
         arr(k) = left(i)
         i=i+1
         k=k+1
      enddo

      do while (j<=size(right))
         arr(k) = right(j)
         j=j+1
         k=k+1
      enddo
      print*,arr
   endif

   if(allocated (left)) deallocate(left)
   if(allocated (right)) deallocate(right)
end subroutine merges
end module test1
