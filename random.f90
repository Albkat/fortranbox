program random
   !$omp_lib
   implicit none


   character(len=:), allocatable :: str
   real :: rnd
   integer :: irnd

   call random_number(rnd)

   irnd = transfer(rnd,irnd)
   print *, irnd,mod (irnd,16)

   
end program random