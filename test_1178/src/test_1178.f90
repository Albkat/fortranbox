module xtb_coloumb_klopmanohno
   use iso_fortran_env, only: dp => real64
   use omp_lib
   implicit none
   private

   public :: getColoumbDerivsCluster
contains
   subroutine getColoumbDerivsCluster(dim)
      
      
      !> @params matrix dimension 
      integer, intent(in) :: dim
      
      !> thread id
      integer :: ID

      !> loop control variables
      integer :: iat, jat, ii, jj 

      !$omp parallel private(ID, ii, jj, iat, jat)
      id = omp_get_thread_num()
      !$omp do
      do iat = 1, dim
         do jat = 1, dim
            print*, "ID: ", ID, iat, jat
            !> do something with ii and jj
         end do
      end do
      !$omp end do
      !$omp end parallel 

   end subroutine getColoumbDerivsCluster
end module xtb_coloumb_klopmanohno
