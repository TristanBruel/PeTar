      SUBROUTINE zcnsts(z,zpars)
      IMPLICIT NONE
      INCLUDE 'const_bse.h'
      
      real*8 z,zpars(20)
      integer :: ierr

      !WRITE(*,*) 'Calling SSE_zcnsts'
      CALL SSE_zcnsts(z,zpars)

      END
