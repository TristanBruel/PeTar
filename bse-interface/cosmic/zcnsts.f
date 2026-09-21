      SUBROUTINE zcnsts(z,zpars)
      IMPLICIT NONE
      INCLUDE 'const_bse.h'
      
      real*8 z,zpars(20)
      integer :: ierr

      if (using_METISSE.eq.1) then
          ! Initialize SSE common blocks so trdot/trflow can fall back to
          ! SSE_star/SSE_hrdiag when called with id=0 (temporary/scratch calls).
          CALL SSE_zcnsts(z,zpars)
          !WRITE(*,*) 'Calling METISSE_zcnsts',using_METISSE
          CALL METISSE_zcnsts(z,zpars,ierr)
           if (ierr/=0) call assign_error()

      elseif (using_SSE.eq.1) then
          !WRITE(*,*) 'Calling SSE_zcnsts'
          CALL SSE_zcnsts(z,zpars)
      endif

      END
