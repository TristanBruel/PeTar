      SUBROUTINE gntage(mc,mt,kw,zpars,m0,aj,id)
      IMPLICIT NONE
      INCLUDE 'const_bse.h'
      
      real*8 mc,mt,zpars,m0,aj
      integer kw ,id
      
      !WRITE(*,*) 'Calling SSE_gntage'
      CALL SSE_gntage(mc,mt,kw,zpars,m0,aj,id)
      
      END
