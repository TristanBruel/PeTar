      real*8 FUNCTION mlwind(kw,lum,r,mt,mc,rl,z,id)
      IMPLICIT NONE
      INCLUDE 'const_bse.h'
      
      integer kw,id
      real*8 lum,r,mt,mc,rl,z
      
      real*8 SSE_mlwind
      external SSE_mlwind
    
      !WRITE(*,*) 'Calling SSE_mlwind'
      mlwind = SSE_mlwind(kw,lum,r,mt,mc,rl,z)

      END
