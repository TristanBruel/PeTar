      SUBROUTINE star(kw,mass,mt,tm,tn,tscls,lums,GB,zpars,dtm,id)
      IMPLICIT NONE
      INCLUDE 'const_bse.h'
      
      real*8 mass,mt,tm,tn,tscls(20),lums(10),GB(10),zpars(20),dtm
      integer kw ,id
      
      if (using_METISSE.eq.1 .and. id.gt.0) then
*         Normal METISSE call: id>0 means a valid tarr slot.
          !WRITE(*,*) 'Calling METISSE_star'
          CALL METISSE_star(kw,mass,mt,tm,tn,tscls,lums,GB,zpars,dtm,id)

      elseif (using_SSE.eq.1 .or. using_METISSE.eq.1) then
*         SSE fallback: id<=0 means a scratch/temporary caller (e.g. trdot)
*         that has no tarr slot.  SSE commons were initialised by the
*         SSE_zcnsts call inside zcnsts() so this is safe.
          !WRITE(*,*) 'Calling SSE_star (fallback, id<=0)'
          CALL SSE_star(kw,mass,mt,tm,tn,tscls,lums,GB,zpars)
      endif

      END
