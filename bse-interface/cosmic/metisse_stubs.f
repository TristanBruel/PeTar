***********************************************************************
* metisse_stubs.f — No-op stubs for all METISSE front-end routines.
*
* Included only when building WITHOUT the real METISSE library
* (i.e. when METISSE_PATH is not set in the Makefile).  When the real
* METISSE library is linked, exclude this file so the real symbols are
* used instead.
***********************************************************************

*---------------------------------------------------------------------
* METISSE stellar-evolution back-end stubs
* (called only inside "if (using_METISSE.eq.1)" guards)
*---------------------------------------------------------------------

      SUBROUTINE METISSE_star(kw,mass,mt,tm,tn,tscls,lums,GB,zpars,
     &                         dtm,id)
      IMPLICIT NONE
      integer kw, id
      real*8 mass,mt,tm,tn,tscls(20),lums(10),GB(10),zpars(20),dtm
      RETURN
      END

      SUBROUTINE METISSE_hrdiag(mass,aj,mt,tm,tn,tscls,lums,GB,zpars,
     &                           r,lum,kw,mc,rc,menv,renv,k2,mcx,id)
      IMPLICIT NONE
      integer kw, id
      real*8 mass,aj,mt,tm,tn,tscls(20),lums(10),GB(10),zpars(20)
      real*8 r,lum,mc,rc,menv,renv,k2,mcx
      RETURN
      END

      SUBROUTINE METISSE_gntage(mc,mt,kw,zpars,m0,aj,id)
      IMPLICIT NONE
      integer kw, id
      real*8 mc,mt,zpars(20),m0,aj
      RETURN
      END

      SUBROUTINE METISSE_deltat(id,age,dt,dtr)
      IMPLICIT NONE
      integer id
      real*8 age,dt,dtr
      RETURN
      END

      REAL*8 FUNCTION METISSE_mlwind(kw,lum,r,mt,mc,rl,z,id)
      IMPLICIT NONE
      integer kw, id
      real*8 lum,r,mt,mc,rl,z
      METISSE_mlwind = 0.0d0
      RETURN
      END

      SUBROUTINE METISSE_zcnsts(z,zpars,ierr)
      IMPLICIT NONE
      real*8 z,zpars(20)
      integer ierr
      ierr = 0
      RETURN
      END

*---------------------------------------------------------------------
* METISSE track management (called inside using_METISSE guards)
*---------------------------------------------------------------------

      SUBROUTINE allocate_track(num, mass)
      IMPLICIT NONE
      integer num
      real*8 mass(*)
      RETURN
      END

      SUBROUTINE dealloc_track()
      RETURN
      END

      SUBROUTINE initialize_front_end(name)
      IMPLICIT NONE
      character*(*) name
      RETURN
      END

*---------------------------------------------------------------------
* METISSE utility stubs (called inside using_METISSE guards)
*---------------------------------------------------------------------

      SUBROUTINE set_star_type(j)
      IMPLICIT NONE
      integer j
      RETURN
      END

      SUBROUTINE check_error(err)
      IMPLICIT NONE
      integer err
      err = 0
      RETURN
      END

      SUBROUTINE assign_error()
      RETURN
      END

      SUBROUTINE get_bhspin(bhspin, id)
      IMPLICIT NONE
      real*8 bhspin
      integer id
      bhspin = 0.0d0
      RETURN
      END

*---------------------------------------------------------------------
* comenv_lambda — only called inside "if (using_METISSE.eq.1)"
* Return a safe default (lambda = 0.5).
*---------------------------------------------------------------------

      SUBROUTINE comenv_lambda(kw,m0,lum,r,menvd,lambdaf,
     &                          starid,lamb)
      IMPLICIT NONE
      integer kw, starid
      real*8 m0,lum,r,menvd,lambdaf,lamb
      lamb = 0.5d0
      RETURN
      END
