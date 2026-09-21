***********************************************************************
* cosmic_stubs.f — No-op stubs for COSMIC output-tracking routines.
*
* PeTar does not use the SSE/BSE output arrays (scm/bcm/spp/bpp);
* these stubs silently discard the writes so the link step succeeds.
* Always included, regardless of METISSE_PATH.
*
* METISSE-specific stubs live in metisse_stubs.f and are excluded
* when the real METISSE library is linked (METISSE_PATH is set).
***********************************************************************

*---------------------------------------------------------------------
* SSE single-star output routines (called from evolv1.f).
* PeTar does not use SSE output tracking; silently discard.
*---------------------------------------------------------------------

      SUBROUTINE WRITESPP(jp,tphys,evolve_type,
     &                    mass,kw,aj,tm,mc,r,m0,lum,
     &                    teff,rc,menv,renv,ospin,b0,
     &                    bacc,tacc,epoch,bhspin)
      IMPLICIT NONE
      integer jp, kw
      real*8 tphys,evolve_type,mass,aj,tm,mc,r,m0,lum
      real*8 teff,rc,menv,renv,ospin,b0,bacc,tacc,epoch,bhspin
      RETURN
      END

      SUBROUTINE WRITESCM(ip,tphys,kw,m0,mass,lum,r,teff,
     &                    mc,rc,menv,renv,epoch,deltam,
     &                    ospin,b0,sn)
      IMPLICIT NONE
      integer ip, kw
      real*8 tphys,m0,mass,lum,r,teff,mc,rc,menv,renv
      real*8 epoch,deltam,ospin,b0,sn
      RETURN
      END
