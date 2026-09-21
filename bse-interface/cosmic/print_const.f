**** Print COSMIC common block parameters for debugging
      SUBROUTINE printConst()

      implicit none
      INCLUDE 'const_bse.h'
      integer k

      write(*,*) '----- SSE/BSE common block parameter list: -----'
      write(*,*) 'windvars: neta:',neta,' bwind:',bwind,
     &     ' hewind:',hewind,' beta:',beta,' xi:',xi
      write(*,*) 'windvars: acc2:',acc2,' epsnov:',epsnov,
     &     ' eddfac:',eddfac,' gamma:',gamma,' LBV_flag:',LBV_flag
      write(*,*) 'cevars: alpha1:',alpha1,' lambdaf:',lambdaf
      write(*,*) 'ceflags: ceflag:',ceflag,' cekickflag:',cekickflag,
     &     ' cemergeflag:',cemergeflag,' cehestarflag:',cehestarflag,
     &     ' ussn:',ussn
      write(*,*) 'snvars: sigma:',sigma,' sigmadiv:',sigmadiv,
     &     ' bhsigmafrac:',bhsigmafrac,' mxns:',mxns
      write(*,*) 'snvars: pisn:',pisn,' ecsn:',ecsn,
     &     ' ecsn_mlow:',ecsn_mlow,' bhspinmag:',bhspinmag
      write(*,*) 'snvars: kickflag:',kickflag,
     &     ' rembar_massloss:',rembar_massloss
      write(*,*) 'flags: tflag:',tflag,' ifflag:',ifflag,
     &     ' remnantflag:',remnantflag,' wdflag:',wdflag,
     &     ' bhflag:',bhflag
      write(*,*) 'flags: windflag:',windflag,' qcflag:',qcflag,
     &     ' eddlimflag:',eddlimflag,' bhspinflag:',bhspinflag
      write(*,*) 'flags: aic:',aic,' rejuvflag:',rejuvflag,
     &     ' htpmb:',htpmb,' ST_cr:',ST_cr,' ST_tide:',ST_tide
      write(*,*) 'flags: bdecayfac:',bdecayfac,' grflag:',grflag,
     &     ' bhms_coll_flag:',bhms_coll_flag,' wd_mass_lim:',wd_mass_lim
      write(*,*) 'flags: rtmsflag:',rtmsflag,
     &     ' maltsev_mode:',maltsev_mode
      write(*,*) 'metvars: zsun:',zsun
      write(*,*) 'mixvars: rejuv_fac:',rejuv_fac
      write(*,*) 'points: pts1:',pts1,' pts2:',pts2,' pts3:',pts3
      write(*,*) 'se_flags: using_metisse:',using_metisse,
     &     ' using_sse:',using_sse
      write(*,*) 'rand1: idum1:',idum1
      write(*,*) 'types: ktype:'
      do K=0,14
         write(*,*) ktype(k,:)
      end do
      flush(6)

      return

      end
