**** For debuging
      SUBROUTINE printConst()

      implicit none
      INCLUDE 'const_bse.h'
      integer k

      write(*,*) '----- SSE/BSE common block parameter list: -----'
      write(*,*) 'flags: tflag',tflag,' ifflag',ifflag,
     &     ' remnantflag:',remnantflag,' wdflag:',wdflag,
     &     ' bhflag:',bhflag,' windflag:',windflag,' qcflag:',qcflag,
     &     ' eddlimflag:',eddlimflag,' bhspinflag:',bhspinflag, 
     &     ' aic:',aic,' rejuvflag:',rejuvflag,' htpmb:',htpmb,
     &     ' ST_cr:',ST_cr,' ST_tide:',ST_tide,
     &     ' bdecayfac:',bdecayfac,' grflag:',grflag, 
     &     ' bhms_coll_flag:',bhms_coll_flag,
     &     ' wd_mass_lim:',wd_mass_lim,' rtmsflag:',rtmsflag,
     &     ' maltsev_mode:',maltsev_mode
      write(*,*) 'mtvars: don_lim:',don_lim,' acc_lim:',acc_lim,
     &     ' smt_periastron_check:',smt_periastron_check
      write(*,*) 'ceflags: ceflag:',ceflag,
     &     ' ce2stageflag:',ce2stageflag,
     &     ' cekickflag:',cekickflag,
     &     ' cemergeflag:',cemergeflag,
     &     ' cehestarflag:',cehestarflag,' ussn:',ussn
      write(*,*) 'metvars: zsun:',zsun
      write(*,*) 'windvars: neta:',neta,' bwind:',bwind,
     &     ' hewind:',hewind,' beta:',beta,' xi:',xi,' acc2:',acc2,
     &     ' epsnov:',epsnov,' eddfac:',eddfac,' gamma:',gamma,
     &     ' LBV_flag',LBV_flag
      write(*,*) 'cevars: alpha1:',alpha1,' lambdaf:',lambdaf
      write(*,*) 'magvars: bconst:',bconst,' ck:',ck
      write(*,*) 'snvars: sigma:',sigma,' sigmdadiv:',sigmadiv,
     &     ' bhsigmafrac:',bhsigmafrac,
     &     ' polar_kick_angle:',polar_kick_angle,' pisn:',pisn,
     &     ' ecsn:',ecsn,' ecsn_mlow:',ecsn_mlow,
     &     ' bhspinmag:',bhspinmag,' mxns:',mxns,
     &     ' rembar_massloss:',rembar_massloss,
     &     ' mm_mu_ns:',mm_mu_ns,' mm_mu_bh:',mm_mu_bh,
     &     ' maltsev_fallback:',maltsev_fallback,
     &     ' maltsev_pf_prob:',maltsev_pf_prob,
     &     ' kickflag:',kickflag,' fryer_mass_limit',fryer_mass_limit,
     &     ' ppi_co_shift:',ppi_co_shift,' ppi_extra_ml:',ppi_extra_ml,
     &     ' fryer_fmix:',fryer_fmix,
     &     ' fryer_mcrit_nsbh:',fryer_mcrit_nsbh
      write(*,*) 'mixvars: rejuv_fac:',rejuv_fac
      write(*,*) 'points: pts1:',pts1,' pts2:',pts2,' pts3:',pts3
*      write(*,*) 'tstepc: dmmax:',dmmax,' drmax:',drmax
      write(*,*) 'types: ktype:'
      do K=0,14
         write(*,*) ktype(k,:)
      end do
*      write(*,*) '----------------------------------------------------'
      flush(6)

      return

      end
