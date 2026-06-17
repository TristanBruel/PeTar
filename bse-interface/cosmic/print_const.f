**** For debuging
      SUBROUTINE printConst()

      implicit none
      INCLUDE 'const_bse.h'
      integer k

      write(*,*) '----- SSE/BSE common block parameter list: -----'
      write(*,*) 'flags: remnantflag:',remnantflag,' wdflag:',wdflag,
     &     ' bhflag:',bhflag,' windflag:',windflag,' qcflag:',qcflag 
      write(*,*) 'ceflags: ceflag:',ceflag,
     &     ' ce2stageflag:',ce2stageflag,
     &     ' cekickflag:',cekickflag,
     &     ' cemergeflag:',cemergeflag,
     &     ' cehestarflag:',cehestarflag
      write(*,*) 'metvars: zsun:',zsun
      write(*,*) 'windvars: neta:',neta,' bwind:',bwind,
     &     ' hewind:',hewind,' beta:',beta,' xi:',xi,' acc2:',acc2,
     &     ' epsnov:',epsnov,' eddfac:',eddfac,' gamma:',gamma
      write(*,*) 'cevars: alpha1:',alpha1,' lambdaf:',lambdaf
      write(*,*) 'snvars: pisn:',pisn,' sigma:',sigma,
     &     ' sigmdadiv:',sigmadiv,' bhsigmafrac:',bhsigmafrac,
     &     ' ecsn:',ecsn,' bhspinmag:',bhspinmag,
     &     ' kickflag:',kickflag
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
