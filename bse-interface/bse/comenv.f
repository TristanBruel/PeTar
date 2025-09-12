***
      SUBROUTINE COMENV(M01,M1,MC1,AJ1,JSPIN1,KW1,
     &                  M02,M2,MC2,AJ2,JSPIN2,KW2,
     &                  ZPARS,ECC,SEP,JORB,
     &                  VKICK1,VKICK2,COEL,z)
*
* Common Envelope Evolution.
*
*     Author : C. A. Tout
*     Date :   18th September 1996
*
*     Redone : J. R. Hurley
*     Date :   7th July 1998
*
      IMPLICIT NONE
*
      INTEGER KW1,KW2,KW
      INTEGER KTYPE(0:14,0:14)
      COMMON /TYPES/ KTYPE
      INTEGER ceflag,tflag,ifflag,nsflag,wdflag
      COMMON /FLAGS/ ceflag,tflag,ifflag,nsflag,wdflag
*
      REAL*8 M01,M1,MC1,AJ1,JSPIN1,R1,L1,K21
      REAL*8 M02,M2,MC2,AJ2,JSPIN2,R2,L2,K22,MC22
      REAL*8 TSCLS1(20),TSCLS2(20),LUMS(10),GB(10),TM1,TM2,TN,ZPARS(20)
      REAL*8 EBINDI,EBINDF,EORBI,EORBF,ECIRC,SEPF,SEPL,MF,XX
      REAL*8 CONST,DELY,DERI,DELMF,MC3,FAGE1,FAGE2
      REAL*8 ECC,SEP,JORB,TB,OORB,OSPIN1,OSPIN2,TWOPI
      REAL*8 RC1,RC2,Q1,Q2,RL1,RL2,LAMB1,LAMB2
      REAL*8 MENV,RENV,MENVD,RZAMS,VKICK1(4),VKICK2(4)
      REAL*8 AURSUN,K3,ALPHA1,LAMBDA
      real*8 FBFAC,FBTOT,MCO
      integer ECS
      PARAMETER (AURSUN = 214.95D0,K3 = 0.21D0) 
****
      REAL*8 ftzacc
      PARAMETER (ftzacc=0.5D0)
****
      COMMON /VALUE2/ ALPHA1,LAMBDA
      LOGICAL COEL
      REAL*8 CELAMF,RL,RZAMSF
      EXTERNAL CELAMF,RL,RZAMSF

      REAL*8 bhspin1,bhspin2,rad1_bpp,rad2_bpp,lumin(2),teff1,teff2
      INTEGER ce2stageflag
      INTEGER star1,star2
      LOGICAL output,switchedCE
      REAL*8 mconvmax1,mconv1,tmin1,tonset1
      REAL*8 mconvmax2,mconv2,tmin2,tonset2
      REAL*8 ragbf,menvmax,mconvenv,celamhe,tonset
      EXTERNAL ragbf,menvmax,mconvenv,celamhe,tonset
      REAL*8 mcgbtf,lmcgbf
      EXTERNAL mcgbtf,lmcgbf
      REAL*8 z,m1endstage1,m2endstage1,reagb
*
* Common envelope evolution - entered only when KW1 = 2, 3, 4, 5, 6, 8 or 9.
*
* For simplicity energies are divided by -G.
*
      TWOPI = 2.D0*ACOS(-1.D0)
      COEL = .FALSE.

*
* In the case of 2-stage formalism (Hirai & Mandel 2022)
*
      IF(CE2STAGEFLAG.EQ.1)THEN
         KW = KW1
         CALL star(KW1,M01,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS)
         CALL hrdiag(M01,AJ1,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS,
     &           R1,L1,KW1,MC1,RC1,MENV,RENV,K21,fbfac,fbtot,mco,ecs)
         OSPIN1 = JSPIN1/(K21*R1*R1*(M1-MC1)+K3*RC1*RC1*MC1)
         if(switchedCE)then
            teff1 = 1000.d0*((1130.d0*lumin(2)/
     &           (rad2_bpp**2.d0))**(1.d0/4.d0))
         else
            teff1 = 1000.d0*((1130.d0*lumin(1)/
     &           (rad1_bpp**2.d0))**(1.d0/4.d0))
         endif
         reagb = ragbf(M1,LUMS(7),zpars(2))
         tmin1 = 1000.d0*((1130.d0*LUMS(7)/(reagb**2.d0))**(1.d0/4.d0))
         tonset1 = tonset(tmin1,z)
         mconvmax1 = menvmax(KW,M1,z)
         mconv1 = mconvenv(KW,M1,z,teff1,tmin1,tonset1,AJ1,TM1)
* if mass > 8 Msun, the mass of the envelope participating in the first stage is the mass of the convective one
* if mass < 2 Msun, the entire envelope participates in the first stage
* linear interpolation in between
         IF(M1.GE.8.0d0)THEN
            m1endstage1 = M1-mconv1
         ELSEIF(M1.LT.2.0d0)THEN
            m1endstage1 = MC1
         ELSE
            m1endstage1 = MC1 + (MENV-MC1 - mconv1) * (M1-2.0d0) /6.0d0
         ENDIF

         KW = KW2
         CALL star(KW2,M02,M2,TM2,TN,TSCLS2,LUMS,GB,ZPARS)
         CALL hrdiag(M02,AJ2,M2,TM2,TN,TSCLS2,LUMS,GB,ZPARS,
     &           R2,L2,KW2,MC2,RC2,MENV,RENV,K22,fbfac,fbtot,mco,ecs)
         OSPIN2 = JSPIN2/(K22*R2*R2*(M2-MC2)+K3*RC2*RC2*MC2)
         if(switchedCE)then
            teff2 = 1000.d0*((1130.d0*lumin(1)/
     &           (rad1_bpp**2.d0))**(1.d0/4.d0))
         else
            teff2 = 1000.d0*((1130.d0*lumin(2)/
     &           (rad2_bpp**2.d0))**(1.d0/4.d0))
         endif
         reagb = ragbf(M2,LUMS(7),zpars(2))
         tmin2 = 1000.d0*((1130.d0*LUMS(7)/(reagb**2.d0))**(1.d0/4.d0))
         tonset2 = tonset(tmin2,z)
         mconvmax2 = menvmax(KW,M2,z)
         mconv2 = mconvenv(KW,M2,z,teff2,tmin2,tonset2,AJ2,TM2)
* if mass > 8 Msun, the mass of the envelope participating in the first stage is the mass of the convective one
* if mass < 2 Msun, the entire envelope participates in the first stage
* linear interpolation in between
         IF(KW2.LE.1.OR.KW2.EQ.7)THEN
            IF(M2.GE.8.0d0)THEN
               m2endstage1 = M2-mconv2
            ELSEIF(M2.LT.2.0d0)THEN
               m2endstage1 = M2
            ELSE
               m2endstage1 = M2 - mconv2 * (M2-2.0d0) /6.0d0
            ENDIF
         ELSE
            IF(M2.GE.8.0d0)THEN
               m2endstage1 = M2-mconv2
            ELSEIF(M2.LT.2.0d0)THEN
               m2endstage1 = MC2
            ELSE
               m2endstage1 = MC2 + (M2-MC2-mconv2) * (M2-2.0d0) /6.0d0
            ENDIF
         ENDIF

*       Compute LAMBDA_HE from Picker, Hirai & Mandel 2024
         LAMB1 = celamhe(M1,z,mconv1,mconvmax1)
         LAMB2 = celamhe(M2,z,mconv2,mconvmax2)
*
* Calculate the binding energy of the giant envelope (multiplied by lambda).
*
         EBINDI = M1*(M1-m1endstage1)/(LAMB1*R1)
*
* If the secondary star is also giant-like add its envelopes energy.
*
         IF(KW2.GE.2.AND.KW2.LE.9.AND.KW2.NE.7)THEN
            EBINDI = EBINDI + M2*(M2-m2endstage1)/(LAMB2*R2)
         ENDIF
*
* Calculate the initial orbital energy
*
         EORBI = M1*M2/(2.D0*SEP)
         if(output) write(*,*)'Init CE:',
     &           M01,M1,R1,M02,M2,R2,EBINDI,EORBI
*
* Allow for an eccentric orbit.
*
         ECIRC = EORBI/(1.D0 - ECC*ECC)
*
* Calculate the final orbital energy without coalescence.
*
         EORBF = EORBI + EBINDI/ALPHA1
         SEPF = m1endstage1*m2endstage1/(2.D0*EORBF)
*
* Second stage: stable mass transfer of the radiative intershell
*
*** THIS NEEDS TO BE UPDATED WITH A CORRECT DESCRIPTION OF MASS TRANSFER ***
         IF(m1endstage1.GT.MC1)THEN
            SEPF = SEPF*((m1endstage1+m2endstage1)/(MC1+m2endstage1))
     &          *(m1endstage1/MC1)**2
     &          *EXP(-2*(m1endstage1-MC1)/m2endstage1)
         ENDIF
         EORBF = MC1*m2endstage1/(2.D0*SEPF)
         Q1 = MC1/m2endstage1
         Q2 = 1.D0/Q1
         RL1 = RL(Q1)
         RL2 = RL(Q2)
*         print*, 'Separation after 2nd stage', SEPF

      ELSE

* 
* Standard energy formalism
*
*
* Obtain the core masses and radii.
*
         KW = KW1
         CALL star(KW1,M01,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS)
         CALL hrdiag(M01,AJ1,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS,
     &        R1,L1,KW1,MC1,RC1,MENV,RENV,K21,fbfac,fbtot,mco,ecs)
         OSPIN1 = JSPIN1/(K21*R1*R1*(M1-MC1)+K3*RC1*RC1*MC1)
         MENVD = MENV/(M1-MC1)
         RZAMS = RZAMSF(M01)
         LAMB1 = CELAMF(KW,M01,L1,R1,RZAMS,MENVD,LAMBDA)
         KW = KW2
         CALL star(KW2,M02,M2,TM2,TN,TSCLS2,LUMS,GB,ZPARS)
         CALL hrdiag(M02,AJ2,M2,TM2,TN,TSCLS2,LUMS,GB,ZPARS,
     &        R2,L2,KW2,MC2,RC2,MENV,RENV,K22,fbfac,fbtot,mco,ecs)
         OSPIN2 = JSPIN2/(K22*R2*R2*(M2-MC2)+K3*RC2*RC2*MC2)
*
* Calculate the binding energy of the giant envelope (multiplied by lambda).
*
         EBINDI = M1*(M1-MC1)/(LAMB1*R1)
*
* If the secondary star is also giant-like add its envelopes's energy.
*
         EORBI = M1*M2/(2.D0*SEP)
         IF(KW2.GE.2.AND.KW2.LE.9.AND.KW2.NE.7)THEN
            MENVD = MENV/(M2-MC2)
            RZAMS = RZAMSF(M02)
            LAMB2 = CELAMF(KW,M02,L2,R2,RZAMS,MENVD,LAMBDA)
            EBINDI = EBINDI + M2*(M2-MC2)/(LAMB2*R2)
*
* Calculate the initial orbital energy
*
            IF(CEFLAG.NE.3) EORBI = MC1*MC2/(2.D0*SEP)
         ELSE
            IF(CEFLAG.NE.3) EORBI = MC1*M2/(2.D0*SEP)
         ENDIF
*
* Allow for an eccentric orbit.
*
         ECIRC = EORBI/(1.D0 - ECC*ECC)
*
* Calculate the final orbital energy without coalescence.
*
         EORBF = EORBI + EBINDI/ALPHA1
*
* If the secondary is on the main sequence see if it fills its Roche lobe.
*
         IF(KW2.LE.1.OR.KW2.EQ.7)THEN
            SEPF = MC1*M2/(2.D0*EORBF)
            Q1 = MC1/M2
            Q2 = 1.D0/Q1
            RL1 = RL(Q1)
            RL2 = RL(Q2)
         ELSE
            SEPF = MC1*MC2/(2.D0*EORBF)
            Q1 = MC1/M2
            Q2 = 1.D0/Q1
            RL1 = RL(Q1)
            RL2 = RL(Q2)
         ENDIF

      ENDIF

*
* If the secondary is on the main sequence see if it fills its Roche lobe.
*
      IF(KW2.LE.1.OR.KW2.EQ.7)THEN
* Check if any eccentricity remains in the orbit by first using 
* energy to circularise the orbit before removing angular momentum. 
* (note this should not be done in case of CE SN ... fix).  
*
         IF(EORBF.LT.ECIRC)THEN
            ECC = SQRT(1.D0 - EORBF/ECIRC)
         ELSE
            ECC = 0.D0
         ENDIF

         IF(RC1/RL1.GE.R2/RL2)THEN
*
* The helium core of a very massive star of type 4 may actually fill
* its Roche lobe in a wider orbit with a very low-mass secondary.
*
            IF(RC1.GT.RL1*SEPF)THEN
               COEL = .TRUE.
               SEPL = RC1/RL1
            ENDIF
         ELSE
            IF(R2.GT.RL2*SEPF)THEN
               COEL = .TRUE.
               SEPL = R2/RL2
            ENDIF
         ENDIF
         IF(COEL)THEN
*
            KW = KTYPE(KW1,KW2) - 100
            MC3 = MC1
            IF(KW2.EQ.7.AND.KW.EQ.4) MC3 = MC3 + M2
*
* Coalescence - calculate final binding energy.
*
            EORBF = MAX(MC1*M2/(2.D0*SEPL),EORBI)
            EBINDF = EBINDI - ALPHA1*(EORBF - EORBI)
         ELSE
*
* Primary becomes a black hole, neutron star, white dwarf or helium star.
*
            MF = M1
            M1 = MC1
            CALL star(KW1,M01,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS)
            CALL hrdiag(M01,AJ1,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS,
     &           R1,L1,KW1,MC1,RC1,MENV,RENV,K21,fbfac,fbtot,mco,ecs)
            IF(KW1.GE.13)THEN
               CALL kick(KW1,MF,M1,M2,ECC,SEPF,JORB,VKICK1,
     &              fbfac,fbtot,mco,ecs)
               IF(ECC.GT.1.D0) GOTO 30
            ENDIF
         ENDIF
      ELSE
*
* Degenerate or giant secondary. Check if the least massive core fills its
* Roche lobe.
*
* Check if any eccentricity remains in the orbit by first using 
* energy to circularise the orbit before removing angular momentum. 
* (note this should not be done in case of CE SN ... fix).  
*
         IF(EORBF.LT.ECIRC)THEN
            ECC = SQRT(1.D0 - EORBF/ECIRC)
         ELSE
            ECC = 0.D0
         ENDIF
         IF(RC1/RL1.GE.RC2/RL2)THEN
            IF(RC1.GT.RL1*SEPF)THEN
               COEL = .TRUE.
               SEPL = RC1/RL1
            ENDIF
         ELSE
            IF(RC2.GT.RL2*SEPF)THEN
               COEL = .TRUE.
               SEPL = RC2/RL2
            ENDIF
         ENDIF
*
         IF(COEL)THEN
*
* If the secondary was a neutron star or black hole the outcome
* is an unstable Thorne-Zytkow object that leaves only the core.
*
            SEPF = 0.D0
            IF(KW2.GE.13)THEN
               MC1 = MC2
*** Retain primary partially in case of a BH secondary
               IF(KW2.EQ.14) MC1 = MC2 + (ftzacc*M1)
               M1 = MC1
               MC2 = 0.D0
               M2 = 0.D0
               KW1 = KW2
               KW2 = 15
               AJ1 = 0.D0
*
* The envelope mass is not required in this case.
*
               GOTO 30
            ENDIF
*
            KW = KTYPE(KW1,KW2) - 100
            MC3 = MC1 + MC2
*
* Calculate the final envelope binding energy.
*
            EORBF = MAX(MC1*MC2/(2.D0*SEPL),EORBI)
            EBINDF = EBINDI - ALPHA1*(EORBF - EORBI)
*
* Check if we have the merging of two degenerate cores and if so
* then see if the resulting core will survive or change form.
*
            IF(KW1.EQ.6.AND.(KW2.EQ.6.OR.KW2.GE.11))THEN
               CALL dgcore(KW1,KW2,KW,MC1,MC2,MC3,EBINDF)
            ENDIF
            IF(KW1.LE.3.AND.M01.LE.ZPARS(2))THEN
               IF((KW2.GE.2.AND.KW2.LE.3.AND.M02.LE.ZPARS(2)).OR.
     &             KW2.EQ.10)THEN
                  CALL dgcore(KW1,KW2,KW,MC1,MC2,MC3,EBINDF)
                  IF(KW.GE.10)THEN
                     KW1 = KW
                     M1 = MC3
                     MC1 = MC3
                     IF(KW.LT.15) M01 = MC3
                     AJ1 = 0.D0
                     MC2 = 0.D0
                     M2 = 0.D0
                     KW2 = 15
                     GOTO 30
                  ENDIF
               ENDIF
            ENDIF
*
         ELSE
*
* The cores do not coalesce - assign the correct masses and ages.
*
            MF = M1
            M1 = MC1
            CALL star(KW1,M01,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS)
            CALL hrdiag(M01,AJ1,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS,
     &           R1,L1,KW1,MC1,RC1,MENV,RENV,K21,fbfac,fbtot,mco,ecs)
            IF(KW1.GE.13)THEN
               CALL kick(KW1,MF,M1,M2,ECC,SEPF,JORB,VKICK1,
     &              fbfac,fbtot,mco,ecs)
               IF(ECC.GT.1.D0) GOTO 30
            ENDIF
            MF = M2
            KW = KW2
            M2 = MC2
            CALL star(KW2,M02,M2,TM2,TN,TSCLS2,LUMS,GB,ZPARS)
            CALL hrdiag(M02,AJ2,M2,TM2,TN,TSCLS2,LUMS,GB,ZPARS,
     &           R2,L2,KW2,MC2,RC2,MENV,RENV,K22,fbfac,fbtot,mco,ecs)
            IF(KW2.GE.13.AND.KW.LT.13)THEN
               CALL kick(KW2,MF,M2,M1,ECC,SEPF,JORB,VKICK2,
     &              fbfac,fbtot,mco,ecs)
               IF(ECC.GT.1.D0) GOTO 30
            ENDIF
         ENDIF
      ENDIF
*
      IF(COEL)THEN
         MC22 = MC2
         IF(KW.EQ.4.OR.KW.EQ.7)THEN
* If making a helium burning star calculate the fractional age 
* depending on the amount of helium that has burnt.
            IF(KW1.LE.3)THEN
               FAGE1 = 0.D0
            ELSEIF(KW1.GE.6)THEN
               FAGE1 = 1.D0
            ELSE
               FAGE1 = (AJ1 - TSCLS1(2))/(TSCLS1(13) - TSCLS1(2))
            ENDIF
            IF(KW2.LE.3.OR.KW2.EQ.10)THEN
               FAGE2 = 0.D0
            ELSEIF(KW2.EQ.7)THEN
               FAGE2 = AJ2/TM2
               MC22 = M2
            ELSEIF(KW2.GE.6)THEN
               FAGE2 = 1.D0
            ELSE
               FAGE2 = (AJ2 - TSCLS2(2))/(TSCLS2(13) - TSCLS2(2))
            ENDIF
         ENDIF
      ENDIF
*
* Now calculate the final mass following coelescence.  This requires a
* Newton-Raphson iteration.
*
      IF(COEL)THEN
*
* Calculate the orbital spin just before coalescence. 
*
         TB = (SEPL/AURSUN)*SQRT(SEPL/(AURSUN*(MC1+MC2)))
         OORB = TWOPI/TB
*
         XX = 1.D0 + ZPARS(7)
         IF(EBINDF.LE.0.D0)THEN
            MF = MC3
            GOTO 20
         ELSE
            CONST = ((M1+M2)**XX)*(M1-MC1+M2-MC22)*EBINDF/EBINDI
         ENDIF
*
* Initial Guess.
*
         MF = MAX(MC1 + MC22,(M1 + M2)*(EBINDF/EBINDI)**(1.D0/XX))
   10    DELY = (MF**XX)*(MF - MC1 - MC22) - CONST
*        IF(ABS(DELY/MF**(1.D0+XX)).LE.1.0D-02) GOTO 20
         IF(ABS(DELY/MF).LE.1.0D-03) GOTO 20
         DERI = MF**ZPARS(7)*((1.D0+XX)*MF - XX*(MC1 + MC22))
         DELMF = DELY/DERI
         MF = MF - DELMF
         GOTO 10
*
* Set the masses and separation.
*
   20    IF(MC22.EQ.0.D0) MF = MAX(MF,MC1+M2)
         M2 = 0.D0
         M1 = MF
         KW2 = 15
*
* Combine the core masses.
*
         IF(KW.EQ.2)THEN
            CALL star(KW,M1,M1,TM2,TN,TSCLS2,LUMS,GB,ZPARS)
            IF(GB(9).GE.MC1)THEN
               M01 = M1
               AJ1 = TM2 + (TSCLS2(1) - TM2)*(AJ1-TM1)/(TSCLS1(1) - TM1)
               CALL star(KW,M01,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS)
            ENDIF
         ELSEIF(KW.EQ.7)THEN
            M01 = M1
            CALL star(KW,M01,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS)
            AJ1 = TM1*(FAGE1*MC1 + FAGE2*MC22)/(MC1 + MC22)
         ELSEIF(KW.EQ.4.OR.MC2.GT.0.D0.OR.KW.NE.KW1)THEN
            IF(KW.EQ.4) AJ1 = (FAGE1*MC1 + FAGE2*MC22)/(MC1 + MC22)
            MC1 = MC1 + MC2
            MC2 = 0.D0
*
* Obtain a new age for the giant.
*
            CALL gntage(MC1,M1,KW,ZPARS,M01,AJ1)
            CALL star(KW,M01,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS)
         ENDIF
         CALL hrdiag(M01,AJ1,M1,TM1,TN,TSCLS1,LUMS,GB,ZPARS,
     &        R1,L1,KW,MC1,RC1,MENV,RENV,K21,fbfac,fbtot,mco,ecs)
         JSPIN1 = OORB*(K21*R1*R1*(M1-MC1)+K3*RC1*RC1*MC1)
         KW1 = KW
         ECC = 0.D0
      ELSE
*
* Check if any eccentricity remains in the orbit by first using 
* energy to circularise the orbit before removing angular momentum. 
* (note this should not be done in case of CE SN ... fix).  
*
         IF(EORBF.LT.ECIRC)THEN
            ECC = SQRT(1.D0 - EORBF/ECIRC)
         ELSE
            ECC = 0.D0
         ENDIF
*
* Set both cores in co-rotation with the orbit on exit of CE, 
*
         TB = (SEPF/AURSUN)*SQRT(SEPF/(AURSUN*(M1+M2)))
         OORB = TWOPI/TB
         JORB = M1*M2/(M1+M2)*SQRT(1.D0-ECC*ECC)*SEPF*SEPF*OORB
*        JSPIN1 = OORB*(K21*R1*R1*(M1-MC1)+K3*RC1*RC1*MC1)
*        JSPIN2 = OORB*(K22*R2*R2*(M2-MC2)+K3*RC2*RC2*MC2)
*
* or, leave the spins of the cores as they were on entry.
* Tides will deal with any synchronization later.
*
         JSPIN1 = OSPIN1*(K21*R1*R1*(M1-MC1)+K3*RC1*RC1*MC1)
         JSPIN2 = OSPIN2*(K22*R2*R2*(M2-MC2)+K3*RC2*RC2*MC2)
      ENDIF
   30 SEP = SEPF
      RETURN
      END
***
