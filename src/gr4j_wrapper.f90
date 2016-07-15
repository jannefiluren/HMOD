      subroutine gr4j_wrapper(NTimes,Prec,PET,St,StUH1,StUH2,Q_all,St_all,Param)

      implicit none

! Input and output variables

      integer, intent(in) :: NTimes
      doubleprecision, dimension(NTimes) :: Prec, PET
      doubleprecision, dimension(2) :: St
      doubleprecision, dimension(20) :: StUH1
      doubleprecision, dimension(40) :: StUH2
      doubleprecision, dimension(4) :: Param
	  
	  doubleprecision, dimension(NTimes) :: Q_all
	  doubleprecision, dimension(NTimes,2) :: St_all
	  
!parameters, internal states and variables

      integer :: itime
      integer NH
      parameter (NH=20)
      doubleprecision OrdUH1(NH), OrdUH2(2*NH)
      doubleprecision D

! Computation of UH ordinates

      D = 2.5

      OrdUH1 = 0.
      OrdUH2 = 0.
	  
      CALL UH1(OrdUH1,Param(4),D)
      CALL UH2(OrdUH2,Param(4),D)

! Run model

      do itime = 1,NTimes

      CALL MOD_GR4J(St,StUH1,StUH2,OrdUH1,OrdUH2,Param,Prec(itime),PET(itime),Q_all(itime))
	  
	  St_all(itime,1) = St(1)
	  St_all(itime,2) = St(2)
	  
      end do

      end subroutine gr4j_wrapper

	  
	  
	  



      subroutine gr4j_wrapper_ens(NTimes,Prec,PET,St,StUH1,StUH2,Q_all,Param)

      implicit none

! Input and output variables

      integer, intent(in) :: NTimes
      doubleprecision, dimension(NTimes) :: Prec, PET
      doubleprecision, dimension(2) :: St
      doubleprecision, dimension(20) :: StUH1
      doubleprecision, dimension(40) :: StUH2
      doubleprecision, dimension(4) :: Param
	  
	  doubleprecision, dimension(NTimes) :: Q_all
	  
!parameters, internal states and variables

      integer :: itime
      integer NH
      parameter (NH=20)
      doubleprecision OrdUH1(NH), OrdUH2(2*NH)
      doubleprecision D

! Computation of UH ordinates

      D = 2.5

      OrdUH1 = 0.
      OrdUH2 = 0.
	  
      CALL UH1(OrdUH1,Param(4),D)
      CALL UH2(OrdUH2,Param(4),D)

! Run model

      do itime = 1,NTimes

      CALL MOD_GR4J(St,StUH1,StUH2,OrdUH1,OrdUH2,Param,Prec(itime),PET(itime),Q_all(itime))
	  
      end do

      end subroutine gr4j_wrapper_ens



!**********************************************************************
      SUBROUTINE MOD_GR4J(St,StUH1,StUH2,OrdUH1,OrdUH2,Param,P1,E,Q)
! Run on a single time step with the GR4J model
! Inputs:
!       St     Vector of model states in stores at the beginning of the time step [mm]
!       StUH1  Vector of model states in Unit Hydrograph 1 at the beginning of the time step [mm]
!       StUH2  Vector of model states in Unit Hydrograph 2 at the beginning of the time step [mm]
!       OrdUH1 Vector of ordinates in UH1 [-]
!       OrdUH2 Vector of ordinates in UH2 [-]
!       Param  Vector of model parameters [various units]
!       P1     Value of rainfall during the time step [mm/day]
!       E      Value of potential evapotranspiration during the time step [mm/day]
! Outputs:
!       St     Vector of model states in stores at the end of the time step [mm]
!       StUH1  Vector of model states in Unit Hydrograph 1 at the end of the time step [mm]
!       StUH2  Vector of model states in Unit Hydrograph 2 at the end of the time step [mm]
!       Q      Value of simulated flow at the catchment outlet for the time step [mm]
!**********************************************************************
      Implicit None
      INTEGER NH,NParam
      PARAMETER (NH=20)
      PARAMETER (NParam=4)
      DOUBLEPRECISION St(2),StUH1(NH),StUH2(2*NH)
      DOUBLEPRECISION OrdUH1(NH),OrdUH2(2*NH)
      DOUBLEPRECISION Param(NParam)
      DOUBLEPRECISION P1,E,Q
      DOUBLEPRECISION A,B,EN,ER,PN,PR,PS,WS,tanHyp
      DOUBLEPRECISION PERC,PRHU1,PRHU2,EXCH,QR,QD
      DOUBLEPRECISION AE,AEXCH1,AEXCH2
      INTEGER K

      DATA B/0.9/
      DOUBLEPRECISION TWS, Sr, Rr ! speed-up

      A=Param(1)

! Interception and production store
      IF(P1.LE.E) THEN
      EN=E-P1
      PN=0.
      WS=EN/A
      IF(WS.GT.13)WS=13.
	  ! speed-up
      TWS = tanHyp(WS)
      Sr = St(1)/A
      ER=St(1)*(2.-Sr)*TWS/(1.+(1.-Sr)*TWS)
      ! ER=X(2)*(2.-X(2)/A)*tanHyp(WS)/(1.+(1.-X(2)/A)*tanHyp(WS))
	  ! fin speed-up  
      AE=ER+P1
      St(1)=St(1)-ER
      PR=0.
      ELSE
      EN=0.
      AE=E
      PN=P1-E
      WS=PN/A
      IF(WS.GT.13)WS=13.
	  ! speed-up
      TWS = tanHyp(WS)
      Sr = St(1)/A
      PS=A*(1.-Sr*Sr)*TWS/(1.+Sr*TWS)
      ! PS=A*(1.-(X(2)/A)**2.)*tanHyp(WS)/(1.+X(2)/A*tanHyp(WS))
	  ! fin speed-up
      PR=PN-PS
      St(1)=St(1)+PS
      ENDIF

! Percolation from production store
      IF(St(1).LT.0.)St(1)=0.
	  ! speed-up
	  ! (9/4)**4 = 25.62891
 	  Sr = St(1)/Param(1)
	  Sr = Sr * Sr
	  Sr = Sr * Sr
      PERC=St(1)*(1.-1./SQRT(SQRT(1.+Sr/25.62891)))
	  ! PERC=X(2)*(1.-(1.+(X(2)/(9./4.*Param(1)))**4.)**(-0.25))
	  ! fin speed-up
      St(1)=St(1)-PERC

      PR=PR+PERC

! Split of effective rainfall into the two routing components
      PRHU1=PR*B
      PRHU2=PR*(1.-B)

! Convolution of unit hydrograph UH1
      DO K=1,MAX(1,MIN(NH-1,INT(Param(4)+1.)))
      StUH1(K)=StUH1(K+1)+OrdUH1(K)*PRHU1
      ENDDO
      StUH1(NH)=OrdUH1(NH)*PRHU1

! Convolution of unit hydrograph UH2
      DO K=1,MAX(1,MIN(2*NH-1,2*INT(Param(4)+1.)))
      StUH2(K)=StUH2(K+1)+OrdUH2(K)*PRHU2
      ENDDO
      StUH2(2*NH)=OrdUH2(2*NH)*PRHU2

! Potential intercatchment semi-exchange
	  ! speed-up
	  Rr = St(2)/Param(3)
      EXCH=Param(2)*Rr*Rr*Rr*SQRT(Rr)
      ! EXCH=Param(2)*(X(1)/Param(3))**3.5
	  ! fin speed-up

! Routing store
      AEXCH1=EXCH
      IF((St(2)+StUH1(1)+EXCH).LT.0) AEXCH1=-St(2)-StUH1(1)
      St(2)=St(2)+StUH1(1)+EXCH
      IF(St(2).LT.0.)St(2)=0.
	  ! speed-up
	  Rr = St(2)/Param(3)
	  Rr = Rr * Rr
	  Rr = Rr * Rr
      QR=St(2)*(1.-1./SQRT(SQRT(1.+Rr)))
      ! QR=X(1)*(1.-(1.+(X(1)/Param(3))**4.)**(-1./4.))
	  ! fin speed-up
      St(2)=St(2)-QR

! Runoff from direct branch QD
      AEXCH2=EXCH
      IF((StUH2(1)+EXCH).LT.0) AEXCH2=-StUH2(1)
      QD=MAX(0.,StUH2(1)+EXCH)

! Total runoff
      Q=QR+QD
      IF(Q.LT.0.) Q=0.

      ENDSUBROUTINE


!**********************************************************************
      SUBROUTINE UH1(OrdUH1,C,D)
! Computation of ordinates of GR unit hydrograph UH1 using successive differences on the S curve SS1
! Inputs:
!    C: time constant
!    D: exponent
! Outputs:
!    OrdUH1: NH ordinates of discrete hydrograph
!**********************************************************************
      Implicit None
      INTEGER NH
      PARAMETER (NH=20)
      DOUBLEPRECISION OrdUH1(NH)
      DOUBLEPRECISION C,D,SS1
      INTEGER I

      DO I=1,NH
      OrdUH1(I)=SS1(I,C,D)-SS1(I-1,C,D)
      ENDDO
      ENDSUBROUTINE


!**********************************************************************
      SUBROUTINE UH2(OrdUH2,C,D)
! Computation of ordinates of GR unit hydrograph HU2 using successive differences on the S curve SS2
! Inputs:
!    C: time constant
!    D: exponent
! Outputs:
!    OrdUH2: 2*NH ordinates of discrete hydrograph
!**********************************************************************
      Implicit None
      INTEGER NH
      PARAMETER (NH=20)
      DOUBLEPRECISION OrdUH2(2*NH)
      DOUBLEPRECISION C,D,SS2
      INTEGER I

      DO I =1,2*NH
      OrdUH2(I)=SS2(I,C,D)-SS2(I-1,C,D)
      ENDDO
      ENDSUBROUTINE


!**********************************************************************
      FUNCTION SS1(I,C,D)
! Values of the S curve (cumulative HU curve) of GR unit hydrograph UH1
! Inputs:
!    C: time constant
!    D: exponent
!    I: time-step
! Outputs:
!    SS1: Values of the S curve for I
!**********************************************************************
      Implicit None
      DOUBLEPRECISION C,D,SS1
      INTEGER I,FI

      FI=I
      IF(FI.LE.0.) THEN
      SS1=0.
      RETURN
      ENDIF
      IF(FI.LT.C) THEN
      SS1=(FI/C)**D
      RETURN
      ENDIF
      SS1=1.
      ENDFUNCTION


!**********************************************************************
      FUNCTION SS2(I,C,D)
! Values of the S curve (cumulative HU curve) of GR unit hydrograph UH2
! Inputs:
!    C: time constant
!    D: exponent
!    I: time-step
! Outputs:
!    SS2: Values of the S curve for I
!**********************************************************************
      Implicit None
      DOUBLEPRECISION C,D,SS2
      INTEGER I,FI

      FI=I
      IF(FI.LE.0.) THEN
      SS2=0.
      RETURN
      ENDIF
      IF(FI.LE.C) THEN
      SS2=0.5*(FI/C)**D
      RETURN
      ENDIF
      IF(FI.LT.2.*C) THEN
      SS2=1.-0.5*(2.-FI/C)**D
      RETURN
      ENDIF
      SS2=1.
      ENDFUNCTION


!**********************************************************************
      FUNCTION tanHyp(Val)
! Computation of hyperbolic tangent
!**********************************************************************
      Implicit None
      DOUBLEPRECISION Val,ValExp,tanHyp

      ValExp=EXP(Val)
      tanHyp=(ValExp - 1./ValExp)/(ValExp + 1./ValExp)
      RETURN
      ENDFUNCTION


