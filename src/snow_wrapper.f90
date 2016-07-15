      subroutine snow_wrapper(NTimes,NZones,Prec,Tair,SWE,Param,MeltRain_all,SWE_all)

      implicit none

! Input and output variables

      integer, intent(in) :: NTimes, NZones
      integer :: itime, izone

      doubleprecision, dimension(NTimes,NZones) :: Prec, Tair, MeltRain_all, SWE_all
      doubleprecision, dimension(NZones) :: SWE
      doubleprecision, dimension(1) :: Param

! Run snow model

      do izone = 1,NZones
      	do itime = 1,NTimes
	
      		call SNOW_MOD(SWE(izone),Prec(itime,izone),Tair(itime,izone),MeltRain_all(itime,izone),Param)

      		SWE_all(itime,izone) = SWE(izone)

      	end do
      end do

      end subroutine snow_wrapper


	  
	  

      subroutine snow_wrapper_ens(NTimes,NZones,Prec,Tair,SWE,Param,MeltRain_all)

      implicit none

! Input and output variables

      integer, intent(in) :: NTimes, NZones
      integer :: itime, izone

      doubleprecision, dimension(NTimes,NZones) :: Prec, Tair, MeltRain_all
      doubleprecision, dimension(NZones) :: SWE
      doubleprecision, dimension(1) :: Param

! Run snow model

      do izone = 1,NZones
      	do itime = 1,NTimes
	
      		call SNOW_MOD(SWE(izone),Prec(itime,izone),Tair(itime,izone),MeltRain_all(itime,izone),Param)
      		
      	end do
      end do

      end subroutine snow_wrapper_ens



!**********************************************************************

      subroutine SNOW_MOD(SWE,Prec,Ta,MeltRain,Param)

      implicit none

      integer NParam
      parameter (NParam = 1)

      doubleprecision :: SWE
      doubleprecision :: Prec
      doubleprecision :: Ta
      doubleprecision :: MeltRain
      doubleprecision :: Param(NParam)

      doubleprecision Psolid
      doubleprecision Pliquid
      doubleprecision Melt

! Compute solid and liquid precipitation

      Psolid = Prec

      if(Ta > 0.0) Psolid = 0.0

      Pliquid = Prec - Psolid

! Compute snow melt

      Melt = Param(1)*Ta

      if(Ta<0.0) Melt = 0

      if(Melt>SWE) Melt = SWE

! Update snow storage

      SWE = SWE + Psolid - Melt

! Compute water available for runoff

      MeltRain = Melt + Pliquid

      end subroutine SNOW_MOD

!**********************************************************************