! 	! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 	! subroutine intp_u(uf, u, uk, li, lj, ll)
! 	! 	real UF(li, lj + 1, ll), VF(li, lj + 1, ll), WF(li, lj, ll)
! 	! 	dimension uf(li, lj + 1, ll), u(li, lj, ll)
! 	! 	do k = 1, ll
! 	! 		do j = 1, lj
! 	! 			do i = 1, li
! 	! 				uk = 0.01*((uf(i, j, k) + uf(i, j + 1, k))*0.5)
! 	! 				if (abs(uk).gt.200) then
! 	! 					u(i, j, k) = 0.0
! 	! 				else
! 	! 					u(i, j, k) = uk
! 	! 				end if
! 	! 			end do
! 	! 		end do
! 	! 	end do
! 	! end subroutine intp_u

! 	! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 	! subroutine intp_v(vf, v, vk, li, lj, ll)
! 	! 	dimension vf(li, lj + 1, ll), v(li, lj, ll)
! 	! 	do k = 1, ll
! 	! 		do i = 1, li
! 	! 			do j = 1, lj
! 	! 				vk = 0.01*((vf(i, j, k) + vf(i, j + 1, k))*0.5)
! 	! 				if (abs(vk).gt.200) then
! 	! 					v(i, j, k) = 0.0
! 	! 				else
! 	! 					v(i, j, k) = vk
! 	! 				end if
! 	! 			end do
! 	! 		end do
! 	! 	end do
! 	! end subroutine intp_v

! 	! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 	! subroutine intp_w(wf, w, wk, li, lj, ll)
! 	! 	dimension wf(li, lj, ll), w(li, lj, ll)
! 	! 	do i = 1, li
! 	! 		do j = 1, lj
! 	! 			do k = 1, ll
! 	! 				wk = 0.01*wf(i, j, k)
! 	! 				if (abs(wk).gt.200) then
! 	! 					w(i, j, k) = 0.0
! 	! 				else
! 	! 					w(i, j, k) = wk
! 	! 				end if
! 	! 			end do
! 	! 		end do
! 	! 	end do
! 	! end subroutine intp_w
! 	! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! 	!------------------------------------------------------------------ -
! 	REAL FUNCTION UNIFRD(IJ)
! 	!------------------------------------------------------------------ -
! 	INTEGER L, C, T30
! 	REAL UU
! 	PARAMETER(L = 843314861, C = 453816695, T30 = 2.0**30, UU = 2.0**31)
! 	IJ = L*IJ + C
! 	IF(IJ.LT.0) IJ = (IJ + T30) + T30
! 	UNIFRD = MOD(REAL(IJ), UU)
! 	UNIFRD = UNIFRD / UU
! END FUNCTION UNIFRD

! !SEIKI - RANSU
! !------------------------------------------------------------------ -
! REAL FUNCTION SEIKIB(IJ)
! !------------------------------------------------------------------ -
! REAL UNIFRD
! SEIKIB = 0
! do K = 1, 12
! 	SEIKIB = SEIKIB + UNIFRD(IJ)
! end do
! SEIKIB = SEIKIB - 6
! END FUNCTION SEIKIB






! 	!*********************************************************************************************
! 	!****************************** SEIKIB OF METAMORPHOSIS & GROWTH ******************************
! 	!各粒子の変態開始時間を正規分布に設定する(AVE&SDは篠田D論に参照しました、適宜変更)
! 	!各粒子の成長速度は(体長 - 3.6) / 変態までの日齢により算出する(AVE&SDは篠田D論に参照しました、適宜変更)
! 	!meta為變態時間 tlseikib為最大體長 growth為成長速率
! 	!       IF(NKAI == 1) THEN
! 	!check point
! 	!print*, SEIKIB(IJ)!
! 	!	   DO n=1,NPCL
! 	!		  META_SEIKIB(n)=SEIKIB(IJ)*19.6+0
! 	!		  TL_SEIKIB(n)=447.0
! 	!		  GROWTH_SEIKIB(n)=(TL_SEIKIB(n)-3.6)/META_SEIKIB(n)
! 	!	   write(30,'(1X,F5.1,1X,F7.2,1X,F5.1)')META_SEIKIB(n),GROWTH_SEIKIB(n),TL_SEIKIB(n)
! 	!print*, META_SEIKIB(n), GROWTH_SEIKIB(n), TL_SEIKIB(n)
! 	!	   end do
! 	!	   close(30)

! 	!	   ENDIF
! ! ========================================================================================================
! ! module swim
! ! 				!  ***** BY XX  *****  
! ! 			!***** MIGRATION DURATION & OTOLITH INCREMENTS  *****
! ! 			DAY=NKAI*DDT/3600./24.
! ! 			!	   IF(NKAI==1)THEN
! ! 			!	   OTODAY(I)=DDT/3600./24.
! ! 			!	   ENDIF

! ! 			!	   IF(TEMP(III, JJJ, LLL)>10.)THEN !水温が10度以下耳石成長が停止
! ! 			!	   OTODAY(I)=OTODAY(I)+1./24.
! ! 			!	   ENDIF

! ! 			!***** TOTAL LENGTH *****
! ! 			!	   IF(DAY <= META_SEIKIB(i))THEN !変態開始時間平均115.2日、SD 19.6日、変態開始後TL成長停止(Pre - leptocephalus 9d, Leptocephalus 106d)GROWTH_SEIKIB
! ! 			!	   TL=GROWTH_SEIKIB(i)*DAY+3.6 !Linear Equation, 単位(㎜)
! ! 			!      IF(TL_SEIKIB(i)>100) print*, i, TL_SEIKIB(i)
! ! 			!	   ELSE
! ! 			!	   TL = TL_SEIKIB(i)
! ! 			!      IF(TL>100) print*, '2.  ', i, TL, TL_SEIKIB(i), GROWTH_SEIKIB(i)
! ! 			!	   ENDIF

! ! 			!***** SWIMMING SPEED *****
! ! 			!      TL = 0.4315*DAY + 8.5259 !Linear Equation, 単位(㎜)
! ! 			!      TL = 64.17816355908316*(1 - 1 * EXP(-0.013830852398122474*(NKAI*DDT / 3600. / 24.))) !Von Bertalanffy, 単位(㎜)
! ! 			!       IF(DAY<= META_SEIKIB(i))THEN
! ! 			!	     SWIMS=0.
! ! 			!       ELSE

! ! 			!	    ENDIF


! ! 			!SWIMMING SPEED
! ! 			if (MOD(NKAI*INT(DDT),24*3600).GT.12*3600) then
! ! 				SWIMS = 0. !(m/sec)
! ! 			else
! ! 				SWIMS = Speed/100. !(m/sec)
! ! 			endif
! 	! !   ************************************
! 	! 		!   經向緯向的最終位置 = 海流求得的位置 + 游泳移動的位置
! 	! 		PCLXS=PCLXS+XSWIMS*DDT
! 	! 		PCLYS=PCLYS+YSWIMS*DDT
! 	! 		!   write(*, '(F10.6)') SEIKIB(IJ)



! ! 			IF(H(III, JJJ).LE.0.) GOTO 300


! ! 			!*****水平の移動方向の定義***********
! ! 			!*****磁力線(伏角)の浅い方向へ遊泳する(0°の場合)*************
! ! ! !Geomagnetic field reading
! ! 			IF(method==4) open(23,file = '/Volumes/Higuchi_JCOPE/Prosessed_data/M_intensity/M_'//dateyear(NI)//'',FORM='UNFORMATTED',status="unknown")
! ! 			IF(method==5) open(23,file = '/Volumes/Higuchi_JCOPE/Prosessed_data/M_inclination/M_'//dateyear(NI)//'',FORM='UNFORMATTED',status="unknown")
! ! 			IF(method==6) open(23,file = '/Volumes/Higuchi_JCOPE/Prosessed_data/M_declination/M_'//dateyear(NI)//'',FORM='UNFORMATTED',status="unknown")
! ! 			do j=1,lj
! ! 				do i=1,li
! ! 					read(23) magnet(i,j)
! ! 				end do
! ! 			end do
! ! 			close(23)
! ! 			!WRITE(*,*) 'MAGNET=',magnet(III, JJJ)
! ! 			MAG_N=magnet(III, JJJ+1)
! ! 			MAG_NE=magnet(III+1, JJJ+1)
! ! 			MAG_E=magnet(III+1, JJJ)
! ! 			MAG_SE=magnet(III+1, JJJ-1)
! ! 			MAG_S=magnet(III, JJJ-1)
! ! 			MAG_SW=magnet(III-1, JJJ-1)
! ! 			MAG_W=magnet(III-1, JJJ)
! ! 			MAG_NW=magnet(III-1, JJJ+1)

! ! 			EMAG=MAG_E+0.70710678*(MAG_NE+MAG_SE)
! ! 			WMAG=MAG_W+0.70710678*(MAG_NW+MAG_SW)
! ! 			NMAG=MAG_N+0.70710678*(MAG_NE+MAG_NW)
! ! 			SMAG=MAG_S+0.70710678*(MAG_SE+MAG_SW)
! ! 			XMAG=WMAG-EMAG
! ! 			YMAG=SMAG-NMAG

! ! 			XSEIBUN=XMAG/SQRT(XMAG**2.0+YMAG**2.0)
! ! 			YSEIBUN=YMAG/SQRT(XMAG**2.0+YMAG**2.0)

! ! 			IF(XSEIBUN==0.and.YSEIBUN>=0) THEN
! ! 				DEG0=0.
! ! 			ELSE IF(XSEIBUN>0.and.YSEIBUN>0) THEN
! ! 				DEG0=(atan(XSEIBUN/YSEIBUN)*(180/3.1415926535))+Angle
! ! 			ELSE IF(XSEIBUN>0.and.YSEIBUN==0) THEN
! ! 				DEG0=90.
! ! 			ELSE IF(XSEIBUN>0.and.YSEIBUN<0) THEN
! ! 				DEG0=180.+(atan(XSEIBUN/YSEIBUN)*(180/3.1415926535))+Angle
! ! 			ELSE IF(XSEIBUN==0.and.YSEIBUN<0) THEN
! ! 				DEG0=180.
! ! 			ELSE IF(XSEIBUN<0.and.YSEIBUN<0) THEN
! ! 				DEG0=180.+(atan(XSEIBUN/YSEIBUN)*(180/3.1415926535))+Angle
! ! 			ELSE IF(XSEIBUN<0.and.YSEIBUN==0) THEN
! ! 				DEG0=270.
! ! 			ELSE IF(XSEIBUN<0.and.YSEIBUN>0) THEN
! ! 				DEG0=360.+(atan(XSEIBUN/YSEIBUN)*(180/3.1415926535))+Angle
! ! 			END IF

! ! 			IF(DEG0>=360.) then
! ! 				DEG=DEG0-360.
! ! 			ELSE IF(DEG0<0.) then
! ! 				DEG=DEG0+360.
! ! 			ELSE
! ! 				DEG=DEG0
! ! 			END IF

! ! 			IF(DEG==0.) then
! ! 				XSWIMS=0.
! ! 				YSWIMS=SWIMS
! ! 			ELSE IF(DEG>0.AND.DEG<90.0) then
! ! 				YSWIMS=COS((DEG)*(3.1415926535/180))*SWIMS
! ! 				XSWIMS=SIN((DEG)*(3.1415926535/180))*SWIMS
! ! 			ELSE IF(DEG==90.) then
! ! 				XSWIMS=SWIMS
! ! 				YSWIMS=0.
! ! 			ELSE IF(DEG>90.AND.DEG<180.0) then
! ! 				YSWIMS=COS((DEG)*(3.1415926535/180))*SWIMS
! ! 				XSWIMS=SIN((DEG)*(3.1415926535/180))*SWIMS
! ! 			ELSE IF(DEG==180.) then
! ! 				XSWIMS=0.
! ! 				YSWIMS=SWIMS*(-1)
! ! 			ELSE IF(DEG>180.AND.DEG<270.0) then
! ! 				YSWIMS=COS((DEG)*(3.1415926535/180))*SWIMS
! ! 				XSWIMS=SIN((DEG)*(3.1415926535/180))*SWIMS
! ! 			ELSE IF(DEG==270.) then
! ! 				XSWIMS=SWIMS*(-1)
! ! 				YSWIMS=0.
! ! 			ELSE IF(DEG>270.AND.DEG<360.0) then
! ! 				YSWIMS=COS((DEG)*(3.1415926535/180))*SWIMS
! ! 				XSWIMS=SIN((DEG)*(3.1415926535/180))*SWIMS
! ! 			END IF
! ! 			!   write(*,*) '==========================================='


! ! ====================================================================================================


! ! 			!***** EFFECT OF SALINITY  *****
! ! 			!      write(*, *) SWIMS*DDT
! ! 			!      塩分 = SAL * 1000 + 35
! ! 			!	   IF(SWIMS.EQ.0.)THEN
! ! 			!	     XSWIMS=0.
! ! 			!		 YSWIMS=0.
! ! 			!	   GOTO 333
! ! 			!	   ENDIF
! ! 			!--------------------------------
! ! 			!      IF(H(III, JJJ).LE.0.) GOTO 300
! ! 			!------------------------------------------------------------------------------ -
! ! 			!	   DO NLAYER = LLL, 2, -1 !(水深150m = Layer18層)
! ! 			!	   IF(SAL(III - 1, JJJ + 1, NLAYER)<-0.035) GOTO 555
! ! 			!	   IF(SAL(III - 1, JJJ, NLAYER)<-0.035) GOTO 555
! ! 			!       IF(SAL(III, JJJ + 1, NLAYER)<-0.035) GOTO 555

! ! 			!	   IF(SAL(III + 1, JJJ, NLAYER)<-0.035) GOTO 555
! ! 			!	   IF(SAL(III, JJJ - 1, NLAYER)<-0.035) GOTO 555
! ! 			!	   IF(SAL(III + 1, JJJ + 1, NLAYER)<-0.035) GOTO 555
! ! 			!       IF(SAL(III + 1, JJJ - 1, NLAYER)<-0.035) GOTO 555
! ! 			!       IF(SAL(III - 1, JJJ - 1, NLAYER)<-0.035) GOTO 555

! ! 			!      IF(NLAYER.LE.0) print*, '1.  ', I, LLL, NLAYER, H(III, JJJ)
! ! 			!	SALL = NLAYER
! ! 			!	   GOTO 500

! ! 			555        continue
! ! 			!       end do

! ! 			!	SALL = NLAYER
! ! 			500   CONTINUE

! ! 			!IF(SALL.LE.0) print*, 'SALL. ', H(III, JJJ), I, III, JJJ, LLL, NLAYER, SALL
! ! 			!SALL = 1
! ! 			!    SALE=SAL(III+1,JJJ,SALL)*1000+35
! ! 			!    SALW=SAL(III-1,JJJ,SALL)*1000+35
! ! 			!    SALN=SAL(III,JJJ+1,SALL)*1000+35
! ! 			!    SALS=SAL(III,JJJ-1,SALL)*1000+35
! ! 			!    SALNE=SAL(III+1,JJJ+1,SALL)*1000+35
! ! 			!	SALNW=SAL(III-1,JJJ+1,SALL)*1000+35
! ! 			!	SALSE=SAL(III+1,JJJ-1,SALL)*1000+35
! ! 			!	SALSW=SAL(III-1,JJJ-1,SALL)*1000+35

! ! 			!	  IF(SALE<=0) SALE=0
! ! 			!	  IF(SALW<=0) SALW=0
! ! 			!	  IF(SALN<=0) SALN=0
! ! 			!	  IF(SALS<=0) SALS=0
! ! 			!	  IF(SALNE<=0) SALNE=0
! ! 			!	  IF(SALNW<=0) SALNW=0
! ! 			!	  IF(SALSE<=0) SALSE=0
! ! 			!	  IF(SALSW<=0) SALSW=0																	


! ! 			!------------------------------------------------------------------------------ -
! ! 			!DO 555 SALL = LLL, 1
! ! 			!IF(SAL(III, JJJ, 1)<-0.035) SALC = -0.035
! ! 			!501       IF(SALC >= -0.035) GOTO 502
! ! 			!IF(SALC<-0.035) SALC = SAL(III, JJJ, SALL - 1)
! ! 			!
! ! 			!IF(SAL(III + 1, JJJ, 1)*1000. + 35.<0.) SALE = -0.035
! ! 			!502       IF(SALE >= -0.035) GOTO 503
! ! 			!IF(SALE*1000. + 35.<0.) SALE = SAL(III + 1, JJJ, SALL)
! ! 			!
! ! 			!IF(SAL(III - 1, JJJ, 1)*1000. + 35.<0.) SALW = -0.035
! ! 			!503       IF(SALW >= -0.035) GOTO 504
! ! 			!IF(SALW*1000. + 35.<0.) SALW = SAL(III - 1, JJJ, SALL)
! ! 			!
! ! 			!IF(SAL(III, JJJ + 1, 1)*1000. + 35.<0.) SALN = -0.035
! ! 			!504       IF(SALN >= -0.035) GOTO 505
! ! 			!IF(SALN*1000. + 35.<0.) SALN = SAL(III, JJJ + 1, SALL)
! ! 			!
! ! 			!IF(SAL(III, JJJ - 1, 1)*1000. + 35.<0.) SALS = -0.035
! ! 			!505       IF(SALS >= -0.035) GOTO 506
! ! 			!IF(SALS*1000. + 35.<0.) SALS = SAL(III, JJJ - 1, SALL)
! ! 			!
! ! 			!IF(SAL(III + 1, JJJ + 1, 1)*1000. + 35.<0.) SALNE = -0.035
! ! 			!506       IF(SALNE >= -0.035) GOTO 507
! ! 			!IF(SALNE*1000. + 35.<0.) SALNE = SAL(III + 1, JJJ + 1, SALL)
! ! 			!
! ! 			!IF(SAL(III - 1, JJJ + 1, 1)*1000. + 35.<0.) SALNW = -0.035
! ! 			!507       IF(SALNW >= -0.035) GOTO 508
! ! 			!IF(SALNW*1000. + 35.<0.) SALNW = SAL(III - 1, JJJ + 1, SALL)
! ! 			!
! ! 			!IF(SAL(III + 1, JJJ - 1, 1)*1000. + 35.<0.) SALSE = -0.035
! ! 			!508       IF(SALSE >= -0.035) GOTO 509
! ! 			!IF(SALSE*1000. + 35.<0.) SALSE = SAL(III + 1, JJJ - 1, SALL)
! ! 			!
! ! 			!IF(SAL(III - 1, JJJ - 1, 1)*1000. + 35.<0.) SALSW = -0.035
! ! 			!509       IF(SALSW >= -0.035) GOTO 550
! ! 			!IF(SALSW*1000. + 35.<0.) SALSW = SAL(III - 1, JJJ - 1, SALL)
! ! 			!
! ! 			!550       continue
! ! 			!555   CONTINUE
! ! 			!----------------------------------------------------------------------------------------
! ! 			!      ESAL=SALE+0.70710678*(SALNE+SALSE)
! ! 			!      WSAL=SALW+0.70710678*(SALNW+SALSW)
! ! 			!	  NSAL=SALN+0.70710678*(SALNE+SALNW)
! ! 			!	  SSAL=SALS+0.70710678*(SALSE+SALSW)

! ! 			!	  XSAL=WSAL-ESAL
! ! 			!	  YSAL=SSAL-NSAL

! ! 			!== == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == =
! ! 			!XSAL = 0
! ! 			!YSAL = 0
! ! 			!IF(SAL(III - 1, JJJ, LLL)*1000. + 35.>1.) XSAL = XSAL + (SAL(III - 1, JJJ, LLL) - SAL(III, JJJ, LLL))
! ! 			!IF(SAL(III + 1, JJJ, LLL)*1000. + 35.>1.) XSAL = XSAL + (SAL(III, JJJ, LLL) - SAL(III + 1, JJJ, LLL))
! ! 			!IF(SAL(III, JJJ - 1, LLL)*1000. + 35.>1.) YSAL = YSAL + (SAL(III, JJJ - 1, LLL) - SAL(III, JJJ, LLL))
! ! 			!IF(SAL(III, JJJ + 1, LLL)*1000. + 35.>1.) YSAL = YSAL + (SAL(III, JJJ, LLL) - SAL(III, JJJ + 1, LLL))

! ! 			!IF(SAL(III - 1, JJJ - 1, LLL)*1000. + 35.>1.)THEN
! ! 			!XSAL = XSAL + 0.70710678*(SAL(III - 1, JJJ - 1, LLL) - SAL(III, JJJ, LLL))
! ! 			!YSAL = YSAL + 0.70710678*(SAL(III - 1, JJJ - 1, LLL) - SAL(III, JJJ, LLL))
! ! 			!ENDIF

! ! 			!IF(SAL(III - 1, JJJ + 1, LLL)*1000. + 35.>1.)THEN
! ! 			!XSAL = XSAL + 0.70710678*(SAL(III - 1, JJJ + 1, LLL) - SAL(III, JJJ, LLL))
! ! 			!YSAL = YSAL + 0.70710678*(SAL(III, JJJ, LLL) - SAL(III - 1, JJJ + 1, LLL))
! ! 			!ENDIF

! ! 			!IF(SAL(III + 1, JJJ - 1, LLL)*1000. + 35.>1.)THEN
! ! 			!XSAL = XSAL + 0.70710678*(SAL(III, JJJ, LLL) - SAL(III + 1, JJJ - 1, LLL))
! ! 			!YSAL = YSAL + 0.70710678*(SAL(III + 1, JJJ - 1, LLL) - SAL(III, JJJ, LLL))
! ! 			!ENDIF

! ! 			!IF(SAL(III + 1, JJJ + 1, LLL)*1000. + 35.>1.)THEN
! ! 			!XSAL = XSAL + 0.70710678*(SAL(III, JJJ, LLL) - SAL(III + 1, JJJ + 1, LLL))
! ! 			!YSAL = YSAL + 0.70710678*(SAL(III, JJJ, LLL) - SAL(III + 1, JJJ + 1, LLL))
! ! 			!ENDIF

! ! 			!   求得經向緯向的游泳速度
! ! 			!        IF(XSAL.EQ.0.AND.YSAL.EQ.0.) THEN
! ! 			!		  XSWIMS=0.
! ! 			!		  YSWIMS=0.
! ! 			!	    ELSE
! ! 			!		  XSWIMS=XSAL/SQRT(XSAL**2.0+YSAL**2.0)*SWIMS
! ! 			!		  YSWIMS=YSAL/SQRT(XSAL**2.0+YSAL**2.0)*SWIMS
! ! 			!		ENDIF

! ! end module swim

! ! ==================================================================================================

! ! subroutine angle_name
! 	!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
! 	!=!=!=!=!=!=!=!=!ファイル名の設定!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
! 	!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
! 	!    if(Angle.lt.-9) then
! 	!         write(angle_name,'(I3)') Angle
! 	!      if(Speed.lt.10) then
! 	!         write(dir_Speed,'(A1,I3,A4,I1,A1,I4)') 'D',Angle,'_S00',Speed,'_',year
! 	!      else if(Speed.lt.100) then
! 	!         write(dir_speed,'(A1,I3,A3,I2,A1,I4)') 'D',Angle,'_S0',Speed,'_',year
! 	!      else
! 	!         write(dir_speed,'(A1,I3,A2,I3,A1,I4)') 'D',Angle,'_S',Speed,'_',year
! 	!      end if
! 	!    else if(Angle.lt.0) then
! 	!         Angle=Angle*(-1)
! 	!         write(angle_name,'(A2,I1)') '-0',Angle
! 	!      if(Speed.lt.10) then
! 	!         write(dir_speed,'(A3,I1,A4,I1,A1,I4)') 'D-0',Angle,'_S00',Speed,'_',year
! 	!      else if(Speed.lt.100) then
! 	!         write(dir_speed,'(A3,I1,A3,I2,A1,I4)') 'D-0',Angle,'_S0',Speed,'_',year
! 	!      else
! 	!         write(dir_speed,'(A3,I1,A2,I3,A1,I4)') 'D-0',Angle,'_S',Speed,'_',year
! 	!      end if
! 	!    else if(Angle.lt.10) then
! 	!         write(angle_name,'(A2,I1)') '+0',Angle
! 	!      if(Speed.lt.10) then
! 	!         write(dir_speed,'(A3,I1,A4,I1,A1,I4)') 'D+0',Angle,'_S00',Speed,'_',year
! 	!      else if(Speed.lt.100) then
! 	!         write(dir_speed,'(A3,I1,A3,I2,A1,I4)') 'D+0',Angle,'_S0',Speed,'_',year
! 	!      else
! 	!         write(dir_speed,'(A3,I1,A2,I3,A1,I4)') 'D+0',Angle,'_S',Speed,'_',year
! 	!      end if
! 	!    else
! 	!         write(angle_name,'(A1,I2)') '+',Angle
! 	!      if(Speed.lt.10) then
! 	!         write(dir_speed,'(A2,I2,A4,I1,A1,I4)') 'D+',Angle,'_S00',Speed,'_',year
! 	!      else if(Speed.lt.100) then
! 	!         write(dir_speed,'(A2,I2,A3,I2,A1,I4)') 'D+',Angle,'_S0',Speed,'_',year
! 	!      else
! 	!         write(dir_speed,'(A2,I2,A2,I3,A1,I4)') 'D+',Angle,'_S',Speed,'_',year
! 	!      end if
! 	!    end if
! 	! !=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!

! 	! !=!=!=!=!=!=!=!=!Speed名の設定!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
! 	!      if(Speed.lt.10) then
! 	!         write(speed_name,'(A1,I1)') '0',Speed
! 	!      else if(Speed.lt.100) then
! 	!         write(speed_name,'(I2)') Speed
! 	!      end if
! 	!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
! ! end subroutine angle_name

! ! =====================================================================================================

! ! subroutine set_vertical

! ! 	if (MOD(NKAI*INT(DDT),24*3600).GT.12*3600) THEN
! ! 		do I=1,NPCL
! ! 			LAYER0=1.                                                                                                                                      !            |
! ! 			do LAYER=2.,28.,1.                                                                                                                              !            |
! ! 				IF (abs(TEMP(INT(PCLX(I)/IDSX(i))+1,INT(PCLY(I)/IDSY)+1,LAYER0)-5.0) > abs(TEMP(INT(PCLX(I)/IDSX(i))+1,INT(PCLY(I)/IDSY)+1,LAYER)-5.0)) then   !            |
! ! 					LAYER0=LAYER                                                                                                                                  !            |
! ! 				END IF                                                                                                                                         !            |
! ! 			end do                                                                                                                                          !            |
! ! 			IF (LAYER0==1) PCLZ(I)=0.                                                                                                                     !            |
! ! 			IF (LAYER0==2) PCLZ(I)=5.                                                                                                                     !            |
! ! 			IF (LAYER0==3) PCLZ(I)=10.                                                                                                                    !        昼間の水深
! ! 			IF (LAYER0==4) PCLZ(I)=50.                                                                                                                    !     水温5°Cに最も近い
! ! 			IF (LAYER0==5) PCLZ(I)=75.                                                                                                                    !        水深を選択
! ! 			IF (LAYER0==6) PCLZ(I)=100.                                                                                                                   !            |
! ! 			IF (LAYER0==7) PCLZ(I)=150.                                                                                                                   !            |
! ! 			IF (LAYER0==8) PCLZ(I)=200.                                                                                                                   !            |
! ! 			IF (LAYER0==9) PCLZ(I)=250.                                                                                                                   !            |
! ! 			IF (LAYER0==10) PCLZ(I)=300.                                                                                                                  !            |
! ! 			IF (LAYER0==11) PCLZ(I)=400.                                                                                                                  !            |
! ! 			IF (LAYER0==12) PCLZ(I)=500.                                                                                                                  !            |
! ! 			IF (LAYER0==13) PCLZ(I)=600.                                                                                                                  !            |
! ! 			IF (LAYER0==14) PCLZ(I)=700.                                                                                                                  !            |
! ! 			IF (LAYER0==15) PCLZ(I)=800.                                                                                                                  !            |
! ! 			IF (LAYER0==16) PCLZ(I)=900.                                                                                                                  !            |
! ! 			IF (LAYER0==17) PCLZ(I)=1000.                                                                                                                 !            |
! ! 			IF (LAYER0==18) PCLZ(I)=1200.                                                                                                                 !            |
! ! 			IF (LAYER0==19) PCLZ(I)=1400.                                                                                                                 !            |
! ! 			IF (LAYER0==20) PCLZ(I)=1600.                                                                                                                 !            |
! ! 			IF (LAYER0==21) PCLZ(I)=1800.                                                                                                                 !            |
! ! 			IF (LAYER0==22) PCLZ(I)=2000.                                                                                                                 !            |
! ! 			IF (LAYER0==23) PCLZ(I)=2500.                                                                                                                 !            |
! ! 			IF (LAYER0==24) PCLZ(I)=3000.                                                                                                                 !            |
! ! 			IF (LAYER0==25) PCLZ(I)=3500.                                                                                                                 !            |
! ! 			IF (LAYER0==26) PCLZ(I)=4000.                                                                                                                 !            |
! ! 			IF (LAYER0==27) PCLZ(I)=5000.                                                                                                                 !            |
! ! 			IF (LAYER0==28) PCLZ(I)=6000.                                                                                                                 !            |
! ! 		end do                                                                                                                                             !!!!!!!!!!!!!!!!!!!!!!!!!
! ! 	else
! ! 				do I=1,NPCL
! ! 			PCLZ(I)=200                                                                                                                                      !!!!!!!!!!!!!!!!!!!!!!!!!
! ! 		end do                                                                                                                                             !!!!!!!!!!!!!!!!!!!!!!!!!
! ! 	endif
! ! end subroutine set_vertical
! ! ===================================================================================================
! ! subroutine henkou
! ! 	!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
! ! 	!=!=!=!=!=!=!Orientation methodを設定=!=!=!=!=!=!=!
! ! 	!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
! ! 	if (method == 1) then
! ! 		method_name = 'Ideal'
! ! 		method_number = '1'
! ! 	elseif (method == 2) then
! ! 		method_name = 'Rando'
! ! 		method_number = '2'
! ! 	elseif (method == 3) then
! ! 		method_name = 'Compa'
! ! 		method_number = '3'
! ! 	elseif (method == 4) then
! ! 		method_name = 'Total'
! ! 		method_number = '4'
! ! 	elseif (method == 5) then
! ! 		method_name = 'Incli'
! ! 		method_number = '5'
! ! 	elseif (method == 6) then
! ! 		method_name = 'Decli'
! ! 		method_number = '6'
! ! 	end if
! ! 	!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!

! ! 	!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
! ! 	!=!=!=!=!=!=計算を開始する位置を設定=!=!=!=!=!=!
! ! 	!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
! ! 	if (releas == 1) start = 'release1'
! ! 	if (releas == 2) start = 'Miyazaki'
! ! 	if (releas == 3) start = 'Tonegawa'
! ! 	if (releas == 4) start = 'Aomori00'
! ! 	if (releas == 5) start = '4station'
! ! 	!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!

! ! 	!計算を開始する期間を'mm.yyyy'で記述(適宜変更)
! ! 	if(month.lt.10) then
! ! 		if(dayset.lt.10) then
! ! 			write(file,'(I4,A1,I1,A1,I1)') year,'0',month,'0',dayset
! ! 		else
! ! 			write(file,'(I4,A1,I1,I2)') year,'0',month,dayset
! ! 		end if
! ! 	else
! ! 		if(dayset.lt.10) then
! ! 			write(file,'(I4,I2,A1,I1)') year,month,'0',dayset
! ! 		else
! ! 			write(file,'(I4,I2,I2)') year,month,dayset
! ! 		end if
! ! 	end if
! ! end subroutine henkou
! ! =============================================================================================
