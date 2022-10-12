!****************************************************************
program main
!****************************************************************

parameter(li = 866, lj = 620, ll = 28)  !読み込むJCOPE2データのグリッド数
integer, parameter::NPCL = 1000    !投入粒子数
integer, parameter::IJ = 2
real, parameter::pi = 3.1415926535
real, parameter::X_edge = 107.958336  !読み込むJCOPE2データのX方向の最西座標
real, parameter::Y_edge = 10.4166660  !読み込むJCOPE2データのY方向の最南座標
real IDSY    !水平方向の単位格子幅→変数化する
real IDSX(NPCL)
real U(li, lj, ll), V(li, lj, ll), W(li, lj, ll)
real SAL(li, lj, ll), TEMP(li, lj, ll)
real DD(ll)
real UF(li, lj + 1, ll), VF(li, lj + 1, ll), WF(li, lj, ll)
real HH(li, lj, ll), H(li, lj), NX(li, lj), NY(li, lj), uk, vk, wk, PDIFH, PDIFV
real PCLX(NPCL), PCLY(NPCL), PCLZ(NPCL), SEIKIB, IID(NPCL), META_SEIKIB(NPCL), GROWTH_SEIKIB(NPCL), TL_SEIKIB(NPCL)
real RPCLX(NPCL), RPCLY(NPCL)
real WSAL, ESAL, SSAL, NSAL, XSAL, YSAL, XSWIMS, YSWIMS
real SWIMS,start_lon,start_lat,goal_lon,goal_lat, magnet(li,lj)
real randomno, randomnos
integer clock, clocks, release_location, imagnet, jmagnet
integer old, new, magmode
integer JJ, IPDUMP, NDAY
!ディレクトリのパスの長さ(適宜変更)
character file * 8, start(122) * 8, file1 * 8
character dateyear(500)*8
character ANG_name * 3, ANG_name2 * 3, dir_speed * 14, method_name * 5, angle_name *3, method_number * 1, speed_name *2
integer year, month, i_start, dayset
real SPD, i_spd
real OTODAY(NPCL), DAY, DAY_h
real SWIMS_MIN, SWIMS_MAX
integer year_start, yeat_end, month_start, month_end, ANG_SET, daycount, method, Angle, Speed


year = 2018
month = 1
dayset = 1
! Speed = 0
! Angle = 0
release_location=1 !1=台湾沖; 2=宮崎県沖; 3=利根川河口沖; 4=青森県小川原湖; 5=4地点(台湾;宮崎;利根川;青森県小川原湖)
method=1!1=Ideal; 2=Random; 3=Compass; 4=Total intensity: 5=Inclination; 6=Declination

!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
!=!=!=!=!=!=!=!=!ファイル名の設定!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
!    if(Angle.lt.-9) then
!         write(angle_name,'(I3)') Angle
!      if(Speed.lt.10) then
!         write(dir_Speed,'(A1,I3,A4,I1,A1,I4)') 'D',Angle,'_S00',Speed,'_',year
!      else if(Speed.lt.100) then
!         write(dir_speed,'(A1,I3,A3,I2,A1,I4)') 'D',Angle,'_S0',Speed,'_',year
!      else
!         write(dir_speed,'(A1,I3,A2,I3,A1,I4)') 'D',Angle,'_S',Speed,'_',year
!      end if
!    else if(Angle.lt.0) then
!         Angle=Angle*(-1)
!         write(angle_name,'(A2,I1)') '-0',Angle
!      if(Speed.lt.10) then
!         write(dir_speed,'(A3,I1,A4,I1,A1,I4)') 'D-0',Angle,'_S00',Speed,'_',year
!      else if(Speed.lt.100) then
!         write(dir_speed,'(A3,I1,A3,I2,A1,I4)') 'D-0',Angle,'_S0',Speed,'_',year
!      else
!         write(dir_speed,'(A3,I1,A2,I3,A1,I4)') 'D-0',Angle,'_S',Speed,'_',year
!      end if
!    else if(Angle.lt.10) then
!         write(angle_name,'(A2,I1)') '+0',Angle
!      if(Speed.lt.10) then
!         write(dir_speed,'(A3,I1,A4,I1,A1,I4)') 'D+0',Angle,'_S00',Speed,'_',year
!      else if(Speed.lt.100) then
!         write(dir_speed,'(A3,I1,A3,I2,A1,I4)') 'D+0',Angle,'_S0',Speed,'_',year
!      else
!         write(dir_speed,'(A3,I1,A2,I3,A1,I4)') 'D+0',Angle,'_S',Speed,'_',year
!      end if
!    else
!         write(angle_name,'(A1,I2)') '+',Angle
!      if(Speed.lt.10) then
!         write(dir_speed,'(A2,I2,A4,I1,A1,I4)') 'D+',Angle,'_S00',Speed,'_',year
!      else if(Speed.lt.100) then
!         write(dir_speed,'(A2,I2,A3,I2,A1,I4)') 'D+',Angle,'_S0',Speed,'_',year
!      else
!         write(dir_speed,'(A2,I2,A2,I3,A1,I4)') 'D+',Angle,'_S',Speed,'_',year
!      end if
!    end if
! !=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!

! !=!=!=!=!=!=!=!=!Speed名の設定!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
!      if(Speed.lt.10) then
!         write(speed_name,'(A1,I1)') '0',Speed
!      else if(Speed.lt.100) then
!         write(speed_name,'(I2)') Speed
!      end if
!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!

!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
!=!=!=!=!=!=!Orientation methodを設定=!=!=!=!=!=!=!
!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
    if (method == 1) then
      method_name = 'Ideal'
      method_number = '1'
    elseif (method == 2) then
      method_name = 'Rando'
      method_number = '2'
    elseif (method == 3) then
      method_name = 'Compa'
      method_number = '3'
    elseif (method == 4) then
      method_name = 'Total'
      method_number = '4'
    elseif (method == 5) then
      method_name = 'Incli'
      method_number = '5'
    elseif (method == 6) then
      method_name = 'Decli'
      method_number = '6'
    end if
!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!

!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
!=!=!=!=!=!=計算を開始する位置を設定=!=!=!=!=!=!
!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
    if (release_location == 1) start = 'release1'
    if (release_location == 2) start = 'Miyazaki'
    if (release_location == 3) start = 'Tonegawa'
    if (release_location == 4) start = 'Aomori00'
    if (release_location == 5) start = '4station'
!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!

!計算を開始する期間を'mm.yyyy'で記述(適宜変更)
  if(month.lt.10) then
    if(dayset.lt.10) then
      write(file,'(I4,A1,I1,A1,I1)') year,'0',month,'0',dayset
    else
      write(file,'(I4,A1,I1,I2)') year,'0',month,dayset
    end if
  else
    if(dayset.lt.10) then
      write(file,'(I4,I2,A1,I1)') year,month,'0',dayset
    else
      write(file,'(I4,I2,I2)') year,month,dayset
    end if
  end if
                                    
!	i_start = 3*year+month-((3*year_start+month_start)-1)         !最初の放流year+monthを代入して"i_start"が「1」になるように設定する!!!!

!	write(*, *) 
!	write(*, '(I4,A1,I2,A1,I2,A24,A6,I6,A6)') year, '/', month, '/', dayset, start, 'に粒子', NPCL, '個投入'

	!読み込む(readする)ファイルを開く(open)
	!装置番号7→MIROCのx, y, zの格子数などを記述(変更不要)
	open(7, file = '/Volumes/erika/CMEMS/set_input/grid_cmems.dat')
	!装置番号8→各格子点の水深を記述(変更不要)
	open(8, file = '/Volumes/erika/CMEMS/set_input/dini_cmems.dat')
	!装置番号9→投入位置を記述.　0，経度，緯度，水深(自分で作成)
	open(9, file = '/Volumes/erika/CMEMS/set_input/start-point/'//start//'.dat')
		!装置番号10→計算日数，拡散を与える頻度，水平，鉛直拡散係数(場合によって変更)
		open(10, file = '/Volumes/erika/CMEMS/set_input/eel.dat', FORM = 'FORMATTED')
		!装置番号11→出力されるファイル
		open(11, file = '/Volumes/erika/CMEMS/output/test.dat',FORM='FORMATTED',STATUS='unknown')
			!装置番号16番→ ? ? ? (変更不要)
			open(16, file = '/Volumes/erika/CMEMS/set_input/nxny_cmems.dat')


			!読み込み開始
			read(7, *) IN, JN, LN 
			read(7, *) (DD(l), l = 1, ll) !各水層の水深
			read(8, *) ((H(i, j), i = 1, li), j = 1, lj) !実際水深
			read(8, *) (((HH(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)!格子深度
			read(16, *) ((NX(i, j), i = 1, li), j = 1, lj) !MIROCデータの最大水深
			read(16, *) ((NY(i, j), i = 1, li), j = 1, lj)
			close(7)
			close(8)
			close(16)
			
			
			read(10, *) NDAY, IPDUMP, PDIFH, PDIFV !PDIFH, PDIFV ?
			DDT=IPDUMP*3600          !計算一回の秒数
			NSSS=NDAY*24*3600        !計算期間の秒数
			MAXKAI=INT(NSSS/DDT)     !計算回数
			WRITE(*,*) 'MAXKAI',MAXKAI
			close(10)

			open(17, file = '/Volumes/erika/CMEMS/set_input/strat_date/'//file//'.txt')
			NI=0

			do NKAI = 1, MAXKAI
			if (MOD(NKAI*INT(DDT), 1*24*3600) == 0) then
			    NI = NI + 1
				read(17, '(A8)') dateyear(NI)       !read(17, '(A8)', END = 99) dateyear(NI)
				
				!Check point
				write(*, *)' '//dateyear(NI)//'粒子運動計算中…'
!				ディレクトリのパスを変数で与える(適宜変更)
				  open(18,file = '/Volumes/erika/CMEMS/prosessed_data/U/U_'//dateyear(NI)//'',&
                       FORM='UNFORMATTED',status="unknown",access='direct',recl=4*li*(lj+1))
                  open(19,file = '/Volumes/erika/CMEMS/prosessed_data/V/V_'//dateyear(NI)//'',&
                       FORM='UNFORMATTED',status="unknown",access='direct',recl=4*li*(lj+1))
                 open(20,file = '/Volumes/Higuchi_JCOPE/Prosessed_data/W/W_'//dateyear(NI)//'',&
                     FORM='UNFORMATTED',status="unknown",access='direct',recl=4*li*(lj+1))

!                装置番号21, 22番→OFESのsal, tempのファイルを開く
                 !open(21,file = '/Volumes/Higuchi_Tracking/tracking/salinity/miroc_s.'//dateyear(NI)//'.dta',&
                 !     FORM='UNFORMATTED',status="unknown",access='direct',recl=4*li*(lj+1))
                  ! open(22,file = '/Volumes/Higuchi_JCOPE/Prosessed_data/T/T_'//dateyear(NI)//'',&
                  !      FORM='UNFORMATTED',status="unknown",access='direct',recl=4*li*(lj+1))
99                continue
                    
!				  input u, v, w, sal, temp - data
				   irec1=1
				   irec2=1
				  !irec3=1
				  !irec4=1
				   ! irec5=1
				   ! irec6=1
				   
                   do k=1,ll             !UVTS grid
					  read(18,rec=irec1)((u(i,j,k),i=1,li),j=1,lj)
                      irec1=irec1+1
                      read(19,rec=irec2)((v(i,j,k),i=1,li),j=1,lj)
                      irec2=irec2+1
                      read(20,rec=irec3)((w(i,j,k),i=1,li),j=1,lj+1)
                      !irec3=irec3+1
                      !read(21,rec=irec4)((sal(i,j,k),i=1,li),j=1,lj+1)
                      !irec4=irec4+1
                      ! read(22,rec=irec5)((temp(i,j,k),i=1,li),j=1,lj)
                      ! irec5=irec5+1
                      if(k==ll) exit
                   end do
                   

				   close(18)
				   close(19)
				   close(20)
				   !close(21)
				   ! close(22)

               !Geomagnetic field reading
                  IF(method==4) open(23,file = '/Volumes/Higuchi_JCOPE/Prosessed_data/M_intensity/M_'//dateyear(NI)//'',FORM='UNFORMATTED',status="unknown")
                  IF(method==5) open(23,file = '/Volumes/Higuchi_JCOPE/Prosessed_data/M_inclination/M_'//dateyear(NI)//'',FORM='UNFORMATTED',status="unknown")
                  IF(method==6) open(23,file = '/Volumes/Higuchi_JCOPE/Prosessed_data/M_declination/M_'//dateyear(NI)//'',FORM='UNFORMATTED',status="unknown")
                     do j=1,lj
                      do i=1,li
                          read(23) magnet(i,j)
                       end do
                     end do
				  close(23)

!				   call intp_u(uf, u, uk, li, lj, ll)     !input_u(v,w)は海流のデータをcm/secからm/secに変換するサブルーチンである。
!				   call intp_v(vf, v, vk, li, lj, ll)     !JCOPE2は生データがm/secであるため、このサブルーチンは不要である。
!				   call intp_w(wf, w, wk, li, lj, ll)     !MIROCは生データはcm/secであるため、このサブルーチンが必要である。

!                  Check point
!				   write(*, *)'   CheckPoint '//veldatadir//'miroc_u.'//dateyear(NI)//'.dta'
!				   write(*, *)'  ', u(101, 145, 5), v(101, 145, 5), w(101, 145, 5)
				endif

              if (NKAI==1) then
				   do i = 1, NPCL
					  read(9, *) IID(i), RPCLX(i), RPCLY(i), PCLZ(i)
					  IDSX(i)=(111.111*1000*cos(RPCLY(i)/360.*2*pi))/12.
					  IDSY=111.111*1000/12.
					  PCLX(i)=(RPCLX(i)-X_edge)*IDSX(i)*12.
					  PCLY(i)=(RPCLY(i)-Y_edge)*IDSY*12.
				   end do
					
					close(9)
					
				 else
				    do i = 1, NPCL
					   IDSX(i) = (111.111*1000*cos(RPCLY(i)/360.*2*pi))/12.
					   IDSY = 111.111*1000/12.
					   RPCLX(i)=(PCLX(I)/(IDSX(i)*12.))+X_edge
					   RPCLY(i)=(PCLY(I)/(IDSY*12.))+Y_edge
					end do
				 endif
				 call PCL(H,HH,U,V,W,DD,PDIFH,PCLX,PCLY,PCLZ,SEIKB,li,lj,ll,IDSX,IDSY,NX,NY,NPCL,DDT,NKAI,SAL,TEMP,SPD,OTODAY,DAY,META_SEIKIB,GROWTH_SEIKIB,TL_SEIKIB,&
				          Speed,Angle,magnet,X_edge,Y_edge)
			  end do

					  close(17)

			   !end do
			   !end do
!			   WRITE(*,*) 'END'
			 end program main

                                
								
!****************************************************************
SUBROUTINE PCL(H,HH,U,V,W,DD,PDIFH,PCLX,PCLY,PCLZ,SEIKB,li,lj,ll,IDSX,IDSY,NX,NY,NPCL,DDT,NKAI,SAL,TEMP,SPD,OTODAY,DAY,META_SEIKIB,GROWTH_SEIKIB,TL_SEIKIB,Speed,Angle,magnet,X_edge,Y_edge)
!****************************************************************
!如果上面添加了其他函數 這邊也要記得加上定義
!from main program
	  real HH(li,lj,ll),U(li,lj,ll),V(li,lj,ll)
	  real IDSX(NPCL),IDSY
	  real W(li,lj,ll),H(li,lj),DD(ll),NX(li,lj),NY(li,lj),PDIFH
	  real SAL(li,lj,ll),TEMP(li,lj,ll), magnet(li,lj)
	  real PCLX(NPCL),PCLY(NPCL),PCLZ(NPCL),SEIKIB,DDT,AS
	  real META_SEIKIB(NPCL),GROWTH_SEIKIB(NPCL),TL_SEIKIB(NPCL)
	  real TL
	  real WSAL,ESAL,SSAL,NSAL,XSAL,YSAL,XSWIMS,YSWIMS, SWIMS
	  real SPD,OTODAY(NPCL),DAY, ANG, ANG_MIN, ANG_MAX
	  real SALC,SALE,SALW,SALN,SALS,SALNE,SALNW,SALSE,SALSW
	  real DEG,X_ANG,Y_ANG,start_lon,start_lat,goal_lon,goal_lat
	  real Idealno, randomnos, MAG_N, MAG_NE, MAG_E, MAG_SE, MAG_S, MAG_SW, MAG_W, MAG_NW, MAG_C
	  real H_N, H_NE, H_E, H_SE, H_S, H_SW, H_W, H_NW, EH, WH, NH, SH, XH, YH, XH_comp, YH_comp, DEEPER0, DEEPER
	  real EMAG,WMAG,NMAG,SMAG,XMAG,YMAG,XSEIBUN,YSEIBUN,ANG_SET,DEG0!,Angle
      integer clock, clocks, imagnet, jmagnet
	  integer NPCL,NDAY,NKAI,NLAYER,SALL
	  integer NI, year_sub, month_sub, dayset_sub, date_interval_sub
      character dateyear(500)*8, date_interval_sub_name *2
      character file_sub * 8
      integer Angle, Speed


!to main program
!*********************************************************************************************
!****************************** SEIKIB OF METAMORPHOSIS & GROWTH ******************************
!各粒子の変態開始時間を正規分布に設定する(AVE&SDは篠田D論に参照しました、適宜変更)
!各粒子の成長速度は(体長 - 3.6) / 変態までの日齢により算出する(AVE&SDは篠田D論に参照しました、適宜変更)
!meta為變態時間 tlseikib為最大體長 growth為成長速率
!       IF(NKAI == 1) THEN
!check point
!print*, SEIKIB(IJ)!
!	   DO n=1,NPCL
!		  META_SEIKIB(n)=SEIKIB(IJ)*19.6+0
!		  TL_SEIKIB(n)=447.0
!		  GROWTH_SEIKIB(n)=(TL_SEIKIB(n)-3.6)/META_SEIKIB(n)
!	   write(30,'(1X,F5.1,1X,F7.2,1X,F5.1)')META_SEIKIB(n),GROWTH_SEIKIB(n),TL_SEIKIB(n)
!print*, META_SEIKIB(n), GROWTH_SEIKIB(n), TL_SEIKIB(n)
!	   end do
!	   close(30)

!	   ENDIF


!  ***  SliP(SL = 1.0) OR NO - SliP(SL = -1.0) ? *******
!     SL = -1.0　　　 !境界条件、反射する？
!     SL = 0.0　　　　!境界条件、停止？
	   SL=1.0       !境界条件、そのまま移動する？
	   EPSH=1.0
!  ***  SliP CONDITION AT BOTTOM  *****************
!     BTMSliP = -1.0  !海底に着くと輸送STOP
	   BTMSliP=1.0  !海底に着いても底に沿って輸送する
	   EPSB=0.01
!  ***  PREFECT REFRECTION OR NOT ? **************
	  REF=1.0
!     REF = -1.0
!*** CHANGE OF PCLZ ******************************垂直迴游的設定

	 if (MOD(NKAI*INT(DDT),24*3600).GT.12*3600) THEN
        do I=1,NPCL
           LAYER0=1.                                                                                                                                      !            |
          do LAYER=2.,28.,1.                                                                                                                              !            |
           IF (abs(TEMP(INT(PCLX(I)/IDSX(i))+1,INT(PCLY(I)/IDSY)+1,LAYER0)-5.0) > abs(TEMP(INT(PCLX(I)/IDSX(i))+1,INT(PCLY(I)/IDSY)+1,LAYER)-5.0)) then   !            |
            LAYER0=LAYER                                                                                                                                  !            |
           END IF                                                                                                                                         !            |
          end do                                                                                                                                          !            |
            IF (LAYER0==1) PCLZ(I)=0.                                                                                                                     !            |
            IF (LAYER0==2) PCLZ(I)=5.                                                                                                                     !            |
            IF (LAYER0==3) PCLZ(I)=10.                                                                                                                    !        昼間の水深
            IF (LAYER0==4) PCLZ(I)=50.                                                                                                                    !     水温5°Cに最も近い
            IF (LAYER0==5) PCLZ(I)=75.                                                                                                                    !        水深を選択
            IF (LAYER0==6) PCLZ(I)=100.                                                                                                                   !            |
            IF (LAYER0==7) PCLZ(I)=150.                                                                                                                   !            |
            IF (LAYER0==8) PCLZ(I)=200.                                                                                                                   !            |
            IF (LAYER0==9) PCLZ(I)=250.                                                                                                                   !            |
            IF (LAYER0==10) PCLZ(I)=300.                                                                                                                  !            |
            IF (LAYER0==11) PCLZ(I)=400.                                                                                                                  !            |
            IF (LAYER0==12) PCLZ(I)=500.                                                                                                                  !            |
            IF (LAYER0==13) PCLZ(I)=600.                                                                                                                  !            |
            IF (LAYER0==14) PCLZ(I)=700.                                                                                                                  !            |
            IF (LAYER0==15) PCLZ(I)=800.                                                                                                                  !            |
            IF (LAYER0==16) PCLZ(I)=900.                                                                                                                  !            |
            IF (LAYER0==17) PCLZ(I)=1000.                                                                                                                 !            |
            IF (LAYER0==18) PCLZ(I)=1200.                                                                                                                 !            |
            IF (LAYER0==19) PCLZ(I)=1400.                                                                                                                 !            |
            IF (LAYER0==20) PCLZ(I)=1600.                                                                                                                 !            |
            IF (LAYER0==21) PCLZ(I)=1800.                                                                                                                 !            |
            IF (LAYER0==22) PCLZ(I)=2000.                                                                                                                 !            |
            IF (LAYER0==23) PCLZ(I)=2500.                                                                                                                 !            |
            IF (LAYER0==24) PCLZ(I)=3000.                                                                                                                 !            |
            IF (LAYER0==25) PCLZ(I)=3500.                                                                                                                 !            |
            IF (LAYER0==26) PCLZ(I)=4000.                                                                                                                 !            |
            IF (LAYER0==27) PCLZ(I)=5000.                                                                                                                 !            |
            IF (LAYER0==28) PCLZ(I)=6000.                                                                                                                 !            |
        end do                                                                                                                                             !!!!!!!!!!!!!!!!!!!!!!!!!
    else
        do I=1,NPCL
          PCLZ(I)=200                                                                                                                                      !!!!!!!!!!!!!!!!!!!!!!!!!
        end do                                                                                                                                             !!!!!!!!!!!!!!!!!!!!!!!!!
	endif

	do I=1,NPCL

!       call IDS_henko(IDSX, IDSY, PCLX, PCLY, NPCL)
	     ITRA=0
101      ITRA=ITRA+1
		 III=INT(PCLX(I)/IDSX(i))+1
!		 WRITE(*,*) 'IDSX(i)=',IDSX(i)
!		 WRITE(*,*) 'PCLX(I)=',PCLX(I)
!		 WRITE(*,*) 'III=',III
		 
!       print*, pclx(I)
		 JJJ=INT(PCLY(I)/IDSY)+1
		 IF((H(III,JJJ).LE.PCLZ(I)).AND.(BTMSLIP.LT.0.0)) GOTO 300  !海底に着くと輸送STOP
		 IF((H(III,JJJ).LE.PCLZ(I)).AND.(BTMSLIP.GT.0.0)) PCLZ(I)=H(III,JJJ)-EPSB  !海底に沿って輸送する(現在の設定)
		 IF(H(III,JJJ).LE.0.0) GOTO 300  !陸に着いたらSTOP
		!IF(H(III,JJJ).LE.10.0) GOTO 300  !陸に着いたらSTOP
         IF(III.GE.866.0 .OR. III.LE.1.0 .OR. JJJ.GE.620.0 .OR. JJJ.LE.1.0) GOTO 300  !境界に着くと輸送STOP
         do 200 K=1,ll
			IF(DD(K).GT.PCLZ(I)) GOTO 250 
200         CONTINUE
250         LLL=K
            IF(HH(III,JJJ,LLL).LE.0.D0) GOTO 300
!         DX, DYは粒子の格子内水平位置を示した
			DX=MOD((PCLX(i)),IDSX(i))/IDSX(i)

			DY=MOD(REAL(PCLY(I)),IDSY)/IDSY
			IF(LLL.EQ.1) THEN
!       write(*, *) III, JJJ, PCLZ(I), I
			   DZ=PCLZ(I)/HH(III,JJJ,1)
			ELSE
			   IF(HH(III, JJJ, LLL).LT.1) &
!                    write(*, *) III, JJJ, LLL, PCLZ(I), DD(LLL - 1), HH(III, JJJ, LLL)
			   DZ=(PCLZ(I)-DD(LLL-1))/HH(III,JJJ,LLL)
			END IF
!        write(*, *)'44'
!        粒子がいる格子と周辺格子の各方向における移動速度を計算 ?
		    UC=U(III,JJJ,LLL)
            UE=U(III+1,JJJ,LLL)
            UN=U(III,JJJ+1,LLL)
            UNE=U(III+1,JJJ+1,LLL)
            US=U(III,JJJ-1,LLL)
            UUSE=U(III+1,JJJ-1,LLL)
            UCL=U(III,JJJ,LLL+1)
            UEL=U(III+1,JJJ,LLL+1)
            UNL=U(III,JJJ+1,LLL+1)
            UNEL=U(III+1,JJJ+1,LLL+1)
            USL=U(III,JJJ-1,LLL+1)
            USEL=U(III+1,JJJ-1,LLL+1)
            VC=V(III,JJJ,LLL)
            VE=V(III+1,JJJ,LLL)
            VNE=V(III+1,JJJ+1,LLL)
            VN=V(III,JJJ+1,LLL)
            VW=V(III-1,JJJ,LLL)
            VNW=V(III-1,JJJ+1,LLL)
            VCL=V(III,JJJ,LLL+1)
            VEL=V(III+1,JJJ,LLL+1)
            VNEL=V(III+1,JJJ+1,LLL+1)
            VNL=V(III,JJJ+1,LLL+1)
            VWL=V(III-1,JJJ,LLL+1)
            VNWL=V(III-1,JJJ+1,LLL+1)
            WC=W(III,JJJ,LLL)
            WE=W(III+1,JJJ,LLL)
            WW=W(III-1,JJJ,LLL)
            WS=W(III,JJJ-1,LLL)
            WN=W(III,JJJ+1,LLL)
            WNE=W(III+1,JJJ+1,LLL)
            WNW=W(III-1,JJJ+1,LLL)
            WSE=W(III+1,JJJ-1,LLL)
            WSW=W(III-1,JJJ-1,LLL)
			IF(LLL.GT.1) THEN
			UCU=U(III,JJJ,LLL-1)
            UEU=U(III+1,JJJ,LLL-1)
            UNU=U(III,JJJ+1,LLL-1)
            UNEU=U(III+1,JJJ+1,LLL-1)
            USU=U(III,JJJ-1,LLL-1)
            USEU=U(III+1,JJJ-1,LLL-1)
            VCU=V(III,JJJ,LLL-1)
            VEU=V(III+1,JJJ,LLL-1)
            VNEU=V(III+1,JJJ+1,LLL-1)
            VNU=V(III,JJJ+1,LLL-1)
            VWU=V(III-1,JJJ,LLL-1)
            VNWU=V(III-1,JJJ+1,LLL-1)
            WCU=W(III,JJJ,LLL-1)
            WEU=W(III+1,JJJ,LLL-1)
            WWU=W(III-1,JJJ,LLL-1)
            WSU=W(III,JJJ-1,LLL-1)
            WNU=W(III,JJJ+1,LLL-1)
            WNEU=W(III+1,JJJ+1,LLL-1)
            WNWU=W(III-1,JJJ+1,LLL-1)
            WSEU=W(III+1,JJJ-1,LLL-1)
            WSWU=W(III-1,JJJ-1,LLL-1)
		 ELSE
            WCU=0.0
            WEU=0.0
            WWU=0.0
            WSU=0.0
            WNU=0.0
            WNEU=0.0
            WNWU=0.0
            WSEU=0.0
            WSWU=0.0
         END IF													
																

!***********************************************************************
!u_component_velocity
!***********************************************************************

            IF(DY.GE.0.5) THEN
               IF((NY(III,JJJ+1).LT.LLL).OR.(NY(III+1,JJJ+1).LT.LLL))&
                    UNE=SL*U(III+1,JJJ,LLL)
               IF((NY(III,JJJ+1).LT.LLL).OR.(NY(III-1,JJJ+1).LT.LLL))&
                    UN=SL*U(III,JJJ,LLL)
               IF(DZ.LT.0.5) THEN
                  IF(LLL.EQ.1) THEN
                     PCLU=DX*(DY-0.5)*UNE+DX*(1.5-DY)*UE+&
                          (1.0-DX)*(DY-0.5)*UN+(1.0-DX)*(1.5-DY)*UC
                     DUDX=((DY-0.5)*(UNE-UN)+(1.5-DY)*(UE-UC))/IDSX(i)
                     DUDZ=0.0
                     IF(NX(III,JJJ).LE.0.0) UC=SL*U(III,JJJ+1,LLL)
                     IF(NX(III,JJJ+1).LE.0.0) UN=SL*U(III,JJJ,LLL)
                     IF(NX(III+1,JJJ).LE.0.0) UE=SL*U(III+1,JJJ+1,LLL)
                     IF(NX(III+1,JJJ+1).LE.0.0) UNE=SL*U(III+1,JJJ,LLL)
                     DUDY=(DX*(UNE-UE)+(1.0-DX)*(UN-UC))/IDSX(i)


				  ELSE!LLL

                     IF((NY(III,JJJ+1).LT.LLL-1).OR.(NY(III+1,JJJ+1).LT.LLL-1))&
                          UNEU=SL*U(III+1,JJJ,LLL-1)
                     IF((NY(III,JJJ+1).LT.LLL-1).OR.(NY(III-1,JJJ+1).LT.LLL-1))&
                          UNU=SL*U(III,JJJ,LLL-1)
                     PCLU=(DX*(DY-0.5)*UNE+DX*(1.5-DY)*UE+&
                          (1.0-DX)*(DY-0.5)*UN+(1.0-DX)*(1.5-DY)*UC)*(0.5+DZ)+&
                          (DX*(DY-0.5)*UNEU+DX*(1.5-DY)*UEU+&
                          (1.0-DX)*(DY-0.5)*UNU+(1.0-DX)*(1.5-DY)*UCU)*(0.5-DZ)
                     DUDX=(((DY-0.5)*(UNE-UN)+(1.5-DY)*(UE-UC))*(0.5+DZ)+&
                          ((DY-0.5)*(UNEU-UNU)+(1.5-DY)*(UEU-UCU))*(0.5-DZ))/IDSX(i)
                     IF(NX(III,JJJ).LT.LLL-1) UCU=SL*U(III,JJJ+1,LLL-1)
                     IF(NX(III,JJJ).LE.LLL-1) UC=SL*U(III,JJJ+1,LLL)
                     IF(NX(III,JJJ+1).LT.LLL-1) UNU=SL*U(III,JJJ,LLL-1)
                     IF(NX(III,JJJ+1).LE.LLL-1) UN=SL*U(III,JJJ,LLL)
                     IF(NX(III+1,JJJ).LT.LLL-1) UEU=SL*U(III+1,JJJ+1,LLL-1)
                     IF(NX(III+1,JJJ).LE.LLL-1) UE=SL*U(III+1,JJJ+1,LLL)
                     IF(NX(III+1,JJJ+1).LT.LLL-1) UNEU=SL*U(III+1,JJJ,LLL-1)
                     IF(NX(III+1,JJJ+1).LE.LLL-1) UNE=SL*U(III+1,JJJ,LLL)
                     DUDY=((DX*(UNE-UE)+(1.0-DX)*(UN-UC))*(0.5+DZ)+&
                          (DX*(UNEU-UEU)+(1.0-DX)*(UNU-UCU))*(0.5-DZ))/IDSX(i)
                     IF(NX(III,JJJ).LT.LLL-1) UC=0.0
                     IF(NX(III,JJJ).LT.LLL-1) UCU=0.0
                     IF(NX(III,JJJ+1).LT.LLL-1) UN=0.0
                     IF(NX(III,JJJ+1).LT.LLL-1) UNU=0.0
                     IF(NX(III+1,JJJ).LT.LLL-1) UE=0.0
                     IF(NX(III+1,JJJ).LT.LLL-1) UEU=0.0
                     IF(NX(III+1,JJJ+1).LT.LLL-1) UNE=0.0
                     IF(NX(III+1,JJJ+1).LT.LLL-1) UNEU=0.0
                     IF(NX(III,JJJ).EQ.LLL-1) UC=SL*U(III,JJJ,LLL-1)
                     IF(NX(III,JJJ+1).EQ.LLL-1) UN=SL*U(III,JJJ+1,LLL-1)
                     IF(NX(III+1,JJJ).EQ.LLL-1) UE=SL*U(III+1,JJJ,LLL-1)
                     IF(NX(III+1,JJJ+1).EQ.LLL-1) UNE=SL*U(III+1,JJJ+1,LLL-1)
                     DUDZ=2*(DX*(DY-0.5)*(UNEU-UNE)+DX*(1.5-DY)*(UEU-UE)+&
                          (1.0-DX)*(1.5-DY)*(UCU-UC)+(1.0-DX)*(DY-0.5)*(UNU-UN))&
                          /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL-1))
                  END IF
               ELSE!DZ

                  IF(LLL.EQ.NX(III,JJJ)) UCL=SL*U(III,JJJ,LLL)
                  IF(LLL.EQ.NX(III+1,JJJ)) UEL=SL*U(III+1,JJJ,LLL)
                  IF(LLL.EQ.NX(III,JJJ+1)) UNL=SL*U(III,JJJ+1,LLL)
                  IF(LLL.EQ.NX(III+1,JJJ+1)) UNEL=SL*U(III+1,JJJ+1,LLL)
                  IF((NY(III,JJJ+1).LT.LLL+1).OR.(NY(III+1,JJJ+1).LT.LLL+1))&
                       UNEL=SL*U(III+1,JJJ,LLL+1)
                  IF((NY(III,JJJ+1).LT.LLL+1).OR.(NY(III-1,JJJ+1).LT.LLL+1))&
                       UNL=SL*U(III,JJJ,LLL+1)
                  PCLU=(DX*(DY-0.5)*UNE+DX*(1.5-DY)*UE+&
                       (1.0-DX)*(DY-0.5)*UN+(1.0-DX)*(1.5-DY)*UC)*(1.5-DZ)+&
                       (DX*(DY-0.5)*UNEL+DX*(1.5-DY)*UEL+&
                       (1.0-DX)*(DY-0.5)*UNL+(1.0-DX)*(1.5-DY)*UCL)*(DZ-0.5)
                  DUDX=(((DY-0.5)*(UNE-UN)+(1.5-DY)*(UE-UC))*(1.5-DZ)+&
                       ((DY-0.5)*(UNEL-UNL)+(1.5-DY)*(UEL-UCL))*(DZ-0.5))/IDSX(i)
                  IF(NX(III,JJJ).LT.LLL) UC=SL*U(III,JJJ+1,LLL)
                  IF(NX(III,JJJ+1).LT.LLL) UN=SL*U(III,JJJ,LLL)
                  IF(NX(III+1,JJJ).LT.LLL) UE=SL*U(III+1,JJJ+1,LLL)
                  IF(NX(III+1,JJJ+1).LT.LLL) UNE=SL*U(III+1,JJJ,LLL)
                  IF(NX(III,JJJ).LE.LLL) UCL=SL*U(III,JJJ+1,LLL+1)
                  IF(NX(III,JJJ+1).LE.LLL) UNL=SL*U(III,JJJ,LLL+1)
                  IF(NX(III+1,JJJ).LE.LLL) UEL=SL*U(III+1,JJJ+1,LLL+1)
                  IF(NX(III+1,JJJ+1).LE.LLL) UNEL=SL*U(III+1,JJJ,LLL+1)
                  DUDY=((DX*(UNE-UE)+(1.0-DX)*(UN-UC))*(1.5-DZ)+&
                       (DX*(UNEL-UEL)+(1.0-DX)*(UNL-UCL))*(DZ-0.5))/IDSX(i)
                  IF(NX(III,JJJ).LT.LLL) UC=0.0
                  IF(NX(III,JJJ).LT.LLL) UCL=0.0
                  IF(NX(III,JJJ+1).LT.LLL) UN=0.0
                  IF(NX(III,JJJ+1).LT.LLL) UNL=0.0
                  IF(NX(III+1,JJJ).LT.LLL) UE=0.0
                  IF(NX(III+1,JJJ).LT.LLL) UEL=0.0
                  IF(NX(III+1,JJJ+1).LT.LLL) UNE=0.0
                  IF(NX(III+1,JJJ+1).LT.LLL) UNEL=0.0
                  IF(NX(III,JJJ).EQ.LLL) UCL=SL*U(III,JJJ,LLL)
                  IF(NX(III,JJJ+1).EQ.LLL) UNL=SL*U(III,JJJ+1,LLL)
                  IF(NX(III+1,JJJ).EQ.LLL) UEL=SL*U(III+1,JJJ,LLL)
                  IF(NX(III+1,JJJ+1).EQ.LLL) UNEL=SL*U(III+1,JJJ+1,LLL)
                  DUDZ=2*(DX*(DY-0.5)*(UNE-UNEL)+DX*(1.5-DY)*(UE-UEL)+&
                       (1.0-DX)*(1.5-DY)*(UC-UCL)+(1.0-DX)*(DY-0.5)*(UN-UNL))&
                       /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL+1))
               END IF
            ELSE!DY


               IF((NY(III,JJJ).LT.LLL).OR.(NY(III+1,JJJ).LT.LLL))&
                    UUSE=SL*U(III+1,JJJ,LLL)
               IF((NY(III,JJJ).LT.LLL).OR.(NY(III-1,JJJ).LT.LLL))&
                    US=SL*U(III,JJJ,LLL)
               IF(DZ.LT.0.5) THEN
                  IF(LLL.EQ.1) THEN
                     PCLU=DX*(DY+0.5)*UE+DX*(0.5-DY)*UUSE+&
                          (1.0-DX)*(0.5-DY)*US+(1.0-DX)*(DY+0.5)*UC
                     DUDX=((0.5-DY)*(UUSE-US)+(DY+0.5)*(UE-UC))/IDSX(i)
                     DUDZ=0.0
                     IF(NX(III,JJJ).LE.0.0) UC=SL*U(III,JJJ-1,LLL)
                     IF(NX(III,JJJ-1).LE.0.0) US=SL*U(III,JJJ,LLL)
                     IF(NX(III+1,JJJ).LE.0.0) UE=SL*U(III+1,JJJ-1,LLL)
                     IF(NX(III+1,JJJ-1).LE.0.0) UUSE=SL*U(III+1,JJJ,LLL)
                     DUDY=(DX*(UE-UUSE)+(1.0-DX)*(UC-US))/IDSX(i)
                     !
                  ELSE
                     !
                     IF((NY(III,JJJ).LT.LLL-1).OR.(NY(III+1,JJJ).LT.LLL-1))&
                          USEU=SL*U(III+1,JJJ,LLL-1)
                     IF((NY(III,JJJ).LT.LLL-1).OR.(NY(III-1,JJJ).LT.LLL-1))&
                          USU=SL*U(III,JJJ,LLL-1)
                     !
                     PCLU=(DX*(DY+0.5)*UE+DX*(0.5-DY)*UUSE+&
                          (1.0-DX)*(0.5-DY)*US+(1.0-DX)*(DY+0.5)*UC)*(DZ+0.5)+&
                          (DX*(DY+0.5)*UEU+DX*(0.5-DY)*USEU+&
                          (1.0-DX)*(0.5-DY)*USU+(1.0-DX)*(DY+0.5)*UCU)*(0.5-DZ)
                     DUDX=(((0.5-DY)*(UUSE-US)+(DY+0.5)*(UE-UC))*(DZ+0.5)+&
                          ((0.5-DY)*(USEU-USU)+(DY+0.5)*(UEU-UCU))*(0.5-DZ))/IDSX(i)
                     IF(NX(III,JJJ).LE.LLL-1) UC=SL*U(III,JJJ-1,LLL)
                     IF(NX(III,JJJ-1).LE.LLL-1) US=SL*U(III,JJJ,LLL)
                     IF(NX(III+1,JJJ).LE.LLL-1) UE=SL*U(III+1,JJJ-1,LLL)
                     IF(NX(III+1,JJJ-1).LE.LLL-1) UUSE=SL*U(III+1,JJJ,LLL)
                     IF(NX(III,JJJ).LT.LLL-1) UCU=SL*U(III,JJJ-1,LLL-1)
                     IF(NX(III,JJJ-1).LT.LLL-1) USU=SL*U(III,JJJ,LLL-1)
                     IF(NX(III+1,JJJ).LT.LLL-1) UEU=SL*U(III+1,JJJ-1,LLL-1)
                     IF(NX(III+1,JJJ-1).LT.LLL-1) USEU=SL*U(III+1,JJJ,LLL-1)
                     DUDY=((DX*(UE-UUSE)+(1.0-DX)*(UC-US))*(DZ+0.5)+&
                          (DX*(UEU-USEU)+(1.0-DX)*(UCU-USU))*(0.5-DZ))/IDSX(i)
                     IF(NX(III,JJJ).LT.LLL-1) UC=0.0
                     IF(NX(III,JJJ-1).LT.LLL-1) US=0.0
                     IF(NX(III+1,JJJ).LT.LLL-1) UE=0.0
                     IF(NX(III+1,JJJ-1).LT.LLL-1) UUSE=0.0
                     IF(NX(III,JJJ).LT.LLL-1) UCU=0.0
                     IF(NX(III,JJJ-1).LT.LLL-1) USU=0.0
                     IF(NX(III+1,JJJ).LT.LLL-1) UEU=0.0
                     IF(NX(III+1,JJJ-1).LT.LLL-1) USEU=0.0
                     IF(NX(III,JJJ).EQ.LLL-1) UC=SL*U(III,JJJ,LLL-1)
                     IF(NX(III,JJJ-1).EQ.LLL-1) US=SL*U(III,JJJ-1,LLL-1)
                     IF(NX(III+1,JJJ).EQ.LLL-1) UE=SL*U(III+1,JJJ,LLL-1)
                     IF(NX(III+1,JJJ-1).EQ.LLL-1) UUSE=SL*U(III+1,JJJ-1,LLL-1)
                     DUDZ=2*(DX*(DY+0.5)*(UEU-UE)+DX*(0.5-DY)*(USEU-UUSE)+&
                          (1.0-DX)*(0.5-DY)*(USU-US)+(1.0-DX)*(DY+0.5)*(UCU-UC))&
                          /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL-1))
                  END IF
                  !
               ELSE
                  !
                  IF(LLL.EQ.NX(III,JJJ)) UCL=SL*U(III,JJJ,LLL)
                  IF(LLL.EQ.NX(III+1,JJJ)) UEL=SL*U(III+1,JJJ,LLL)
                  IF(LLL.EQ.NX(III,JJJ-1)) USL=SL*U(III,JJJ-1,LLL)
                  IF(LLL.EQ.NX(III+1,JJJ-1)) USEL=SL*U(III+1,JJJ-1,LLL)
                  IF((NY(III,JJJ).LT.LLL+1).OR.(NY(III+1,JJJ).LT.LLL+1))&
                       USEL=SL*U(III+1,JJJ,LLL+1)
                  IF((NY(III,JJJ).LT.LLL+1).OR.(NY(III-1,JJJ).LT.LLL+1))&
                       USL=SL*U(III,JJJ,LLL+1)
                  PCLU=(DX*(DY+0.5)*UE+DX*(0.5-DY)*UUSE+&
                       (1.0-DX)*(0.5-DY)*US+(1.0-DX)*(DY+0.5)*UC)*(1.5-DZ)+&
                       (DX*(DY+0.5)*UEL+DX*(0.5-DY)*USEL+&
                       (1.0-DX)*(0.5-DY)*USL+(1.0-DX)*(DY+0.5)*UCL)*(DZ-0.5)
                  DUDX=(((0.5-DY)*(UUSE-US)+(DY+0.5)*(UE-UC))*(1.5-DZ)+&
                       ((0.5-DY)*(USEL-USL)+(DY+0.5)*(UEL-UCL))*(DZ-0.5))/IDSX(i)
                  IF(NX(III,JJJ).LT.LLL) UC=SL*U(III,JJJ-1,LLL)
                  IF(NX(III,JJJ-1).LT.LLL) US=SL*U(III,JJJ,LLL)
                  IF(NX(III+1,JJJ).LT.LLL) UE=SL*U(III+1,JJJ-1,LLL)
                  IF(NX(III+1,JJJ-1).LT.LLL) UUSE=SL*U(III+1,JJJ,LLL)
                  IF(NX(III,JJJ).LE.LLL) UCL=SL*U(III,JJJ-1,LLL+1)
                  IF(NX(III,JJJ-1).LE.LLL) USL=SL*U(III,JJJ,LLL+1)
                  IF(NX(III+1,JJJ).LE.LLL) UEL=SL*U(III+1,JJJ-1,LLL+1)
                  IF(NX(III+1,JJJ-1).LE.LLL) USEL=SL*U(III+1,JJJ,LLL+1)
                  DUDY=((DX*(UE-UUSE)+(1.0-DX)*(UC-US))*(1.5-DZ)+&
                       (DX*(UEL-USEL)+(1.0-DX)*(UCL-USL))*(DZ-0.5))/IDSX(i)
                  IF(NX(III,JJJ).LT.LLL) UC=0.0
                  IF(NX(III,JJJ-1).LT.LLL) US=0.0
                  IF(NX(III+1,JJJ).LT.LLL) UE=0.0
                  IF(NX(III+1,JJJ-1).LT.LLL) UUSE=0.0
                  IF(NX(III,JJJ).LT.LLL) UCL=0.0
                  IF(NX(III,JJJ-1).LT.LLL) USL=0.0
                  IF(NX(III+1,JJJ).LT.LLL) UEL=0.0
                  IF(NX(III+1,JJJ-1).LT.LLL) USEL=0.0
                  IF(NX(III,JJJ).EQ.LLL) UCL=SL*U(III,JJJ,LLL)
                  IF(NX(III,JJJ-1).EQ.LLL) USL=SL*U(III,JJJ-1,LLL)
                  IF(NX(III+1,JJJ).EQ.LLL) UEL=SL*U(III+1,JJJ,LLL)
                  IF(NX(III+1,JJJ-1).EQ.LLL) USEL=SL*U(III+1,JJJ-1,LLL)
                  DUDZ=2*(DX*(DY+0.5)*(UE-UEL)+DX*(0.5-DY)*(UUSE-USEL)+&
                       (1.0-DX)*(0.5-DY)*(US-USL)+(1.0-DX)*(DY+0.5)*(UC-UCL))&
                       /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL+1))
               END IF
            END IF
            !
!  *****  V-COMPONENT VELOCITY  *****
!
            IF(DX.GE.0.5) THEN
               IF((NX(III+1,JJJ).LT.LLL).OR.(NX(III+1,JJJ-1).LT.LLL))&
                    VE=SL*V(III,JJJ,LLL)
               IF((NX(III+1,JJJ).LT.LLL).OR.(NX(III+1,JJJ+1).LT.LLL))&
                    VNE=SL*V(III,JJJ+1,LLL)
               IF(DZ.LT.0.5) THEN
                  IF(LLL.EQ.1) THEN
                     PCLV=(DX-0.5)*DY*VNE+(DX-0.5)*(1.0-DY)*VE+&
                          (1.5-DX)*(1.0-DY)*VC+(1.5-DX)*DY*VN
                     DVDY=((DX-0.5)*(VNE-VE)+(1.5-DX)*(VN-VC))/IDSY
                     IF(NY(III,JJJ).LE.0.0) VC=SL*V(III+1,JJJ,LLL)
                     IF(NY(III+1,JJJ).LE.0.0) VE=SL*V(III,JJJ,LLL)
                     IF(NY(III,JJJ+1).LE.0.0) VN=SL*V(III+1,JJJ+1,LLL)
                     IF(NY(III+1,JJJ+1).LE.0.0) VNE=SL*V(III,JJJ+1,LLL)
                     DVDX=(DY*(VNE-VN)+(1.0-DY)*(VE-VC))/IDSY
                     DVDZ=0.0
                     
                  ELSE!LLL
                     
                     IF((NX(III+1,JJJ).LT.LLL-1).OR.(NX(III+1,JJJ-1).LT.LLL-1))&
                          VEU=SL*V(III,JJJ,LLL-1)
                     IF((NX(III+1,JJJ).LT.LLL-1).OR.(NX(III+1,JJJ+1).LT.LLL-1))&
                          VNEU=SL*V(III,JJJ+1,LLL-1)
                     DVDY=(((DX-0.5)*(VNE-VE)+(1.5-DX)*(VN-VC))*(DZ+0.5)+&
                          ((DX-0.5)*(VNEU-VEU)+(1.5-DX)*(VNU-VCU))*(0.5-DZ))/IDSY
                     PCLV=((DX-0.5)*DY*VNE+(DX-0.5)*(1.0-DY)*VE+&
                          (1.5-DX)*(1.0-DY)*VC+(1.5-DX)*DY*VN)*(DZ+0.5)+&
                          ((DX-0.5)*DY*VNEU+(DX-0.5)*(1.0-DY)*VEU+&
                          (1.5-DX)*(1.0-DY)*VCU+(1.5-DX)*DY*VNU)*(0.5-DZ)
                     IF(NY(III,JJJ).LT.LLL-1) VCU=SL*V(III+1,JJJ,LLL-1)
                     IF(NY(III+1,JJJ).LT.LLL-1) VEU=SL*V(III,JJJ,LLL-1)
                     IF(NY(III,JJJ+1).LT.LLL-1) VNU=SL*V(III+1,JJJ+1,LLL-1)
                     IF(NY(III+1,JJJ+1).LT.LLL-1) VNEU=SL*V(III,JJJ+1,LLL-1)
                     IF(NY(III,JJJ).LE.LLL-1) VC=SL*V(III+1,JJJ,LLL)
                     IF(NY(III+1,JJJ).LE.LLL-1) VE=SL*V(III,JJJ,LLL)
                     IF(NY(III,JJJ+1).LE.LLL-1) VN=SL*V(III+1,JJJ+1,LLL)
                     IF(NY(III+1,JJJ+1).LE.LLL-1) VNE=SL*V(III,JJJ+1,LLL)
                     DVDX=((DY*(VNE-VN)+(1.0-DY)*(VE-VC))*(DZ+0.5)+&
                          (DY*(VNEU-VNU)+(1.0-DY)*(VEU-VCU))*(0.5-DZ))/IDSY
                     IF(NY(III,JJJ).LT.LLL-1) VCU=0.0
                     IF(NY(III+1,JJJ).LT.LLL-1) VEU=0.0
                     IF(NY(III,JJJ+1).LT.LLL-1) VNU=0.0
                     IF(NY(III+1,JJJ+1).LT.LLL-1) VNEU=0.0
                     IF(NY(III,JJJ).LT.LLL-1) VC=0.0
                     IF(NY(III+1,JJJ).LT.LLL-1) VE=0.0
                     IF(NY(III,JJJ+1).LT.LLL-1) VN=0.0
                     IF(NY(III+1,JJJ+1).LT.LLL-1) VNE=0.0
                     IF(NY(III,JJJ).EQ.LLL-1) VC=SL*V(III,JJJ,LLL-1)
                     IF(NY(III+1,JJJ).EQ.LLL-1) VE=SL*V(III+1,JJJ,LLL-1)
                     IF(NY(III,JJJ+1).EQ.LLL-1) VN=SL*V(III,JJJ+1,LLL-1)
                     IF(NY(III+1,JJJ+1).EQ.LLL-1) VNE=SL*V(III+1,JJJ+1,LLL-1)
                     DVDZ=2*((DX-0.5)*DY*(VNEU-VNE)+(DX-0.5)*(1.0-DY)*(VEU-VE)+&
                          (1.5-DX)*(1.0-DY)*(VCU-VC)+(1.5-DX)*DY*(VNU-VN))&
                          /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL-1))
                  END IF
                  
               ELSE!DZ
                  
                  IF(LLL.EQ.NY(III,JJJ)) VCL=SL*V(III,JJJ,LLL)
                  IF(LLL.EQ.NY(III+1,JJJ)) VEL=SL*V(III+1,JJJ,LLL)
                  IF(LLL.EQ.NY(III,JJJ+1)) VNL=SL*V(III,JJJ+1,LLL)
                  IF(LLL.EQ.NY(III+1,JJJ+1)) VNEL=SL*V(III+1,JJJ+1,LLL)
                  IF((NX(III+1,JJJ).LT.LLL+1).OR.(NX(III+1,JJJ-1).LT.LLL+1))&
                       VEL=SL*V(III,JJJ,LLL+1)
                  IF((NX(III+1,JJJ).LT.LLL+1).OR.(NX(III+1,JJJ+1).LT.LLL+1))&
                       VNEL=SL*V(III,JJJ+1,LLL+1)
                  PCLV=((DX-0.5)*DY*VNE+(DX-0.5)*(1.0-DY)*VE+&
                       (1.5-DX)*(1.0-DY)*VC+(1.5-DX)*DY*VN)*(1.5-DZ)+&
                       ((DX-0.5)*DY*VNEL+(DX-0.5)*(1.0-DY)*VEL+&
                       (1.5-DX)*(1.0-DY)*VCL+(1.5-DX)*DY*VNL)*(DZ-0.5)
                  DVDY=(((DX-0.5)*(VNE-VE)+(1.5-DX)*(VN-VC))*(1.5-DZ)+&
                       ((DX-0.5)*(VNEL-VEL)+(1.5-DX)*(VNL-VCL))*(DZ-0.5))/IDSY
                  IF(NY(III,JJJ).LT.LLL) VC=SL*V(III+1,JJJ,LLL)
                  IF(NY(III+1,JJJ).LT.LLL) VE=SL*V(III,JJJ,LLL)
                  IF(NY(III,JJJ+1).LT.LLL) VN=SL*V(III+1,JJJ+1,LLL)
                  IF(NY(III+1,JJJ+1).LT.LLL) VNE=SL*V(III,JJJ+1,LLL)
                  IF(NY(III,JJJ).LE.LLL) VCL=SL*V(III+1,JJJ,LLL+1)
                  IF(NY(III+1,JJJ).LE.LLL) VEL=SL*V(III,JJJ,LLL+1)
                  IF(NY(III,JJJ+1).LE.LLL) VNL=SL*V(III+1,JJJ+1,LLL+1)
                  IF(NY(III+1,JJJ+1).LE.LLL) VNEL=SL*V(III,JJJ+1,LLL+1)
                  DVDX=((DY*(VNE-VN)+(1.0-DY)*(VE-VC))*(1.5-DZ)+&
                       (DY*(VNEL-VNL)+(1.0-DY)*(VEL-VCL))*(DZ-0.5))/IDSY
                  IF(NY(III,JJJ).LT.LLL) VC=0.0
                  IF(NY(III+1,JJJ).LT.LLL) VE=0.0
                  IF(NY(III,JJJ+1).LT.LLL) VN=0.0
                  IF(NY(III+1,JJJ+1).LT.LLL) VNE=0.0
                  IF(NY(III,JJJ).LT.LLL) VCL=0.0
                  IF(NY(III+1,JJJ).LT.LLL) VEL=0.0
                  IF(NY(III,JJJ+1).LT.LLL) VNL=0.0
                  IF(NY(III+1,JJJ+1).LT.LLL) VNEL=0.0
                  IF(NY(III,JJJ).EQ.LLL) VCL=SL*V(III,JJJ,LLL)
                  IF(NY(III+1,JJJ).EQ.LLL) VEL=SL*V(III+1,JJJ,LLL)
                  IF(NY(III,JJJ+1).EQ.LLL) VNL=SL*V(III,JJJ+1,LLL)
                  IF(NY(III+1,JJJ+1).EQ.LLL) VNEL=SL*V(III+1,JJJ+1,LLL)
                  DVDZ=2*((DX-0.5)*DY*(VNE-VNEL)+(DX-0.5)*(1.0-DY)*(VE-VEL)+&
                       (1.5-DX)*(1.0-DY)*(VC-VCL)+(1.5-DX)*DY*(VN-VNL))&
                       /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL+1))
               END IF

!
            ELSE!DX

!
               IF((NX(III,JJJ).LT.LLL).OR.(NX(III,JJJ+1).LT.LLL))&
                    VNW=SL*V(III,JJJ+1,LLL)
               IF((NX(III,JJJ).LT.LLL).OR.(NX(III,JJJ-1).LT.LLL))&
                    VW=SL*V(III,JJJ,LLL)
               IF(DZ.LT.0.5) THEN
                  IF(LLL.EQ.1) THEN
                     PCLV=(DX+0.5)*DY*VN+(DX+0.5)*(1.0-DY)*VC+&
                          (0.5-DX)*(1.0-DY)*VW+(0.5-DX)*DY*VNW
                     DVDY=((0.5+DX)*(VN-VC)+(0.5-DX)*(VNW-VW))/IDSY
                     IF(NY(III,JJJ).LE.0.0) VC=SL*V(III-1,JJJ,LLL)
                     IF(NY(III-1,JJJ).LE.0.0) VW=SL*V(III,JJJ,LLL)
                     IF(NY(III,JJJ+1).LE.0.0) VN=SL*V(III-1,JJJ+1,LLL)
                     IF(NY(III-1,JJJ+1).LE.0.0) VNW=SL*V(III,JJJ+1,LLL)
                     DVDX=(DY*(VN-VNW)+(1.0-DY)*(VC-VW))/IDSY
                     DVDZ=0.0
                     !
                  ELSE
                     !
                     IF((NX(III,JJJ).LT.LLL-1).OR.(NX(III,JJJ+1).LT.LLL-1))&
                          VNWU=SL*V(III,JJJ+1,LLL-1)
                     IF((NX(III,JJJ).LT.LLL-1).OR.(NX(III,JJJ-1).LT.LLL-1))&
                          VWU=SL*V(III,JJJ,LLL-1)
                     PCLV=((DX+0.5)*DY*VN+(DX+0.5)*(1.0-DY)*VC+&
                          (0.5-DX)*(1.0-DY)*VW+(0.5-DX)*DY*VNW)*(DZ+0.5)+&
                          ((DX+0.5)*DY*VNU+(DX+0.5)*(1.0-DY)*VCU+&
                          (0.5-DX)*(1.0-DY)*VWU+(0.5-DX)*DY*VNWU)*(0.5-DZ)
                     DVDY=(((0.5+DX)*(VN-VC)+(0.5-DX)*(VNW-VW))*(DZ+0.5)+&
                          ((0.5+DX)*(VNU-VCU)+(0.5-DX)*(VNWU-VWU))*(0.5-DZ))/IDSY
                     IF(NY(III,JJJ).LT.LLL-1) VCU=SL*V(III-1,JJJ,LLL-1)
                     IF(NY(III-1,JJJ).LT.LLL-1) VWU=SL*V(III,JJJ,LLL-1)
                     IF(NY(III,JJJ+1).LT.LLL-1) VNU=SL*V(III-1,JJJ+1,LLL-1)
                     IF(NY(III-1,JJJ+1).LT.LLL-1) VNWU=SL*V(III,JJJ+1,LLL-1)
                     IF(NY(III,JJJ).LE.LLL-1) VC=SL*V(III-1,JJJ,LLL)
                     IF(NY(III-1,JJJ).LE.LLL-1) VW=SL*V(III,JJJ,LLL)
                     IF(NY(III,JJJ+1).LE.LLL-1) VN=SL*V(III-1,JJJ+1,LLL)
                     IF(NY(III-1,JJJ+1).LE.LLL-1) VNW=SL*V(III,JJJ+1,LLL)
                     DVDX=((DY*(VN-VNW)+(1.0-DY)*(VC-VW))*(DZ+0.5)+&
                          (DY*(VNU-VNWU)+(1.0-DY)*(VCU-VWU))*(0.5-DZ))/IDSY
                     IF(NY(III,JJJ).LT.LLL-1) VCU=0.0
                     IF(NY(III-1,JJJ).LT.LLL-1) VWU=0.0
                     IF(NY(III,JJJ+1).LT.LLL-1) VNU=0.0
                     IF(NY(III-1,JJJ+1).LT.LLL-1) VNWU=0.0
                     IF(NY(III,JJJ).LT.LLL-1) VC=0.0
                     IF(NY(III-1,JJJ).LT.LLL-1) VW=0.0
                     IF(NY(III,JJJ+1).LT.LLL-1) VN=0.0
                     IF(NY(III-1,JJJ+1).LT.LLL-1) VNW=0.0
                     IF(NY(III,JJJ).EQ.LLL-1) VC=SL*V(III,JJJ,LLL-1)
                     IF(NY(III-1,JJJ).EQ.LLL-1) VW=SL*V(III-1,JJJ,LLL-1)
                     IF(NY(III,JJJ+1).EQ.LLL-1) VN=SL*V(III,JJJ+1,LLL-1)
                     IF(NY(III-1,JJJ+1).EQ.LLL-1) VNW=SL*V(III-1,JJJ+1,LLL-1)
                     DVDZ=2*((0.5+DX)*DY*(VNU-VN)+(0.5+DX)*(1.0-DY)*(VCU-VC)+&
                          (0.5-DX)*(1.0-DY)*(VWU-VW)+(0.5-DX)*DY*(VNWU-VNW))&
                          /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL-1))
                  END IF
                  !
               ELSE
                  !
                  IF(LLL.EQ.NY(III,JJJ)) VCL=SL*V(III,JJJ,LLL)
                  IF(LLL.EQ.NY(III-1,JJJ)) VWL=SL*V(III-1,JJJ,LLL)
                  IF(LLL.EQ.NY(III,JJJ+1)) VNL=SL*V(III,JJJ+1,LLL)
                  IF(LLL.EQ.NY(III-1,JJJ+1)) VNWL=SL*V(III-1,JJJ+1,LLL)
                  IF((NX(III,JJJ).LT.LLL+1).OR.(NX(III,JJJ+1).LT.LLL+1))&
                       VNWL=SL*V(III,JJJ+1,LLL+1)
                  IF((NX(III,JJJ).LT.LLL+1).OR.(NX(III,JJJ-1).LT.LLL+1))&
                       VWL=SL*V(III,JJJ,LLL+1)
                  PCLV=((DX+0.5)*DY*VN+(DX+0.5)*(1.0-DY)*VC+&
                       (0.5-DX)*(1.0-DY)*VW+(0.5-DX)*DY*VNW)*(1.5-DZ)+&
                       ((DX+0.5)*DY*VNL+(DX+0.5)*(1.0-DY)*VCL+&
                       (0.5-DX)*(1.0-DY)*VWL+(0.5-DX)*DY*VNWL)*(DZ-0.5)
                  DVDY=(((0.5+DX)*(VN-VC)+(0.5-DX)*(VNW-VW))*(1.5-DZ)+&
                       ((0.5+DX)*(VNL-VCL)+(0.5-DX)*(VNWL-VWL))*(DZ-0.5))/IDSY
                  IF(NY(III,JJJ).LT.LLL) VC=SL*V(III-1,JJJ,LLL)
                  IF(NY(III-1,JJJ).LT.LLL) VW=SL*V(III,JJJ,LLL)
                  IF(NY(III,JJJ+1).LT.LLL) VN=SL*V(III-1,JJJ+1,LLL)
                  IF(NY(III-1,JJJ+1).LT.LLL) VNW=SL*V(III,JJJ+1,LLL)
                  IF(NY(III,JJJ).LE.LLL) VCL=SL*V(III-1,JJJ,LLL+1)
                  IF(NY(III-1,JJJ).LE.LLL) VWL=SL*V(III,JJJ,LLL+1)
                  IF(NY(III,JJJ+1).LE.LLL) VNL=SL*V(III-1,JJJ+1,LLL+1)
                  IF(NY(III-1,JJJ+1).LE.LLL) VNWL=SL*V(III,JJJ+1,LLL+1)
                  DVDX=((DY*(VN-VNW)+(1.0-DY)*(VC-VW))*(1.5-DZ)+&
                       (DY*(VNL-VNWL)+(1.0-DY)*(VCL-VWL))*(DZ-0.5))/IDSY
                  IF(NY(III,JJJ).LT.LLL) VC=0.0
                  IF(NY(III-1,JJJ).LT.LLL) VW=0.0
                  IF(NY(III,JJJ+1).LT.LLL) VN=0.0
                  IF(NY(III-1,JJJ+1).LT.LLL) VNW=0.0
                  IF(NY(III,JJJ).LT.LLL) VCL=0.0
                  IF(NY(III-1,JJJ).LT.LLL) VWL=0.0
                  IF(NY(III,JJJ+1).LT.LLL) VNL=0.0
                  IF(NY(III-1,JJJ+1).LT.LLL) VNWL=0.0
                  IF(NY(III,JJJ).EQ.LLL) VCL=SL*V(III,JJJ,LLL)
                  IF(NY(III-1,JJJ).EQ.LLL) VWL=SL*V(III-1,JJJ,LLL)
                  IF(NY(III,JJJ+1).EQ.LLL) VNL=SL*V(III,JJJ+1,LLL)
                  IF(NY(III-1,JJJ+1).EQ.LLL) VNWL=SL*V(III-1,JJJ+1,LLL)
                  DVDZ=2*((0.5+DX)*DY*(VN-VNL)+(0.5+DX)*(1.0-DY)*(VC-VCL)+&
                       (0.5-DX)*(1.0-DY)*(VW-VWL)+(0.5-DX)*DY*(VNW-VNWL))&
                       /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL+1))
               END IF
            END IF
!
!  *****  W-COMPONENT VELOCITY  *****
!      
	
            IF(DX.GE.0.5) THEN
               IF(DY.GE.0.5) THEN
                  IF((NX(III+1,JJJ).LT.LLL).AND.(NY(III,JJJ+1).LT.LLL)) THEN
                     PCLW=DZ*WC+(1.0-DZ)*WCU
                  ELSE
                     IF(NX(III+1,JJJ).LT.LLL) WE=SL*W(III,JJJ,LLL)
                     IF(NX(III+1,JJJ+1).LT.LLL) WNE=SL*W(III,JJJ+1,LLL)
                     IF(NY(III,JJJ+1).LT.LLL) WN=SL*W(III,JJJ,LLL)
                     IF(NY(III+1,JJJ+1).LT.LLL) WNE=SL*W(III+1,JJJ,LLL)
                     IF(LLL.EQ.1) THEN
                        WCU=0.0
                        WNU=0.0
                        WEU=0.0
                        WNEU=0.0
                     ELSE
                        IF(NX(III+1,JJJ).LT.LLL-1) WEU=SL*W(III,JJJ,LLL-1)
                        IF(NX(III+1,JJJ+1).LT.LLL-1) WNEU=SL*W(III,JJJ+1,LLL-1)
                        IF(NY(III,JJJ+1).LT.LLL-1) WNU=SL*W(III,JJJ,LLL-1)
                        IF(NY(III+1,JJJ+1).LT.LLL-1) WNEU=SL*W(III+1,JJJ,LLL-1)
                     END IF
                     PCLW=((DX-0.5)*(DY-0.5)*WNE+(DX-0.5)*(1.5-DY)*WE+&
                          (1.5-DX)*(1.5-DY)*WC+(1.5-DX)*(DY-0.5)*WN)*DZ+&
                          ((DX-0.5)*(DY-0.5)*WNEU+(DX-0.5)*(1.5-DY)*WEU+&
                          (1.5-DX)*(1.5-DY)*WCU+(1.5-DX)*(DY-0.5)*WNU)*(1.0-DZ)
!      DWDX=(((WE-WC)*(1.5-DY)+(WNE-WN)*(DY-0.5))*DZ+&
!    &       ((WEU-WCU)*(1.5-DY)+(WNEU-WNU)*(DY-0.5))*(1.0-DZ))/IDS
!      DWDY=(((WNE-WE)*(1.5-DX)+(WN-WC)*(DX-0.5))*DZ+&
!    &       (WNEU-WEU)*(1.5-DX)+(WNU-WCU)*(DX-0.5))*(1.0-DZ))/IDS
!      DWDZ=((DX-0.5)*(DY-0.5)*(WNEU-WNE)+(DX-0.5)*(1.5-DY)*(WEU-WE)+&
!    &      (1.5-DX)*(1.5-DY)*(WCU-WC)+(1.5-DX)*(DY-0.5)*(WNU-WN))&
!    &      /HH(III,JJJ,LLL)
                  END IF
!
               ELSE
!
                  IF((NX(III+1,JJJ).LT.LLL).AND.(NY(III,JJJ).LT.LLL)) THEN
                     PCLW=DZ*WC+(1.0-DZ)*WCU
        ELSE
        IF(NY(III,JJJ).LT.LLL) WS=SL*W(III,JJJ,LLL)
        IF(NY(III+1,JJJ).LT.LLL) WSE=SL*W(III+1,JJJ,LLL)
        IF(NX(III+1,JJJ).LT.LLL) WE=SL*W(III,JJJ,LLL)
        IF(NX(III+1,JJJ-1).LT.LLL) WSE=SL*W(III,JJJ-1,LLL)
        IF(LLL.EQ.1) THEN
          WSU=0.0
          WCU=0.0
          WEU=0.0
          WSEU=0.0
         ELSE
          IF(NY(III,JJJ).LT.LLL-1) WSU=SL*W(III,JJJ,LLL-1)
          IF(NY(III+1,JJJ).LT.LLL-1) WSEU=SL*W(III+1,JJJ,LLL-1)
          IF(NX(III+1,JJJ).LT.LLL-1) WEU=SL*W(III,JJJ,LLL-1)
          IF(NX(III+1,JJJ-1).LT.LLL-1) WSEU=SL*W(III,JJJ-1,LLL-1)
        END IF
       PCLW=((DX-0.5)*(0.5+DY)*WE+(DX-0.5)*(0.5-DY)*WSE+&
           (1.5-DX)*(0.5-DY)*WS+(1.5-DX)*(0.5+DY)*WC)*DZ+&
           ((DX-0.5)*(0.5+DY)*WEU+(DX-0.5)*(0.5-DY)*WSEU+&
           (1.5-DX)*(0.5-DY)*WSU+(1.5-DX)*(0.5+DY)*WCU)*(1.0-DZ)
!      DWDX=(((WE-WC)*(0.5+DY)+(WSE-WS)*(0.5-DY))*DZ+&
!    &       ((WEU-WCU)*(0.5+DY)+(WSEU-WSU)*(0.5-DY))*(1.0-DZ))/IDS
!      DWDY=(((WE-WSE)*(DX-0.5)+(WC-WS)*(1.5-DX))*DZ+&
!    &       ((WEU-WSEU)*(DX-0.5)+(WCU-WSU)*(1.5-DX))*(1.0-DZ))/IDS
!      DWDZ=((DX-0.5)*(DY+0.5)*(WEU-WE)+(DX-0.5)*(0.5-DY)*(WSEU-WSE)+&
!    &      (1.5-DX)*(0.5-DY)*(WSU-WS)+(1.5-DX)*(DY+0.5)*(WCU-WC))
!    &      /HH(III,JJJ,LLL)
       END IF
       END IF

!
       ELSE

!
       IF(DY.GE.0.5) THEN
        IF((NX(III,JJJ).LT.LLL).AND.(NY(III,JJJ+1).LT.LLL)) THEN
        PCLW=DZ*WC+(1.0-DZ)*WCU
         ELSE
       IF(NX(III,JJJ).LT.LLL) WW=SL*W(III,JJJ,LLL)
       IF(NX(III,JJJ+1).LT.LLL) WNW=SL*W(III,JJJ+1,LLL)
       IF(NY(III,JJJ+1).LT.LLL) WN=SL*W(III,JJJ,LLL)
       IF(NY(III-1,JJJ+1).LT.LLL) WNW=SL*W(III-1,JJJ,LLL)
       IF(LLL.EQ.1) THEN
         WWU=0.0
         WCU=0.0
         WNU=0.0
         WNWU=0.0
        ELSE
         IF(NX(III,JJJ).LT.LLL-1) WWU=SL*W(III,JJJ,LLL-1)
         IF(NX(III,JJJ+1).LT.LLL-1) WNWU=SL*W(III,JJJ+1,LLL-1)
         IF(NY(III,JJJ+1).LT.LLL-1) WNU=SL*W(III,JJJ,LLL-1)
         IF(NY(III-1,JJJ+1).LT.LLL-1) WNWU=SL*W(III-1,JJJ,LLL-1)
       END IF
       PCLW=((DX+0.5)*(DY-0.5)*WN+(DX+0.5)*(1.5-DY)*WC+&
           (0.5-DX)*(1.5-DY)*WW+(0.5-DX)*(DY-0.5)*WNW)*DZ+&
           ((DX+0.5)*(DY-0.5)*WNU+(DX+0.5)*(1.5-DY)*WCU+&
           (0.5-DX)*(1.5-DY)*WWU+(0.5-DX)*(DY-0.5)*WNWU)*(1.0-DZ)
!      DWDX=(((WN-WNW)*(DY-0.5)+(WC-WW)*(1.5-DY))*DZ+
!    &       ((WNU-WNWU)*(DY-0.5)+(WCU-WWU)*(1.5-DY))*(1.0-DZ))/IDS
!      DWDY=(((WN-WC)*(0.5-DX)+(WNW-WW)*(DX+0.5))*DZ+
!    &       ((WNU-WCU)*(0.5-DX)+(WNWU-WWU)*(DX+0.5))*(1.0-DZ))/IDS
!      DWDZ=((DX+0.5)*(DY-0.5)*(WNU-WN)+(DX+0.5)*(1.5-DY)*(WCU-WC)+
!    &      (0.5-DX)*(1.5-DY)*(WWU-WW)+(0.5-DX)*(DY-0.5)*(WNWU-WNW))
!    &      /HH(III,JJJ,LLL)
       END IF
!
       ELSE
!
       IF((NX(III,JJJ).LT.LLL).AND.(NY(III,JJJ).LT.LLL)) THEN
        PCLW=DZ*WC+(1.0-DZ)*WCU
        ELSE
       IF(NX(III,JJJ).LT.LLL) WW=SL*W(III,JJJ,LLL)
       IF(NX(III,JJJ-1).LT.LLL) WSW=SL*W(III,JJJ-1,LLL)
       IF(NY(III,JJJ).LT.LLL) WS=SL*W(III,JJJ,LLL)
       IF(NY(III-1,JJJ).LT.LLL) WSW=SL*W(III-1,JJJ,LLL)
       IF(LLL.EQ.1) THEN
         WCU=0.0
         WWU=0.0
         WSU=0.0
         WSWU=0.0
        ELSE
         IF(NX(III,JJJ).LT.LLL-1) WWU=SL*W(III,JJJ,LLL-1)
         IF(NX(III,JJJ-1).LT.LLL-1) WSWU=SL*W(III,JJJ-1,LLL-1)
         IF(NY(III,JJJ).LT.LLL-1) WSU=SL*W(III,JJJ,LLL-1)
         IF(NY(III-1,JJJ).LT.LLL-1) WSWU=SL*W(III-1,JJJ,LLL-1)
       END IF
       PCLW=((DX+0.5)*(0.5+DY)*WC+(DX+0.5)*(0.5-DY)*WS+&
           (0.5-DX)*(0.5-DY)*WSW+(0.5-DX)*(0.5+DY)*WW)*DZ+&
           ((DX+0.5)*(0.5+DY)*WCU+(DX+0.5)*(0.5-DY)*WSU+&
           (0.5-DX)*(0.5-DY)*WSWU+(0.5-DX)*(0.5+DY)*WWU)*(1.0-DZ)
!      DWDX=(((WC-WW)*(0.5+DY)+(WS-WSW)*(0.5-DY))*DZ+
!    &       ((WCU-WWU)*(0.5+DY)+(WSU-WSWU)*(0.5-DY))*(1.0-DZ))/IDS
!      DWDY=(((WC-WS)*(DX+0.5)+(WW-WSW)*(0.5-DX))*DZ+
!    &       ((WCU-WSU)*(DX+0.5)+(WWU-WSWU)*(0.5-DX))*(1.0-DZ))/IDS
!      DWDZ=((DX+0.5)*(DY+0.5)*(WCU-WC)+(DX+0.5)*(0.5-DY)*(WSU-WS)+
!    &      (0.5-DX)*(0.5-DY)*(WSWU-WSW)+(0.5-DX)*(DY+0.5)*(WWU-WW))
!    &      /HH(III,JJJ,LLL)
       END IF
       END IF
       END IF
!*************************************************************************
!
      IF(ITRA.LT.2) THEN
        PCLXO=PCLX(I)
        PCLYO=PCLY(I)
        PCLZO=PCLZ(I)
        IIIO=INT(PCLX(I)/(IDSX(i)))+1
        JJJO=INT(PCLY(I)/real(IDSY))+1
!        write(*,*) 'IIIO=',IIIO,'JJJO=',IIIO
        SPCLXO=PCLU+(PCLU*DUDX+PCLV*DUDY+PCLW*DUDZ)*DDT
        SPCLYO=PCLV+(PCLU*DVDX+PCLV*DVDY+PCLW*DVDZ)*DDT
        SPCLZO=PCLW
        SPCLX=PCLXO+DDT*SPCLXO
        SPCLY=PCLYO+DDT*SPCLYO
        SPCLZ=PCLZO-DDT*SPCLZO
        IF(SPCLX.LT.0.0) SPCLX=0.0
        IF(SPCLY.LT.0.0) SPCLY=0.0
        IF(SPCLZ.LT.0.0) SPCLZ=0.0
        IF(SPCLX.GT.li*IDSX(i)) SPCLX=li*IDSX(i)-1
        IF(SPCLZ.GT.lj*IDSY) SPCLY=lj*IDSY-1
        GOTO 110
       ELSE
        SPCLXS=PCLU+(PCLU*DUDX+PCLV*DUDY+PCLW*DUDZ)*DDT
        SPCLYS=PCLV+(PCLU*DVDX+PCLV*DVDY+PCLW*DVDZ)*DDT
        SPCLZS=PCLW
        SPCLX=PCLXO+DDT*(SPCLXO+SPCLXS)*0.5D0
        SPCLY=PCLYO+DDT*(SPCLYO+SPCLYS)*0.5D0
        SPCLZ=PCLZO-DDT*(SPCLZO+SPCLZS)*0.5D0
        IF(SPCLX.LT.0.0) SPCLX=0.0
        IF(SPCLY.LT.0.0) SPCLY=0.0
        IF(SPCLZ.LT.0.0) SPCLZ=0.0
        IF(SPCLX.GT.li*IDSX(i)) SPCLX=li*IDSX(i)-1
        IF(SPCLY.GT.lj*IDSY) SPCLY=lj*IDSY-1
       END IF
!
! ********************************************************************
!
  110 NIII=INT(SPCLX/IDSX(i))+1
      NJJJ=INT(SPCLY/IDSY)+1
!      write(*,*) 'spclx=',spclx,niii,'spcly=',spcly,njjj
!       write(*,*) I,PCLX(I),PCLY(I),PCLZ(I)
!       write(*,*) ITRA,IDS,DDT
!       write(*,*) SPCLX,SPCLY
!       write(*,*) NIII,IIIO
!       write(*,*) NJJJ,JJJO      
      IF((ABS(NIII-IIIO).GE.2.0).OR.(ABS(NJJJ-JJJO).GE.2.0))&
      PCLX(I)=99999
      PCLY(I)=99999
      PCLZ(I)=99999
      GOTO 800     
! STOP 'TIME STEP IS TOO LARGE FOR PCL'

!
! #####################################################################
!     write(*,*) ITRA,IIIO,JJJO,NIII,NJJJ,PCLXO,PCLYO,PCLZO,SPCLX,SPCLY,
!    &           SPCLZ
! #####################################################################
      IF((ABS(NJJJ-JJJO).LE.0.5).AND.(ABS(NIII-IIIO).LE.0.5)) GOTO 800
      IF(ABS(SPCLX-PCLXO).LT.1.0D-6) THEN
         PCLGRAD=1.0D5
        ELSE IF(ABS(SPCLY-PCLYO).LT.1.0D-6) THEN
         PCLGRAD=1.0D-5
        ELSE
         PCLGRAD=(SPCLY-PCLYO)/(SPCLX-PCLXO)
      END IF

      IF(REF.GT.0.0) GOTO 600
! ---------------------------------------------------------------------
      IF(NJJJ-JJJO.GE.0.5) THEN
          XINTC=PCLXO+(1.0-DY)*IDSX(i)/PCLGRAD
       IF(NIII-IIIO.GE.0.5) THEN
          YINTC=PCLYO+PCLGRAD*(1.0-DX)*IDSY
        IF(YINTC.GE.JJJO*IDSY) THEN
         IF(NY(IIIO,NJJJ).LT.0.5) THEN
           SPCLX=XINTC
           SPCLY=JJJO*IDSY-EPSH
          ELSE IF(NX(NIII,NJJJ).LT.0.5) THEN
           SPCLX=IIIO*IDSX(i)-EPSH
           SPCLY=YINTC
          ELSE
           CONTINUE
         END IF
         ELSE
         IF(NX(NIII,JJJO).LT.0.5) THEN
           SPCLX=IIIO*IDSX(i)-EPSH
           SPCLY=YINTC
          ELSE IF(NY(NIII,NJJJ).LT.0.5) THEN
           SPCLX=XINTC
           SPCLY=JJJO*IDSY-EPSH
          ELSE
           CONTINUE
         END IF
        END IF

         ELSE IF(NIII-IIIO.LE.-0.5) THEN
          YINTC=PCLYO-PCLGRAD*DX*IDSY
         IF(YINTC.GE.JJJO*IDSY) THEN
          IF(NY(IIIO,NJJJ).LT.0.5) THEN
            SPCLX=XINTC
            SPCLY=JJJO*IDSY-EPSH
           ELSE IF(NX(IIIO,NJJJ).LT.0.5) THEN
            SPCLX=NIII*IDSX(i)+EPSH
            SPCLY=YINTC
           ELSE
            CONTINUE
          END IF
          ELSE
          IF(NX(IIIO,JJJO).LT.0.5) THEN
            SPCLX=NIII*IDSX(i)+EPSH
            SPCLY=YINTC
           ELSE IF(NY(NIII,NJJJ).LT.0.5) THEN
            SPCLX=XINTC
            SPCLY=JJJO*IDSY-EPSH
           ELSE
            CONTINUE
          END IF
         END IF
        ELSE
          IF(NY(NIII,NJJJ).LT.0.5) SPCLX=XINTC
          IF(NY(NIII,NJJJ).LT.0.5) SPCLY=JJJO*IDSY-EPSH
       END IF

       ELSE IF(NJJJ-JJJO.LT.-0.5) THEN
         XINTC=PCLXO-DY*IDSX(i)/PCLGRAD
       IF(NIII-IIIO.GT.0.5) THEN
           YINTC=PCLYO+PCLGRAD*(1.0-DX)*IDSY
        IF(YINTC.GE.NJJJ*IDSY) THEN
         IF(NX(NIII,JJJO).LT.0.5) THEN
           SPCLX=IIIO*IDSX(i)-EPSH
           SPCLY=YINTC
          ELSE IF(NY(NIII,JJJO).LT.0.5) THEN
           SPCLX=XINTC
           SPCLY=NJJJ*IDSY+EPSH
          ELSE
           CONTINUE
         END IF
         ELSE
         IF(NY(IIIO,JJJO).LT.0.5) THEN
           SPCLX=XINTC
           SPCLY=NJJJ*IDSY+EPSH
          ELSE IF(NX(NIII,NJJJ).LT.0.5) THEN
           SPCLX=IIIO*IDSX(i)
           SPCLY=YINTC-EPSH
          ELSE
           CONTINUE
         END IF
        END IF
        ELSE IF(NIII-IIIO.LT.-0.5) THEN
         YINTC=PCLYO-PCLGRAD*DX*IDSY
         IF(YINTC.GE.NJJJ*IDSY) THEN
          IF(NX(IIIO,JJJO).LT.0.5) THEN
            SPCLX=NIII*IDSX(i)+EPSH
            SPCLY=YINTC
           ELSE IF(NY(IIIO,JJJO).LT.0.5) THEN
            SPCLX=XINTC
            SPCLY=NJJJ*IDSY+EPSH
           ELSE
            CONTINUE
          END IF
          ELSE
          IF(NY(IIIO,JJJO).LT.0.5) THEN
            SPCLX=XINTC
            SPCLY=NJJJ*IDSY+EPSH
           ELSE IF(NX(IIIO,NJJJ).LT.0.5) THEN
            SPCLX=NIII*IDSX(i)+EPSH
            SPCLY=YINTC
           ELSE
            CONTINUE
          END IF
         END IF
         ELSE
          IF(NY(IIIO,JJJO).LT.0.5)  SPCLX=XINTC
          IF(NY(IIIO,JJJO).LT.0.5)  SPCLY=NJJJ*IDSY+EPSH
        END IF
        ELSE
         IF(NIII-IIIO.GE.0.5) THEN
           YINTC=PCLYO+PCLGRAD*(1.0-DX)*IDSY
           IF(NX(NIII,NJJJ).LT.0.5) SPCLX=IIIO*IDSX(i)-EPSH
           IF(NX(NIII,NJJJ).LT.0.5) SPCLY=YINTC
          ELSE
           YINTC=PCLYO-PCLGRAD*DX*IDSY
           IF(NX(IIIO,JJJO).LT.0.5) SPCLX=NIII*IDSX(i)+EPSH
           IF(NX(IIIO,JJJO).LT.0.5) SPCLY=YINTC
         END IF
       END IF
       GOTO 800
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  600 CONTINUE
      IF(NJJJ-JJJO.GE.0.5) THEN
          XINTC=PCLXO+(1.0-DY)*IDSX(i)/PCLGRAD
       IF(NIII-IIIO.GE.0.5) THEN
          YINTC=PCLYO+PCLGRAD*(1.0-DX)*IDSY
        IF(YINTC.GE.JJJO*IDSY) THEN
         IF(NY(IIIO,NJJJ).LT.0.5) THEN
           SPCLY=2*JJJO*IDSY-SPCLY
           IF(NX(NIII,JJJO).LT.0.5) SPCLX=2*IIIO*IDSX(i)-SPCLX
          ELSE IF(NX(NIII,NJJJ).LT.0.5) THEN
           SPCLX=2*IIIO*IDSX(i)-SPCLX
           SPCLY=SPCLY
          ELSE
           CONTINUE
         END IF
         ELSE
         IF(NX(NIII,JJJO).LT.0.5) THEN
           SPCLX=2*IIIO*IDSX(i)-SPCLX
           IF(NY(IIIO,NJJJ).LT.0.5) SPCLY=2*IIIO*IDSY-SPCLY
          ELSE IF(NY(NIII,NJJJ).LT.0.5) THEN
           SPCLX=SPCLX
           SPCLY=2*JJJO*IDSY-SPCLY
          ELSE
           CONTINUE
         END IF
        END IF
!
         ELSE IF(NIII-IIIO.LE.-0.5) THEN
          YINTC=PCLYO-PCLGRAD*DX*IDSY
         IF(YINTC.GE.JJJO*IDSY) THEN
          IF(NY(IIIO,NJJJ).LT.0.5) THEN
            SPCLY=2*JJJO*IDSY-SPCLY
            IF(NX(IIIO,JJJO).LT.0.5) SPCLX=2*NIII*IDSX(i)-SPCLX
           ELSE IF(NX(IIIO,NJJJ).LT.0.5) THEN
            SPCLX=2*NIII*IDSX(i)-SPCLX
            SPCLY=SPCLY
           ELSE
            CONTINUE
          END IF
          ELSE
          IF(NX(IIIO,JJJO).LT.0.5) THEN
            SPCLX=2*NIII*IDSX(i)-SPCLX
            IF(NY(IIIO,JJJO).LT.0.5) SPCLY=2*JJJO*IDSY-SPCLY
           ELSE IF(NY(NIII,NJJJ).LT.0.5) THEN
            SPCLX=SPCLX
            SPCLY=2*JJJO*IDSY-SPCLY
           ELSE
            CONTINUE
          END IF
         END IF
        ELSE
          IF(NY(NIII,NJJJ).LT.0.5) SPCLX=SPCLX
          IF(NY(NIII,NJJJ).LT.0.5) SPCLY=2*JJJO*IDSY-SPCLY
       END IF
!
       ELSE IF(NJJJ-JJJO.LT.-0.5) THEN
         XINTC=PCLXO-DY*IDSX(i)/PCLGRAD
       IF(NIII-IIIO.GT.0.5) THEN
         YINTC=PCLYO+PCLGRAD*(1.0-DX)*IDSY
        IF(YINTC.GE.NJJJ*IDSY) THEN
         IF(NX(NIII,JJJO).LT.0.5) THEN
           SPCLX=2*IIIO*IDSX(i)-SPCLX
           IF(NY(IIIO,JJJO).LT.0.5) SPCLY=2*NJJJ*IDSY-SPCLY
          ELSE IF(NY(NIII,JJJO).LT.0.5) THEN
           SPCLX=SPCLX
           SPCLY=2*NJJJ*IDSY-SPCLY
          ELSE
           CONTINUE
         END IF
         ELSE
         IF(NY(IIIO,JJJO).LT.0.5) THEN
           SPCLY=2*NJJJ*IDSY-SPCLY
           IF(NX(NIII,JJJO).LT.0.5) SPCLX=2*IIIO*IDSX(i)-SPCLX
          ELSE IF(NX(NIII,NJJJ).LT.0.5) THEN
           SPCLX=2*IIIO*IDSX(i)-SPCLX
           SPCLY=SPCLY
          ELSE
           CONTINUE
         END IF
        END IF
        ELSE IF(NIII-IIIO.LT.-0.5) THEN
         YINTC=PCLYO-PCLGRAD*DX*IDSY
         IF(YINTC.GE.NJJJ*IDSY) THEN
          IF(NX(IIIO,JJJO).LT.0.5) THEN
            SPCLX=2*NIII*IDSX(i)-SPCLX
            IF(NY(IIIO,JJJO).LT.0.5) SPCLY=2*NJJJ*IDSY-SPCLY
           ELSE IF(NY(IIIO,JJJO).LT.0.5) THEN
            SPCLX=SPCLX
            SPCLY=2*NJJJ*IDSY-SPCLY
           ELSE
            CONTINUE
          END IF
          ELSE
          IF(NY(IIIO,JJJO).LT.0.5) THEN
            SPCLY=2*NJJJ*IDSY-SPCLY
            IF(NX(IIIO,JJJO).LT.0.5) SPCLX=2*NIII*IDSX(i)-SPCLX
           ELSE IF(NX(IIIO,NJJJ).LT.0.5) THEN
            SPCLX=2*NIII*IDSX(i)-SPCLX
            SPCLY=SPCLY
           ELSE
            CONTINUE
          END IF
         END IF
         ELSE
          IF(NY(IIIO,JJJO).LT.0.5)  SPCLX=SPCLX
          IF(NY(IIIO,JJJO).LT.0.5)  SPCLY=2*NJJJ*IDSY-SPCLY
!  #####################################################
!       write(*,*) IIIO,JJJO,NY(IIIO,JJJO),SPCLX,SPCLY
!  #####################################################
        END IF
        ELSE
         IF(NIII-IIIO.GE.0.5) THEN
!          YINTC=PCLYO+PCLGRAD*(1.0-DX)*IDS
           IF(NX(NIII,NJJJ).LT.0.5) SPCLX=2*IIIO*IDSX(i)-SPCLX
           IF(NX(NIII,NJJJ).LT.0.5) SPCLY=SPCLY
          ELSE
!          YINTC=PCLYO-PCLGRAD*DX*IDS
           IF(NX(IIIO,JJJO).LT.0.5) SPCLX=2*NIII*IDSX(i)-SPCLX
           IF(NX(IIIO,JJJO).LT.0.5) SPCLY=SPCLY
         END IF
       END IF
!
  800 PCLX(I)=SPCLX
      PCLY(I)=SPCLY
      PCLZ(I)=SPCLZ
!
!  ***** EFFECT OF DISPERSION  *****  
!     COMPUTING NORMAliZED Ideal NUMBER 
!
      PCLXS=PCLX(I)+SEIKIB(IJ)*SQRT(2.0*DDT*PDIFH)
      PCLYS=PCLY(I)+SEIKIB(IJ)*SQRT(2.0*DDT*PDIFH)
      IF(PCLZ(I).LE.0.D0) PCLZ(I)=1.0
!
      III=INT(PCLXS/IDSX(i))+1
      JJJ=INT(PCLYS/IDSY)+1
      IF(H(III,JJJ).LE.0.D0) THEN
        PCLX(I)=PCLX(I)
        PCLY(I)=PCLY(I)
       ELSE
        PCLX(I)=PCLXS
        PCLY(I)=PCLYS
      END IF
!
      IF(ITRA.LT.2) GOTO 101
      
!  ***** BY XX  *****  
!***** MIGRATION DURATION & OTOLITH INCREMENTS  *****
       DAY=NKAI*DDT/3600./24.
!	   IF(NKAI==1)THEN
!	   OTODAY(I)=DDT/3600./24.
!	   ENDIF

!	   IF(TEMP(III, JJJ, LLL)>10.)THEN !水温が10度以下耳石成長が停止
!	   OTODAY(I)=OTODAY(I)+1./24.
!	   ENDIF

!***** TOTAL LENGTH *****
!	   IF(DAY <= META_SEIKIB(i))THEN !変態開始時間平均115.2日、SD 19.6日、変態開始後TL成長停止(Pre - leptocephalus 9d, Leptocephalus 106d)GROWTH_SEIKIB
!	   TL=GROWTH_SEIKIB(i)*DAY+3.6 !Linear Equation, 単位(㎜)
!      IF(TL_SEIKIB(i)>100) print*, i, TL_SEIKIB(i)
!	   ELSE
!	   TL = TL_SEIKIB(i)
!      IF(TL>100) print*, '2.  ', i, TL, TL_SEIKIB(i), GROWTH_SEIKIB(i)
!	   ENDIF

!***** SWIMMING SPEED *****
!      TL = 0.4315*DAY + 8.5259 !Linear Equation, 単位(㎜)
!      TL = 64.17816355908316*(1 - 1 * EXP(-0.013830852398122474*(NKAI*DDT / 3600. / 24.))) !Von Bertalanffy, 単位(㎜)
!       IF(DAY<= META_SEIKIB(i))THEN
!	     SWIMS=0.
!       ELSE

!	    ENDIF


!SWIMMING SPEED
   if (MOD(NKAI*INT(DDT),24*3600).GT.12*3600) then
	   SWIMS = 0. !(m/sec)
   else
      SWIMS = Speed/100. !(m/sec)
   endif



	   IF(H(III, JJJ).LE.0.) GOTO 300


!*****水平の移動方向の定義***********
!*****磁力線(伏角)の浅い方向へ遊泳する(0°の場合)*************
!WRITE(*,*) 'MAGNET=',magnet(III, JJJ)
MAG_N=magnet(III, JJJ+1)
MAG_NE=magnet(III+1, JJJ+1)
MAG_E=magnet(III+1, JJJ)
MAG_SE=magnet(III+1, JJJ-1)
MAG_S=magnet(III, JJJ-1)
MAG_SW=magnet(III-1, JJJ-1)
MAG_W=magnet(III-1, JJJ)
MAG_NW=magnet(III-1, JJJ+1)

EMAG=MAG_E+0.70710678*(MAG_NE+MAG_SE)
WMAG=MAG_W+0.70710678*(MAG_NW+MAG_SW)
NMAG=MAG_N+0.70710678*(MAG_NE+MAG_NW)
SMAG=MAG_S+0.70710678*(MAG_SE+MAG_SW)
XMAG=WMAG-EMAG
YMAG=SMAG-NMAG

XSEIBUN=XMAG/SQRT(XMAG**2.0+YMAG**2.0)
YSEIBUN=YMAG/SQRT(XMAG**2.0+YMAG**2.0)

IF(XSEIBUN==0.and.YSEIBUN>=0) THEN
DEG0=0.
ELSE IF(XSEIBUN>0.and.YSEIBUN>0) THEN
DEG0=(atan(XSEIBUN/YSEIBUN)*(180/3.1415926535))+Angle
ELSE IF(XSEIBUN>0.and.YSEIBUN==0) THEN
DEG0=90.
ELSE IF(XSEIBUN>0.and.YSEIBUN<0) THEN
DEG0=180.+(atan(XSEIBUN/YSEIBUN)*(180/3.1415926535))+Angle
ELSE IF(XSEIBUN==0.and.YSEIBUN<0) THEN
DEG0=180.
ELSE IF(XSEIBUN<0.and.YSEIBUN<0) THEN
DEG0=180.+(atan(XSEIBUN/YSEIBUN)*(180/3.1415926535))+Angle
ELSE IF(XSEIBUN<0.and.YSEIBUN==0) THEN
DEG0=270.
ELSE IF(XSEIBUN<0.and.YSEIBUN>0) THEN
DEG0=360.+(atan(XSEIBUN/YSEIBUN)*(180/3.1415926535))+Angle
END IF

IF(DEG0>=360.) then
DEG=DEG0-360.
ELSE IF(DEG0<0.) then
DEG=DEG0+360.
ELSE
DEG=DEG0
END IF

IF(DEG==0.) then
XSWIMS=0.
YSWIMS=SWIMS
ELSE IF(DEG>0.AND.DEG<90.0) then
YSWIMS=COS((DEG)*(3.1415926535/180))*SWIMS
XSWIMS=SIN((DEG)*(3.1415926535/180))*SWIMS
ELSE IF(DEG==90.) then
XSWIMS=SWIMS
YSWIMS=0.
ELSE IF(DEG>90.AND.DEG<180.0) then
YSWIMS=COS((DEG)*(3.1415926535/180))*SWIMS
XSWIMS=SIN((DEG)*(3.1415926535/180))*SWIMS
ELSE IF(DEG==180.) then
XSWIMS=0.
YSWIMS=SWIMS*(-1)
ELSE IF(DEG>180.AND.DEG<270.0) then
YSWIMS=COS((DEG)*(3.1415926535/180))*SWIMS
XSWIMS=SIN((DEG)*(3.1415926535/180))*SWIMS
ELSE IF(DEG==270.) then
XSWIMS=SWIMS*(-1)
YSWIMS=0.
ELSE IF(DEG>270.AND.DEG<360.0) then
YSWIMS=COS((DEG)*(3.1415926535/180))*SWIMS
XSWIMS=SIN((DEG)*(3.1415926535/180))*SWIMS
END IF
!   write(*,*) '==========================================='





!***** EFFECT OF SALINITY  *****
!      write(*, *) SWIMS*DDT
!      塩分 = SAL * 1000 + 35
!	   IF(SWIMS.EQ.0.)THEN
!	     XSWIMS=0.
!		 YSWIMS=0.
!	   GOTO 333
!	   ENDIF
!--------------------------------
!      IF(H(III, JJJ).LE.0.) GOTO 300
!------------------------------------------------------------------------------ -
!	   DO NLAYER = LLL, 2, -1 !(水深150m = Layer18層)
!	   IF(SAL(III - 1, JJJ + 1, NLAYER)<-0.035) GOTO 555
!	   IF(SAL(III - 1, JJJ, NLAYER)<-0.035) GOTO 555
!       IF(SAL(III, JJJ + 1, NLAYER)<-0.035) GOTO 555

!	   IF(SAL(III + 1, JJJ, NLAYER)<-0.035) GOTO 555
!	   IF(SAL(III, JJJ - 1, NLAYER)<-0.035) GOTO 555
!	   IF(SAL(III + 1, JJJ + 1, NLAYER)<-0.035) GOTO 555
!       IF(SAL(III + 1, JJJ - 1, NLAYER)<-0.035) GOTO 555
!       IF(SAL(III - 1, JJJ - 1, NLAYER)<-0.035) GOTO 555

!      IF(NLAYER.LE.0) print*, '1.  ', I, LLL, NLAYER, H(III, JJJ)
!	SALL = NLAYER
!	   GOTO 500

            555        continue
!       end do

!	SALL = NLAYER
            500   CONTINUE

!IF(SALL.LE.0) print*, 'SALL. ', H(III, JJJ), I, III, JJJ, LLL, NLAYER, SALL
!SALL = 1
!    SALE=SAL(III+1,JJJ,SALL)*1000+35
!    SALW=SAL(III-1,JJJ,SALL)*1000+35
!    SALN=SAL(III,JJJ+1,SALL)*1000+35
!    SALS=SAL(III,JJJ-1,SALL)*1000+35
!    SALNE=SAL(III+1,JJJ+1,SALL)*1000+35
!	SALNW=SAL(III-1,JJJ+1,SALL)*1000+35
!	SALSE=SAL(III+1,JJJ-1,SALL)*1000+35
!	SALSW=SAL(III-1,JJJ-1,SALL)*1000+35

!	  IF(SALE<=0) SALE=0
!	  IF(SALW<=0) SALW=0
!	  IF(SALN<=0) SALN=0
!	  IF(SALS<=0) SALS=0
!	  IF(SALNE<=0) SALNE=0
!	  IF(SALNW<=0) SALNW=0
!	  IF(SALSE<=0) SALSE=0
!	  IF(SALSW<=0) SALSW=0																	


																	!------------------------------------------------------------------------------ -
																	!DO 555 SALL = LLL, 1
																	!IF(SAL(III, JJJ, 1)<-0.035) SALC = -0.035
																	!501       IF(SALC >= -0.035) GOTO 502
																	!IF(SALC<-0.035) SALC = SAL(III, JJJ, SALL - 1)
																	!
																	!IF(SAL(III + 1, JJJ, 1)*1000. + 35.<0.) SALE = -0.035
																	!502       IF(SALE >= -0.035) GOTO 503
																	!IF(SALE*1000. + 35.<0.) SALE = SAL(III + 1, JJJ, SALL)
																	!
																	!IF(SAL(III - 1, JJJ, 1)*1000. + 35.<0.) SALW = -0.035
																	!503       IF(SALW >= -0.035) GOTO 504
																	!IF(SALW*1000. + 35.<0.) SALW = SAL(III - 1, JJJ, SALL)
																	!
																	!IF(SAL(III, JJJ + 1, 1)*1000. + 35.<0.) SALN = -0.035
																	!504       IF(SALN >= -0.035) GOTO 505
																	!IF(SALN*1000. + 35.<0.) SALN = SAL(III, JJJ + 1, SALL)
																	!
																	!IF(SAL(III, JJJ - 1, 1)*1000. + 35.<0.) SALS = -0.035
																	!505       IF(SALS >= -0.035) GOTO 506
																	!IF(SALS*1000. + 35.<0.) SALS = SAL(III, JJJ - 1, SALL)
																	!
																	!IF(SAL(III + 1, JJJ + 1, 1)*1000. + 35.<0.) SALNE = -0.035
																	!506       IF(SALNE >= -0.035) GOTO 507
																	!IF(SALNE*1000. + 35.<0.) SALNE = SAL(III + 1, JJJ + 1, SALL)
																	!
																	!IF(SAL(III - 1, JJJ + 1, 1)*1000. + 35.<0.) SALNW = -0.035
																	!507       IF(SALNW >= -0.035) GOTO 508
																	!IF(SALNW*1000. + 35.<0.) SALNW = SAL(III - 1, JJJ + 1, SALL)
																	!
																	!IF(SAL(III + 1, JJJ - 1, 1)*1000. + 35.<0.) SALSE = -0.035
																	!508       IF(SALSE >= -0.035) GOTO 509
																	!IF(SALSE*1000. + 35.<0.) SALSE = SAL(III + 1, JJJ - 1, SALL)
																	!
																	!IF(SAL(III - 1, JJJ - 1, 1)*1000. + 35.<0.) SALSW = -0.035
																	!509       IF(SALSW >= -0.035) GOTO 550
																	!IF(SALSW*1000. + 35.<0.) SALSW = SAL(III - 1, JJJ - 1, SALL)
																	!
																	!550       continue
																	!555   CONTINUE
																	!----------------------------------------------------------------------------------------
!      ESAL=SALE+0.70710678*(SALNE+SALSE)
!      WSAL=SALW+0.70710678*(SALNW+SALSW)
!	  NSAL=SALN+0.70710678*(SALNE+SALNW)
!	  SSAL=SALS+0.70710678*(SALSE+SALSW)

!	  XSAL=WSAL-ESAL
!	  YSAL=SSAL-NSAL

																	!== == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == =
																	!XSAL = 0
																	!YSAL = 0
																	!IF(SAL(III - 1, JJJ, LLL)*1000. + 35.>1.) XSAL = XSAL + (SAL(III - 1, JJJ, LLL) - SAL(III, JJJ, LLL))
																	!IF(SAL(III + 1, JJJ, LLL)*1000. + 35.>1.) XSAL = XSAL + (SAL(III, JJJ, LLL) - SAL(III + 1, JJJ, LLL))
																	!IF(SAL(III, JJJ - 1, LLL)*1000. + 35.>1.) YSAL = YSAL + (SAL(III, JJJ - 1, LLL) - SAL(III, JJJ, LLL))
																	!IF(SAL(III, JJJ + 1, LLL)*1000. + 35.>1.) YSAL = YSAL + (SAL(III, JJJ, LLL) - SAL(III, JJJ + 1, LLL))

																	!IF(SAL(III - 1, JJJ - 1, LLL)*1000. + 35.>1.)THEN
																	!XSAL = XSAL + 0.70710678*(SAL(III - 1, JJJ - 1, LLL) - SAL(III, JJJ, LLL))
																	!YSAL = YSAL + 0.70710678*(SAL(III - 1, JJJ - 1, LLL) - SAL(III, JJJ, LLL))
																	!ENDIF

																	!IF(SAL(III - 1, JJJ + 1, LLL)*1000. + 35.>1.)THEN
																	!XSAL = XSAL + 0.70710678*(SAL(III - 1, JJJ + 1, LLL) - SAL(III, JJJ, LLL))
																	!YSAL = YSAL + 0.70710678*(SAL(III, JJJ, LLL) - SAL(III - 1, JJJ + 1, LLL))
																	!ENDIF

																	!IF(SAL(III + 1, JJJ - 1, LLL)*1000. + 35.>1.)THEN
																	!XSAL = XSAL + 0.70710678*(SAL(III, JJJ, LLL) - SAL(III + 1, JJJ - 1, LLL))
																	!YSAL = YSAL + 0.70710678*(SAL(III + 1, JJJ - 1, LLL) - SAL(III, JJJ, LLL))
																	!ENDIF

																	!IF(SAL(III + 1, JJJ + 1, LLL)*1000. + 35.>1.)THEN
																	!XSAL = XSAL + 0.70710678*(SAL(III, JJJ, LLL) - SAL(III + 1, JJJ + 1, LLL))
																	!YSAL = YSAL + 0.70710678*(SAL(III, JJJ, LLL) - SAL(III + 1, JJJ + 1, LLL))
																	!ENDIF

!   求得經向緯向的游泳速度
!        IF(XSAL.EQ.0.AND.YSAL.EQ.0.) THEN
!		  XSWIMS=0.
!		  YSWIMS=0.
!	    ELSE
!		  XSWIMS=XSAL/SQRT(XSAL**2.0+YSAL**2.0)*SWIMS
!		  YSWIMS=YSAL/SQRT(XSAL**2.0+YSAL**2.0)*SWIMS
!		ENDIF

333     CONTINUE

!   ************************************
!   經向緯向的最終位置 = 海流求得的位置 + 游泳移動的位置
	  PCLXS=PCLXS+XSWIMS*DDT
	  PCLYS=PCLYS+YSWIMS*DDT
!   write(*, '(F10.6)') SEIKIB(IJ)
!   ************************************
        IF(PCLZ(I).LE.0.D0) PCLZ(I)=1.0
!
		  III=INT(PCLXS/IDSX(i))+1
		  JJJ=INT(PCLYS/IDSY)+1

!   Check point
																	!IF(III.LE.0) print*, '3.  ', III, XSWIMS, PCLXS, SWIMS, SPD
																	!IF(III.LE.0) print*, '4.  ', i, DAY, META_SEIKIB(i), TL, TL_SEIKIB(i)

		IF(H(III,JJJ).LE.0.) THEN
		  PCLX(I)=PCLX(I)
		  PCLY(I)=PCLY(I)
		ELSE
		  PCLX(I)=PCLXS
		  PCLY(I)=PCLYS
		END IF

!   == == == == == == == == == == == == == OUTPUT == == == == == == == == == == == == ==
300     if (MOD(NKAI*INT(DDT),1*24*3600).LT.1) then
!		if ((SAL(III,JJJ,LLL)*1000.+35.).LE.0.) then
!		   SAL(III,JJJ,LLL)=-.134
!	    endif
!		if (TEMP(III,JJJ,LLL).LE.0.) then
!		   TEMP(III,JJJ,LLL)=-99.
!		endif
!     粒子番号、日数、経度、緯度、水深、輸送水深、
!     塩分、水温、体長、海流速度、遊泳速度、日輪数
!     遊泳速度X、遊泳速度Y
!     2回以後各粒子の結果を出力する

	   !NREC = 1000 * (int(DAY) - 1) + I
	   !write(11, REC=(DAY-1)*NPCL+I) I,DAY,(PCLX(I)/(IDSX(i)*10.))+110.,(PCLY(I)/(IDSY*10.))-10.,H(III,JJJ),PCLZ(I),&
	   !SAL(III,JJJ,LLL)*1000.+35.,TEMP(III,JJJ,LLL),TL,(U(III,JJJ,LLL)**2.0+V(III,JJJ,LLL)**2.0)**0.5,SWIMS,OTODAY(I)
	   !write(11,REC=(DAY-1)*NPCL+I) I,DAY,(PCLX(I)/(IDSX(i)*10.))+110.,(PCLY(I)/(IDSY*10.))-10.,H(III,JJJ),PCLZ(I),&
	   !SAL(III,JJJ,LLL)*1000.+35.,TEMP(III,JJJ,LLL),TL,(U(III,JJJ,LLL)**2.0+V(III,JJJ,LLL)**2.0)**0.5,SWIMS,OTODAY(I)]
! 		 write(11,2) I,DAY,(PCLX(I)/(IDSX(i)*10.))+100.,(PCLY(I)/(IDSY*10.))-10.,PCLZ(I),TL,OTODAY(I)
!2		 FORMAT (I6,1X,F11.2,1X,F11.2,1x,F11.2,1X,F11.2,1X,F11.2,1X,F11.2)

 		 write(11,'(I6,1X,F11.2,1X,F11.2,1x,F11.2,1X,F11.2,1X,F11.2)')&
 		    I,DAY,(PCLX(I)/(IDSX(i)*12.))+X_edge,(PCLY(I)/(IDSY*12.))+Y_edge,PCLZ(I),TEMP(III,JJJ,LLL)
																				
								                                 !REC        FORMAT(I6,1X,F6.2,1X,F11.2,1X,F11.2,1X,F11.2)
								
																			!write(11, 2) I, DAY, (PCLX(I) / (IDSX(i)*10.)) + 110., (PCLY(I) / (IDSY*10.)) - 10., H(III, JJJ), PCLZ(I), &
																			!SAL(III, JJJ, LLL)*1000. + 35., TEMP(III, JJJ, LLL), TL, (U(III, JJJ, LLL)**2.0 + V(III, JJJ, LLL)**2.0)**0.5, SWIMS, OTODAY(I)
																			!, U(III, JJJ, LLL), V(III, JJJ, LLL)

																			!2        FORMAT(I6, 1X, F6.2, 1X, F6.2, 1X, F11.2, 1X, F11.2, 1X, F11.2, 1X, F11.2, 1X, &
																				!F11.2, 1X, F11.2, 1X, F11.2, 1X, F11.2, 1X, F11.2, 1X, F6.2)
																			!, &F11.4, 1X, F11.4)

          endif
           end do
            end subroutine PCL

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
																subroutine intp_u(uf, u, uk, li, lj, ll)
																dimension uf(li, lj + 1, ll), u(li, lj, ll)
																do k = 1, ll
																	do j = 1, lj
																		do i = 1, li
																			uk = 0.01*((uf(i, j, k) + uf(i, j + 1, k))*0.5)
																			if (abs(uk).gt.200) then
																				u(i, j, k) = 0.0
																			else
																				u(i, j, k) = uk
																				end if
																				end do
																				end do
																				end do
																				end subroutine intp_u

																				!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
																				subroutine intp_v(vf, v, vk, li, lj, ll)
																				dimension vf(li, lj + 1, ll), v(li, lj, ll)
																				do k = 1, ll
																					do i = 1, li
																						do j = 1, lj
																							vk = 0.01*((vf(i, j, k) + vf(i, j + 1, k))*0.5)
																							if (abs(vk).gt.200) then
																								v(i, j, k) = 0.0
																							else
																								v(i, j, k) = vk
																								end if
																								end do
																								end do
																								end do
																								end subroutine intp_v

																								!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
																								subroutine intp_w(wf, w, wk, li, lj, ll)
																								dimension wf(li, lj, ll), w(li, lj, ll)
																								do i = 1, li
																									do j = 1, lj
																										do k = 1, ll
																											wk = 0.01*wf(i, j, k)
																											if (abs(wk).gt.200) then
																												w(i, j, k) = 0.0
																											else
																												w(i, j, k) = wk
																												end if
																												end do
																												end do
																												end do
																												end subroutine intp_w
																												!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

																												!subroutine IDS_henko(IDSX, IDSY, PCLX, PCLY, NPCL)

																												!from PCL program
																												!integer NPCL
																												!real(NPCL), PCLY(NPCL)
																												!to PCL program
																												!real IDSX(NPCL), IDSY(NPCL)
																												!only this subroutine
																												!real a, pi, alat, alon, alat1, alon1, f, e, i
																												!real, parameter::a = 6378137.
																												!real(8), parameter::pi = 3.14159265358979

																												!f = 1 / 298.257
																												!e = sqrt(2 * f - f**2)

																												!do i = 1, NPCL
																												!alat(i) = (PCLX(i) / (IDS*10.)) + 110.
																												!alon(i) = (PCLY(i) / (IDS*10.)) - 10.

																												!alat1 = alat / 180.*pi
																												!alon1 = alon / 180.*pi
																												!IDSX(i) = pi*a*cos(alat1)*10. / (180.*10.*sqrt(1 - e**2 * (sin(alat1))**2))
																												!IDSY(i) = pi*a*(1 - e**2) / (180.*(1 - e**2 * (sin(alon1))**2)**1.5)

																												!end do
																												!end subroutine IDS_henko
																												!
																												!------------------------------------------------------------------ -
																												REAL FUNCTION UNIFRD(IJ)
																												!------------------------------------------------------------------ -
																												INTEGER L, C, T30
																												REAL UU
																												PARAMETER(L = 843314861, C = 453816695, T30 = 2.0**30, UU = 2.0**31)
																												IJ = L*IJ + C
																												IF(IJ.LT.0) IJ = (IJ + T30) + T30
																												UNIFRD = MOD(REAL(IJ), UU)
																												UNIFRD = UNIFRD / UU
																												END FUNCTION UNIFRD

																												!SEIKI - RANSU
																												!------------------------------------------------------------------ -
																												REAL FUNCTION SEIKIB(IJ)
																												!------------------------------------------------------------------ -
																												REAL UNIFRD
																												SEIKIB = 0
																												do K = 1, 12
																													SEIKIB = SEIKIB + UNIFRD(IJ)
																													end do
																													SEIKIB = SEIKIB - 6
																													END FUNCTION SEIKIB

