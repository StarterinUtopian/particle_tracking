! gfortran PCL_0701.f90 -L/usr/local/Cellar/netcdf/4.8.0_1/lib -lnetcdff


module globals
	integer,parameter::LN=4320,DN=2041,JN=50
	integer,parameter::li = 640, lj = 522, ll = 50 !读入数 
	integer, parameter::NPCL = 1000    !投入粒子数
	integer, parameter::IJ = 2
	real,parameter::IDSY=9259.25
	real, parameter::pi = 3.1415926535
	real, parameter::X_edge = 100.000000  !読み込むデータのX方向の最西座標
	real, parameter::Y_edge = -13.4166670  !読み込むデータのY方向の最南座標
	integer,parameter::NDAY=150,DDT=1200
	integer,parameter::releas=1
	integer,parameter::method=1
		!  ***  SliP(SL = 1.0) OR NO - SliP(SL = -1.0) ? *******
	!     SL = -1.0　　　 !境界条件、反射する？
	!     SL = 0.0　　　　!境界条件、停止？
	real,parameter::SL=1.0,EPSH=1.0       !境界条件、そのまま移動する？
	!  ***  SliP CONDITION AT BOTTOM  *****************
	!     BTMSliP = -1.0  !海底に着くと輸送STOP
	real,parameter::BTMSliP=1.0,EPSB=0.01  !海底に着いても底に沿って輸送する
	
	!  ***  PREFECT REFRECTION OR NOT ? **************
	real,parameter::REF=1.0
	!     REF = -1.0
	real,parameter::PDIFH=50.0D0!扩散系数
real,save :: DD(ll), HH(li, lj, ll), H(li, lj), NX(li, lj), NY(li, lj),mask(li,lj)
real,save:: U(li, lj), V(li, lj)
real,save:: IDSX(NPCL)
real,save:: RPCLX(NPCL), RPCLY(NPCL)!纬度表示的初始位置
real,save:: PCLX(NPCL), PCLY(NPCL), PCLZ(NPCL)!距离表示的现在位置
real,save:: SEIKIB(IJ)	
character,save:: dateyear(NDAY)*8
character,save:: datenow*8,year*4,month*2
integer,save:: i,j,l,k,NI,ip,IID(NPCL)
integer,save::III,JJJ,LLL,IIIO,JJJO,NIII,NJJJ,ITRA
real,save::PCLXO,PCLYO,PCLZO,PCLXS,PCLYS,PCLZS
real,save::SPCLX,SPCLY,SPCLZ,SPCLXO,SPCLYO,SPCLZO,SPCLXS,SPCLYS,SPCLZS
real,save::XINTC,YINTC,PCLGRAD
real,save::UC,UE,UN,UNE,US,UUSE,UCL,UEL,UNL,UNEL,USL,USEL
real,save::UCU,UEU,UNU,UNEU,USU,USEU,VCU,VEU,VNEU,VNU,VWU,VNWU
real,save::VC,VE,VNE,VN,VW,VNW,VCL,VEL,VNEL,VNL,VWL,VNWL
real,save::WCU,WEU,WWU,WSU,WNU,WNEU,WNWU,WSEU,WSWU
real,save:: WC,WE,WW,WS,WN,WNE,WNW,WSE,WSW
real,save::PCLU,PCLV,PCLW
real,save::DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ,DWDX,DWDY,DWDZ
real,save::DX,DY,DZ
integer,save::DIII,DJJJ
integer,save:: NSSS,MAXKAI,NKAI
end module globals

!****************************************************************
module pcl_track
!****************************************************************
use globals
implicit none
contains

! ******************************************************************************
    subroutine settings
NSSS=NDAY*24*3600        
MAXKAI=INT(NSSS/DDT)
	!読み込む(readする)ファイルを開く(open)
	!装置番号7→各个水层的水深
	open(7, file = '/Volumes/erika/CMEMS/set_input/grid_cmems.dat')
	!装置番号8→各个坐标的实际水深，单个格子的深度
	open(8, file = '/Volumes/erika/CMEMS/set_input/dini_cmems.dat')
     !装置番号16番→ 最深水深所在的水层（虽然我也不知道nx和ny究竟有啥区别）
     open(16, file = '/Volumes/erika/CMEMS/set_input/nxny_cmems.dat')
	!装置番号9→投入位置
	open(9, file = '/Volumes/erika/CMEMS/set_input/start-point/release1.dat')
     !装置番号11→出力されるファイル
     open(11, file = '/Volumes/erika/CMEMS/output/test.dat',FORM='FORMATTED',STATUS='unknown')

	!読み込み開始
	read(7, *) (DD(l), l = 1, ll) !各水層の水深
	read(8, *) ((H(i, j), i = 1, li), j = 1, lj) !実際水深
	read(8, *) (((HH(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)!格子深度
	read(9, *) ((NX(i, j), i = 1, li), j = 1, lj) !MIROCデータの最大水深
	read(16, *) ((NY(i, j), i = 1, li), j = 1, lj)
	close(7)
	close(8)
	close(16)


	
	open(17, file = '/Volumes/erika/CMEMS/set_input/start-timing/20180101.txt')
	read(17,*) dateyear
	close(17)

do ip = 1, NPCL
				
read(9, *) IID(ip), RPCLX(ip), RPCLY(ip), PCLZ(ip)
	IDSX(ip)=(111.111*1000*cos(RPCLY(ip)/360.*2*pi))/12.
	PCLX(ip)=(RPCLX(ip)-X_edge)*IDSX(ip)*12.
	PCLY(ip)=(RPCLY(ip)-Y_edge)*IDSY*12.
	PCLZ(ip)=200.0d0
end do
 close(9)

end subroutine settings

! *************************************************************************************************

subroutine readin_velocity
     
	   NI = NI + 1
		datenow=dateyear(NI)
		year=datenow(1:4)
		month=datenow(5:6)


		open(10,file='/Volumes/erika/CMEMS/processed_data/U/U_'//datenow//'.dat')
		read(10,*)((U(i, j), i = 1, li), j = 1, lj)
		close(10)
		open(11,file='/Volumes/erika/CMEMS/processed_data/V/V_'//datenow//'.dat')
		read(11,*) ((V(i, j, k), i = 1, li), j = 1, lj)
		close(11)
		

		
end subroutine readin_velocity

! ****************************************************************
SUBROUTINE adverage_velocity


		 III=INT(PCLX(IP)/IDSX(IP))+1
		 JJJ=INT(PCLY(IP)/IDSY)+1
           LLL=23
     
        
!         DX, DYは粒子の格子内水平位置を示した
			DX=MOD((PCLX(iP)),IDSX(iP))/IDSX(iP)
			DY=MOD(REAL(PCLY(IP)),IDSY)/IDSY


		  UC=U(III,JJJ)
            UE=U(III+1,JJJ)
            UN=U(III,JJJ+1)
            UNE=U(III+1,JJJ+1)
            US=U(III,JJJ-1)
            UUSE=U(III+1,JJJ-1)

            VC=V(III,JJJ)
            VE=V(III+1,JJJ)
            VNE=V(III+1,JJJ+1)
            VN=V(III,JJJ+1)
            VW=V(III-1,JJJ)
            VNW=V(III-1,JJJ+1)

    

												
																

!  *****  U-COMPONENT VELOCITY  *****
    IF(DY.GE.0.5) THEN
               IF((NY(III,JJJ+1).LT.LLL).OR.(NY(III+1,JJJ+1).LT.LLL))&
                    UNE=SL*U(III+1,JJJ,LLL)
               IF((NY(III,JJJ+1).LT.LLL).OR.(NY(III-1,JJJ+1).LT.LLL))&
                    UN=SL*U(III,JJJ)
                    PCLU=DX*(DY-0.5)*UNE+DX*(1.5-DY)*UE+&
                          (1.0-DX)*(DY-0.5)*UN+(1.0-DX)*(1.5-DY)*UC
                     DUDX=((DY-0.5)*(UNE-UN)+(1.5-DY)*(UE-UC))/IDSX(i)
                     DUDZ=0.0
                     IF(NX(III,JJJ).LE.0.0) UC=SL*U(III,JJJ+1)
                     IF(NX(III,JJJ+1).LE.0.0) UN=SL*U(III,JJJ)
                     IF(NX(III+1,JJJ).LE.0.0) UE=SL*U(III+1,JJJ+1)
                     IF(NX(III+1,JJJ+1).LE.0.0) UNE=SL*U(III+1,JJJ)
                     DUDY=(DX*(UNE-UE)+(1.0-DX)*(UN-UC))/IDSX(i)
     ELSE!DY


               IF((NY(III,JJJ).LT.LLL).OR.(NY(III+1,JJJ).LT.LLL))&
                    UUSE=SL*U(III+1,JJJ,LLL)
               IF((NY(III,JJJ).LT.LLL).OR.(NY(III-1,JJJ).LT.LLL))&
                    US=SL*U(III,JJJ)
                    PCLU=DX*(DY+0.5)*UE+DX*(0.5-DY)*UUSE+&
                          (1.0-DX)*(0.5-DY)*US+(1.0-DX)*(DY+0.5)*UC
                     DUDX=((0.5-DY)*(UUSE-US)+(DY+0.5)*(UE-UC))/IDSX(i)
                     DUDZ=0.0
                     IF(NX(III,JJJ).LE.0.0) UC=SL*U(III,JJJ-1,LLL)
                     IF(NX(III,JJJ-1).LE.0.0) US=SL*U(III,JJJ,LLL)
                     IF(NX(III+1,JJJ).LE.0.0) UE=SL*U(III+1,JJJ-1,LLL)
                     IF(NX(III+1,JJJ-1).LE.0.0) UUSE=SL*U(III+1,JJJ,LLL)
                     DUDY=(DX*(UE-UUSE)+(1.0-DX)*(UC-US))/IDSX(i) 
END IF
  
!  *****  V-COMPONENT VELOCITY  *****
IF(DX.GE.0.5) THEN
               IF((NX(III+1,JJJ).LT.LLL).OR.(NX(III+1,JJJ-1).LT.LLL))&
                    VE=SL*V(III,JJJ)
               IF((NX(III+1,JJJ).LT.LLL).OR.(NX(III+1,JJJ+1).LT.LLL))&
                    VNE=SL*V(III,JJJ+1)
                    PCLV=(DX-0.5)*DY*VNE+(DX-0.5)*(1.0-DY)*VE+&
                          (1.5-DX)*(1.0-DY)*VC+(1.5-DX)*DY*VN
                     DVDY=((DX-0.5)*(VNE-VE)+(1.5-DX)*(VN-VC))/IDSY
                     IF(NY(III,JJJ).LE.0.0) VC=SL*V(III+1,JJJ)
                     IF(NY(III+1,JJJ).LE.0.0) VE=SL*V(III,JJJ)
                     IF(NY(III,JJJ+1).LE.0.0) VN=SL*V(III+1,JJJ+1)
                     IF(NY(III+1,JJJ+1).LE.0.0) VNE=SL*V(III,JJJ+1)
                     DVDX=(DY*(VNE-VN)+(1.0-DY)*(VE-VC))/IDSY

           ELSE!DX

               IF((NX(III,JJJ).LT.LLL).OR.(NX(III,JJJ+1).LT.LLL))&
                    VNW=SL*V(III,JJJ+1,LLL)
               IF((NX(III,JJJ).LT.LLL).OR.(NX(III,JJJ-1).LT.LLL))&
                    VW=SL*V(III,JJJ)
                    PCLV=(DX+0.5)*DY*VN+(DX+0.5)*(1.0-DY)*VC+&
                          (0.5-DX)*(1.0-DY)*VW+(0.5-DX)*DY*VNW
                     DVDY=((0.5+DX)*(VN-VC)+(0.5-DX)*(VNW-VW))/IDSY
                     IF(NY(III,JJJ).LE.0.0) VC=SL*V(III-1,JJJ)
                     IF(NY(III-1,JJJ).LE.0.0) VW=SL*V(III,JJJ)
                     IF(NY(III,JJJ+1).LE.0.0) VN=SL*V(III-1,JJJ+1)
                     IF(NY(III-1,JJJ+1).LE.0.0) VNW=SL*V(III,JJJ+1)
                     DVDX=(DY*(VN-VNW)+(1.0-DY)*(VC-VW))/IDSY
            END IF
                     
      
end subroutine adverage_velocity
!*************************************************************************
subroutine distance_0

        PCLXO=PCLX(IP)
        PCLYO=PCLY(IP)
        IIIO=INT(PCLX(IP)/(IDSX(iP)))+1
        JJJO=INT(PCLY(IP)/real(IDSY))+1

        SPCLXO=PCLU+(PCLU*DUDX+PCLV*DUDY)*DDT
        SPCLYO=PCLV+(PCLU*DVDX+PCLV*DVDY)*DDT

        PCLX(IP)=PCLXO+DDT*SPCLXO
        PCLY(IP)=PCLYO+DDT*SPCLYO

end subroutine distance_0

subroutine distance_1
        SPCLXS=PCLU+(PCLU*DUDX+PCLV*DUDY)*DDT
        SPCLYS=PCLV+(PCLU*DVDX+PCLV*DVDY)*DDT

        PCLX(IP)=PCLXO+DDT*(SPCLXO+SPCLXS)*0.5D0
        PCLY(IP)=PCLYO+DDT*(SPCLYO+SPCLYS)*0.5D0
!边界判断
        IF(SPCLX.LT.0.0) SPCLX=0.0
        IF(SPCLY.LT.0.0) SPCLY=0.0

        IF(SPCLX.GT.li*IDSX(iP)) SPCLX=li*IDSX(iP)-1
        IF(SPCLY.GT.lj*IDSY) SPCLY=lj*IDSY-1
 end subroutine distance_1
     
subroutine random_current
 ! ***** EFFECT OF DISPERSION  *****  
    ! COMPUTING NORMAliZED Ideal NUMBER 
call random_number(SEIKIB)
      PCLXS=PCLX(IP)+SEIKIB(1)*SQRT(2.0*DDT*PDIFH)
      PCLYS=PCLY(IP)+SEIKIB(2)*SQRT(2.0*DDT*PDIFH)
!
      III=INT(PCLXS/IDSX(iP))+1
      JJJ=INT(PCLYS/IDSY)+1
      IF(H(III,JJJ).LE.0.D0)THEN
        PCLX(IP)=PCLX(IP)
        PCLY(IP)=PCLY(IP)
       ELSE
        PCLX(IP)=PCLXS
        PCLY(IP)=PCLYS
      END IF
end subroutine random_current
! ********************************************************************
! subroutine check_land!施工中
!   NIII=INT(SPCLX/IDSX(iP))+1
!   NJJJ=INT(SPCLY/IDSY)+1
  
!   DIII=NIII-IIIO
!   DJJJ=NJJJ-JJJO

! IF((ABS(NIII-IIIO).GE.2.0).OR.(ABS(NIII-IIIO).GE.2.0))&
! PCLX(IP)=99999
! PCLY(IP)=99999
! PCLZ(IP)=99999



! IF((ABS(DJJJ).LE.0.5).AND.(ABS(DIII).LE.0.5)) GOTO 800!如果两边都没跑出格子
! IF(ABS(SPCLX-PCLXO).LT.1.0D-6) THEN!如果x方向跑的距离十分小
!      PCLGRAD=1.0D5
! ELSE IF(ABS(SPCLY-PCLYO).LT.1.0D-6) THEN
!      PCLGRAD=1.0D-5
! ELSE
!      PCLGRAD=(SPCLY-PCLYO)/(SPCLX-PCLXO)!求梯度tan*
! END IF

!       IF(REF.GT.0.0) !GOTO 600

! !
!   800 PCLX(IP)=SPCLX
!       PCLY(IP)=SPCLY
!       PCLZ(IP)=SPCLZ
! !

! end subroutine check_land


subroutine output

! !   粒子番号、日数、経度、緯度、水深、輸送水深、
! ! 	塩分、水温、体長、海流速度、遊泳速度、日輪数
! ! 	遊泳速度X、遊泳速度Y
! ! 	2回以後各粒子の結果を出力する

write(*,'(I6,1X,I4,1X,F11.2,1x,F11.2)')&
IP,NI,RPCLX(IP),RPCLY(IP)


end subroutine output

subroutine R_P
     do iP = 1, NPCL
          IDSX(iP) = (111.111*1000*cos(RPCLY(iP)/360.*2*pi))/12.
          RPCLX(iP)=(PCLX(IP)/(IDSX(iP)*12.))+X_edge
          RPCLY(iP)=(PCLY(IP)/(IDSY*12.))+Y_edge
     end do
end subroutine R_P

end module pcl_track

program main 
use pcl_track
do NKAI = 1, MAXKAI
     if (NKAI==1) then
          NI=0
          call settings
          call readin_velocity
     else
          if (MOD(NKAI*INT(DDT), 1*24*3600) == 0) then 
          call readin_velocity
          call output
          end if
     write(*,'(f11.2,1x,f11.2)')U(:,:),V(:,:)
     ! do ip=1,NPCL
     !      call adverage_velocity
     !      call distance_0
     !      call adverage_velocity
     !      call distance_1
     !      call R_P
     !      call output
     ! enddo 
     end if
end do
end program main




