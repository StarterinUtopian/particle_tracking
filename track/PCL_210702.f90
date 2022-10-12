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
	real::PDIFH=50.0D0!扩散系数
real :: DD(ll), HH(li, lj, ll), H(li, lj), NX(li, lj), NY(li, lj)
integer::IID(NPCL)
real,save:: U(li, lj, ll), V(li, lj, ll), W(li, lj, ll)   
real,save:: IDSX(NPCL)
real,save:: RPCLX(NPCL), RPCLY(NPCL)!纬度表示的初始位置
real,save:: PCLX(NPCL), PCLY(NPCL), PCLZ(NPCL)!距离表示的现在位置
real,save:: SEIKIB	
character,save:: dateyear(NDAY)*8
character,save:: datenow*8,year*4,month*2
integer,save:: i,j,l,k,NI,ip
end module globals

!****************************************************************
module pcl_track
!****************************************************************
use globals
implicit none
contains
! ******************************************************************************
    subroutine settings
	
	!読み込む(readする)ファイルを開く(open)
	!装置番号7→MIROCのx, y, zの格子数などを記述(変更不要)
	open(7, file = '/Volumes/erika/CMEMS/set_input/grid_cmems.dat')
	!装置番号8→各格子点の水深を記述(変更不要)
	open(8, file = '/Volumes/erika/CMEMS/set_input/dini_cmems.dat')
	!装置番号9→投入位置を記述.　0，経度，緯度，水深(自分で作成)
	open(9, file = '/Volumes/erika/CMEMS/set_input/start-point/release1.dat')
	!装置番号11→出力されるファイル
	open(11, file = '/Volumes/erika/CMEMS/output/test.dat',FORM='FORMATTED',STATUS='unknown')
	!装置番号16番→ ? ? ? (変更不要)
	open(16, file = '/Volumes/erika/CMEMS/set_input/nxny_cmems.dat')


	!読み込み開始
	read(7, *) (DD(l), l = 1, ll) !各水層の水深
	read(8, *) ((H(i, j), i = 1, li), j = 1, lj) !実際水深
	read(8, *) (((HH(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)!格子深度
	read(16, *) ((NX(i, j), i = 1, li), j = 1, lj) !MIROCデータの最大水深
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
     NI=0
	   NI = NI + 1
		datenow=dateyear(NI)
		year=datenow(1:4)
		month=datenow(5:6)


		open(4,file='/Volumes/erika/CMEMS/processed_data/U/U_'//datenow//'.dat')
		read(4,*)(((U(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)
		close(4)
		open(5,file='/Volumes/erika/CMEMS/processed_data/V/V_'//datenow//'.dat')
		read(5,*) (((V(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)
		close(5)
		
		! W(:,:,:)=0.0d0
		
end subroutine readin_velocity

! ****************************************************************
SUBROUTINE pcl

integer::III,JJJ,LLL,IIIO,JJJO,NIII,NJJJ,ITRA
real::PCLXO,PCLYO,PCLZO,PCLXS,PCLYS,PCLZS
real::SPCLX,SPCLY,SPCLZ,SPCLXO,SPCLYO,SPCLZO,SPCLXS,SPCLYS,SPCLZS
real::XINTC,YINTC,PCLGRAD
real::UC,UE,UN,UNE,US,UUSE,UCL,UEL,UNL,UNEL,USL,USEL
real::UCU,UEU,UNU,UNEU,USU,USEU,VCU,VEU,VNEU,VNU,VWU,VNWU
real::VC,VE,VNE,VN,VW,VNW,VCL,VEL,VNEL,VNL,VWL,VNWL
real::WCU,WEU,WWU,WSU,WNU,WNEU,WNWU,WSEU,WSWU
real:: WC,WE,WW,WS,WN,WNE,WNW,WSE,WSW
real::PCLU,PCLV,PCLW
real::DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ,DWDX,DWDY,DWDZ
real::DX,DY,DZ
integer::
ITRA=0
101      ITRA=ITRA+1
do ip=1,NPCL
		 III=INT(PCLX(IP)/IDSX(IP))+1
		 JJJ=INT(PCLY(IP)/IDSY)+1
           LLL=37

		DX=MOD((PCLX(iP)),IDSX(iP))/IDSX(iP)
		DY=MOD(REAL(PCLY(IP)),IDSY)/IDSY
          DZ=(PCLZ(IP)-DD(LLL-1))/HH(III,JJJ,LLL)
		
!        write(*, *)'44'
!        粒子がいる格子と周辺格子の各方向における移動速度を計算 ?
if(DY.GE.0.5) then

    PCLU=DX*(DY-0.5)*UNE+DX*(1.5-DY)*UE+&
    (1.0-DX)*(DY-0.5)*UN+(1.0-DX)*(1.5-DY)*UC
    DUDX=((DY-0.5)*(UNE-UN)+(1.5-DY)*(UE-UC))/IDSX(i)
    DUDY=(DX*(UNE-UE)+(1.0-DX)*(UN-UC))/IDSX(i)

  else!DY下半部分

    PCLU=DX*(DY+0.5)*UE+DX*(0.5-DY)*UUSE+&
    (1.0-DX)*(0.5-DY)*US+(1.0-DX)*(DY+0.5)*UC
    DUDX=((0.5-DY)*(UUSE-US)+(DY+0.5)*(UE-UC))/IDSX(i)
    DUDY=(DX*(UE-UUSE)+(1.0-DX)*(UC-US))/IDSX(i)
    !
  end if
!***********************************************************************
  !v_component_velocity
!***********************************************************************
  if(DX.GE.0.5) then

    PCLV=(DX-0.5)*DY*VNE+(DX-0.5)*(1.0-DY)*VE+&
    (1.5-DX)*(1.0-DY)*VC+(1.5-DX)*DY*VN
    DVDY=((DX-0.5)*(VNE-VE)+(1.5-DX)*(VN-VC))/IDSY
    DVDX=(DY*(VNE-VN)+(1.0-DY)*(VE-VC))/IDSY
    DVDZ=0.0

 else!DX<0.5

    PCLV=(DX+0.5)*DY*VN+(DX+0.5)*(1.0-DY)*VC+&
    (0.5-DX)*(1.0-DY)*VW+(0.5-DX)*DY*VNW
    DVDY=((0.5+DX)*(VN-VC)+(0.5-DX)*(VNW-VW))/IDSY
    DVDX=(DY*(VN-VNW)+(1.0-DY)*(VC-VW))/IDSY
    DVDZ=0.0

  end if 

  !*************************************************************************
  ITRA=1
  200 IF(ITRA.LT.2) THEN
    PCLXO=PCLX(I)!把投入位置赋给
    PCLYO=PCLY(I)

    IIIO=INT(PCLX(I)/(IDSX(i)))+1!开始时格子编号
    JJJO=INT(PCLY(I)/real(IDSY))+11!结束时格子编号
    
    SPCLXO=PCLU+(PCLU*DUDX+PCLV*DUDY)*DDT!初始速度
    SPCLYO=PCLV+(PCLU*DVDX+PCLV*DVDY)*DDT

    SPCLX=PCLXO+DDT*SPCLXO
    SPCLY=PCLYO+DDT*SPCLYO
    ! 防止跑出原点
    IF(SPCLX.LT.0.0) SPCLX=0.0
    IF(SPCLY.LT.0.0) SPCLY=0.0
    !防止一下子跑超过一个格子
    IF(SPCLX.GT.li*IDSX(i)) SPCLX=li*IDSX(i)-1
    IF(SPCLY.GT.lj*IDSY) SPCLY=lj*IDSY-1
    ITRA=ITRA+1 
  ELSE
    SPCLXS=PCLU+(PCLU*DUDX+PCLV*DUDY)*DDT
    SPCLYS=PCLV+(PCLU*DVDX+PCLV*DVDY)*DDT

    SPCLX=PCLXO+DDT*(SPCLXO+SPCLXS)*0.5D0
    SPCLY=PCLYO+DDT*(SPCLYO+SPCLYS)*0.5D0

    IF(SPCLX.LT.0.0) SPCLX=0.0
    IF(SPCLY.LT.0.0) SPCLY=0.0

    IF(SPCLX.GT.li*IDSX(i)) SPCLX=li*IDSX(i)-1
    IF(SPCLY.GT.lj*IDSY) SPCLY=lj*IDSY-1
  END IF


  110 NIII=INT(SPCLX/IDSX(i))+1
  NJJJ=INT(SPCLY/IDSY)+1

  IF((ABS(NIII-IIIO).GE.2.0).OR.(ABS(NJJJ-JJJO).GE.2.0))&
  PCLX(I)=99999
  PCLY(I)=99999
  PCLZ(I)=99999
  GOTO 800     
  STOP 'TIME STEP IS TOO LARGE FOR PCL'
  

!
800 PCLX(I)=SPCLX
PCLY(I)=SPCLY

!
!  ***** EFFECT OF DISPERSION  *****  
! COMPUTING NORMAliZED Ideal NUMBER 

! PCLXS=PCLX(I)+SEIKIB(IJ)*SQRT(2.0*DDT*PDIFH)
! PCLYS=PCLY(I)+SEIKIB(IJ)*SQRT(2.0*DDT*PDIFH)

! III=INT(PCLXS/IDSX(i))+1
! JJJ=INT(PCLYS/IDSY)+1

! PCLX(I)=PCLXS
! PCLY(I)=PCLYS
IF(ITRA.LT.2) GOTO 101


end do
end subroutine PCL

! subroutine output

! !   粒子番号、日数、経度、緯度、水深、輸送水深、
! ! 	塩分、水温、体長、海流速度、遊泳速度、日輪数
! ! 	遊泳速度X、遊泳速度Y
! ! 	2回以後各粒子の結果を出力する
! ! do ip=1,NPCL
! ! write(6,'(I6,1X,I4,1X,F11.2,1x,F11.2)')&
! ! ip,NI,(PCLX(ip)/(IDSX(ip)*12.))+X_edge,(PCLY(Ip)/(IDSY*12.))+Y_edge
! ! end do 

! end subroutine output

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



integer:: NSSS,MAXKAI,NKAI


NSSS=NDAY*24*3600        
MAXKAI=INT(NSSS/DDT)

do NKAI = 1, MAXKAI
     if (NKAI==1) then
          call settings
          call readin_velocity
     else
          if (MOD(NKAI*INT(DDT), 1*24*3600) == 0) then 
          call readin_velocity
          end if
     end if
     write(*,'(A8)')datenow
     write(*,*)(((U(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)
     write(*,*)
     call pcl
     ! write(6,'(I6,1X,I6,1X,F11.2,1x,F11.2)')&
! NI,IP,(PCLX(ip)/(IDSX(ip)*12.))+X_edge,(PCLY(Ip)/(IDSY*12.))+Y_edge
     call R_P

end do
end program main




