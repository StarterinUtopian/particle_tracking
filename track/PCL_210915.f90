! ifort PCL_0701.f90 -L/usr/local/Cellar/netcdf/4.8.0_1/lib -lnetcdff
module globals
    integer,parameter::LN=4320,DN=2041,JN=50!数据总数
    integer,parameter::li = 640, lj = 522, ll = 50 !读入数 
    integer, parameter::NPCL = 1000    !投入粒子数
    integer, parameter::IJ = 2
    real,parameter::IDSY=9259.25
    real, parameter::pi = 3.1415926535
    real, parameter::X_edge = 100.000000  !読み込むデータのX方向の最西座標
    real, parameter::Y_edge = -13.4166670  !読み込むデータのY方向の最南座標
    integer,parameter::NDAY=200,DDT=1200
    integer,parameter::release=1
    integer,parameter::method=1
        !  ***  SliP(SL = 1.0) OR NO - SliP(SL = -1.0) ? *******
    !     SL = -1.0　　　 !境界条件、反射する？
    !     SL = 0.0　　　　!境界条件、停止？
    real,parameter::SL=1.0,EPSH=0.01       !境界条件、そのまま移動する？
    !  ***  SliP CONDITION AT BOTTOM  *****************
    !     BTMSliP = -1.0  !海底に着くと輸送STOP
    real,parameter::BTMSliP=1.0,EPSB=0.01  !海底に着いても底に沿って輸送する
    
    !  ***  PREFECT REFRECTION OR NOT ? **************
    real,parameter::REF=1.0
    !     REF = -1.0
    real,parameter::PDIFH=100.!扩散系数
    real,save:: DD(ll), HH(li, lj, ll), H(li, lj), NX(li, lj), NY(li, lj),mask(li,lj)
end module globals

program pcl_track
use globals
implicit none
real:: U(li, lj, ll), V(li, lj, ll)
integer::IID(NPCL)  
real:: IDSX(NPCL)
real:: RPCLX(NPCL), RPCLY(NPCL)!纬度表示的初始位置
real:: PCLX(NPCL), PCLY(NPCL), PCLZ(NPCL)!距离表示的现在位置
character:: dateyear(NDAY)*8
integer:: i,j,l,k,NI,ip!loop index
	! --------------------------初始设定----------------------------------
	! 读取格子的数据（全部写到一个file就好）
	!装置番号7→各个水层的水深
	open(7, file = '/Volumes/erika/CMEMS/set_input/grid_cmems.dat')
	!装置番号8→各个坐标的实际水深，单个格子的深度
	open(8, file = '/Volumes/erika/CMEMS/set_input/dini_cmems.dat')
	 !装置番号16番→ 最深水深所在的水层    
	 open(9, file = '/Volumes/erika/CMEMS/set_input/nxny_cmems.dat')
	!装置番号9→投入位置


	!読み込み開始
	read(7, *) (DD(l), l = 1, ll) !各水層の水深
	read(8, *) ((H(i, j), i = 1, li), j = 1, lj) !実際水深
	read(8, *) (((HH(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)!格子深度
	read(9, *) ((NX(i, j), i = 1, li), j = 1, lj) !MIROCデータの最大水深
	read(9, *) ((NY(i, j), i = 1, li), j = 1, lj)
	close(7)
	close(8)
	close(9)

	open(10, file = '/Volumes/erika/CMEMS/set_input/start-point/release1.dat')
	do ip = 1, NPCL             
	    read(10, *) IID(ip), RPCLX(ip), RPCLY(ip)
	    call R_P(RPCLX(ip),RPCLY(ip),IDSX(ip),PCLX(ip),PCLY(ip),PCLZ(ip))
	end do
	 close(10)
! ---------------------------------------------------------------

open(11, file = '/Volumes/erika/CMEMS/output/test.dat',FORM='FORMATTED',STATUS='unknown')




end program
 subroutine initial(RPCLX,RPCLY,IDSX,PCLX,PCLY,PCLZ)
 	 ! 用初始纬度计算出离原点的距离
real,intent(in):: RPCLX, RPCLY!纬度表示的初始位置
real,intent(out):: IDSX
real,intent(out):: PCLX, PCLY, PCLZ!距离表示的现在位置
    IDSX=(111.111*1000*cos(RPCLY/360.*2*pi))/12.
    PCLX=(RPCLX-X_edge)*IDSX*12.
    PCLY=(RPCLY-Y_edge)*IDSY*12.
    !PCLZ设定(查文献)
end subroutine initial

subroutine adverage_velocity(PCLX,PCLY,PCLZ,IDSX,IDSY,U,V,PCLU,PCLV,DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ)
real,intent(in)::	PCLX,PCLY,PCLZ,IDSX,IDSY
real,dimension(li,lj,ll),intent(in)::U,V
real,intent(out)::PCLU,PCLV
real,intent(out)::DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ
integer::III,JJJ,LLL
integer:: i,j,k,ip!loop index
real::UC,UE,UN,UNE,US,UUSE,UCL,UEL,UNL,UNEL,USL,USEL
real::UCU,UEU,UNU,UNEU,USU,USEU,VCU,VEU,VNEU,VNU,VWU,VNWU
real::VC,VE,VNE,VN,VW,VNW,VCL,VEL,VNEL,VNL,VWL,VNWL
real::DX,DY,DZ

         III=INT(PCLX/IDSX)+1
         JJJ=INT(PCLY/IDSY)+1
         do  K=1,ll
            if(DD(K).GT.PCLZ)then
                LLL=K
                exit
            end if 
        end do

           
! DX, DYは粒子の格子内水平位置を示した
            DX=MOD(PCLX,IDSX)/IDSX
            DY=MOD(PCLY,IDSY)/IDSY
            IF(LLL.EQ.1) THEN
               DZ=PCLZ/HH(III,JJJ,1)
            ELSE
               DZ=(PCLZ-DD(LLL-1))/HH(III,JJJ,LLL)
            END IF
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
                     DUDX=((DY-0.5)*(UNE-UN)+(1.5-DY)*(UE-UC))/IDSX
                     DUDZ=0.0
                     IF(NX(III,JJJ).LE.1.0) UC=SL*U(III,JJJ+1,LLL)
                     IF(NX(III,JJJ+1).LE.1.0) UN=SL*U(III,JJJ,LLL)
                     IF(NX(III+1,JJJ).LE.1.0) UE=SL*U(III+1,JJJ+1,LLL)
                     IF(NX(III+1,JJJ+1).LE.1.0) UNE=SL*U(III+1,JJJ,LLL)
                     DUDY=(DX*(UNE-UE)+(1.0-DX)*(UN-UC))/IDSX


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
                          ((DY-0.5)*(UNEU-UNU)+(1.5-DY)*(UEU-UCU))*(0.5-DZ))/IDSX
                     IF(NX(III,JJJ).LT.LLL-1) UCU=SL*U(III,JJJ+1,LLL-1)
                     IF(NX(III,JJJ).LE.LLL-1) UC=SL*U(III,JJJ+1,LLL)
                     IF(NX(III,JJJ+1).LT.LLL-1) UNU=SL*U(III,JJJ,LLL-1)
                     IF(NX(III,JJJ+1).LE.LLL-1) UN=SL*U(III,JJJ,LLL)
                     IF(NX(III+1,JJJ).LT.LLL-1) UEU=SL*U(III+1,JJJ+1,LLL-1)
                     IF(NX(III+1,JJJ).LE.LLL-1) UE=SL*U(III+1,JJJ+1,LLL)
                     IF(NX(III+1,JJJ+1).LT.LLL-1) UNEU=SL*U(III+1,JJJ,LLL-1)
                     IF(NX(III+1,JJJ+1).LE.LLL-1) UNE=SL*U(III+1,JJJ,LLL)
                     DUDY=((DX*(UNE-UE)+(1.0-DX)*(UN-UC))*(0.5+DZ)+&
                          (DX*(UNEU-UEU)+(1.0-DX)*(UNU-UCU))*(0.5-DZ))/IDSX
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
                       ((DY-0.5)*(UNEL-UNL)+(1.5-DY)*(UEL-UCL))*(DZ-0.5))/IDSX
                  IF(NX(III,JJJ).LT.LLL) UC=SL*U(III,JJJ+1,LLL)
                  IF(NX(III,JJJ+1).LT.LLL) UN=SL*U(III,JJJ,LLL)
                  IF(NX(III+1,JJJ).LT.LLL) UE=SL*U(III+1,JJJ+1,LLL)
                  IF(NX(III+1,JJJ+1).LT.LLL) UNE=SL*U(III+1,JJJ,LLL)
                  IF(NX(III,JJJ).LE.LLL) UCL=SL*U(III,JJJ+1,LLL+1)
                  IF(NX(III,JJJ+1).LE.LLL) UNL=SL*U(III,JJJ,LLL+1)
                  IF(NX(III+1,JJJ).LE.LLL) UEL=SL*U(III+1,JJJ+1,LLL+1)
                  IF(NX(III+1,JJJ+1).LE.LLL) UNEL=SL*U(III+1,JJJ,LLL+1)
                  DUDY=((DX*(UNE-UE)+(1.0-DX)*(UN-UC))*(1.5-DZ)+&
                       (DX*(UNEL-UEL)+(1.0-DX)*(UNL-UCL))*(DZ-0.5))/IDSX
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
                     DUDX=((0.5-DY)*(UUSE-US)+(DY+0.5)*(UE-UC))/IDSX
                     DUDZ=0.0
                     IF(NX(III,JJJ).LE.0.0) UC=SL*U(III,JJJ-1,LLL)
                     IF(NX(III,JJJ-1).LE.0.0) US=SL*U(III,JJJ,LLL)
                     IF(NX(III+1,JJJ).LE.0.0) UE=SL*U(III+1,JJJ-1,LLL)
                     IF(NX(III+1,JJJ-1).LE.0.0) UUSE=SL*U(III+1,JJJ,LLL)
                     DUDY=(DX*(UE-UUSE)+(1.0-DX)*(UC-US))/IDSX
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
                          ((0.5-DY)*(USEU-USU)+(DY+0.5)*(UEU-UCU))*(0.5-DZ))/IDSX
                     IF(NX(III,JJJ).LE.LLL-1) UC=SL*U(III,JJJ-1,LLL)
                     IF(NX(III,JJJ-1).LE.LLL-1) US=SL*U(III,JJJ,LLL)
                     IF(NX(III+1,JJJ).LE.LLL-1) UE=SL*U(III+1,JJJ-1,LLL)
                     IF(NX(III+1,JJJ-1).LE.LLL-1) UUSE=SL*U(III+1,JJJ,LLL)
                     IF(NX(III,JJJ).LT.LLL-1) UCU=SL*U(III,JJJ-1,LLL-1)
                     IF(NX(III,JJJ-1).LT.LLL-1) USU=SL*U(III,JJJ,LLL-1)
                     IF(NX(III+1,JJJ).LT.LLL-1) UEU=SL*U(III+1,JJJ-1,LLL-1)
                     IF(NX(III+1,JJJ-1).LT.LLL-1) USEU=SL*U(III+1,JJJ,LLL-1)
                     DUDY=((DX*(UE-UUSE)+(1.0-DX)*(UC-US))*(DZ+0.5)+&
                          (DX*(UEU-USEU)+(1.0-DX)*(UCU-USU))*(0.5-DZ))/IDSX
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
                       ((0.5-DY)*(USEL-USL)+(DY+0.5)*(UEL-UCL))*(DZ-0.5))/IDSX
                  IF(NX(III,JJJ).LT.LLL) UC=SL*U(III,JJJ-1,LLL)
                  IF(NX(III,JJJ-1).LT.LLL) US=SL*U(III,JJJ,LLL)
                  IF(NX(III+1,JJJ).LT.LLL) UE=SL*U(III+1,JJJ-1,LLL)
                  IF(NX(III+1,JJJ-1).LT.LLL) UUSE=SL*U(III+1,JJJ,LLL)
                  IF(NX(III,JJJ).LE.LLL) UCL=SL*U(III,JJJ-1,LLL+1)
                  IF(NX(III,JJJ-1).LE.LLL) USL=SL*U(III,JJJ,LLL+1)
                  IF(NX(III+1,JJJ).LE.LLL) UEL=SL*U(III+1,JJJ-1,LLL+1)
                  IF(NX(III+1,JJJ-1).LE.LLL) USEL=SL*U(III+1,JJJ,LLL+1)
                  DUDY=((DX*(UE-UUSE)+(1.0-DX)*(UC-US))*(1.5-DZ)+&
                       (DX*(UEL-USEL)+(1.0-DX)*(UCL-USL))*(DZ-0.5))/IDSX
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


end subroutine adverage_velocity


