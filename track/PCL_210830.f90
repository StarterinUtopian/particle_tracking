! gfortran PCL_0701.f90 -L/usr/local/Cellar/netcdf/4.8.0_1/lib -lnetcdff


module pcl_track
implicit none
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
real:: DD(ll), HH(li, lj, ll), H(li, lj), NX(li, lj), NY(li, lj)
real:: U(li, lj, ll), V(li, lj, ll), W(li, lj, ll)   
real:: IDSX(NPCL)
real:: RPCLX(NPCL), RPCLY(NPCL)!纬度表示的初始位置
real:: PCLX(NPCL), PCLY(NPCL), PCLZ(NPCL)!距离表示的现在位置
real:: SEIKIB(IJ)	
character:: dateyear(NDAY)*8
character:: datenow*8,year*4,month*2
integer:: i,j,l,k,NI,ip,IID(NPCL)
integer::III,JJJ,LLL,IIIO,JJJO,NIII,NJJJ,mask(li,lj)
real::PCLXO,PCLYO,PCLZO,PCLXS,PCLYS,PCLZS
real::SPCLX,SPCLY,SPCLZ,SPCLXO,SPCLYO,SPCLZO,SPCLXS,SPCLYS,SPCLZS
real::XINTC,YINTC,PCLGRAD
real::UC,UE,UN,UNE,US,UUSE,UCL,UEL,UNL,UNEL,USL,USEL
real::UCU,UEU,UNU,UNEU,USU,USEU,VCU,VEU,VNEU,VNU,VWU,VNWU
real::VC,VE,VNE,VN,VW,VNW,VCL,VEL,VNEL,VNL,VWL,VNWL
real::PCLU,PCLV
real::DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ
real::DX,DY,DZ
integer:: NSSS,MAXKAI,NKAI

subroutine settings
! Purpose:
! To calculate the hypotenuse of a right triangle from the two other sides.
! Record of revisions:
!     Date    Programmer     Description of change
!     ====    ==========     ===================== 
!                shuku           Original code
IMPLICIT NONE
	!読み込む(readする)ファイルを開く(open)
	!装置番号7→各个水层的水深
	open(7, file = '/Volumes/erika/CMEMS/set_input/grid_cmems.dat')
	!装置番号8→各个坐标的实际水深，单个格子的深度
	open(8, file = '/Volumes/erika/CMEMS/set_input/dini_cmems.dat')
     !装置番号16番→ 最深水深所在的水层    
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
do ip = 1, NPCL             
    read(9, *) IID(ip), RPCLX(ip), RPCLY(ip)
    IDSX(ip)=(111.111*1000*cos(RPCLY(ip)/360.*2*pi))/12.
    PCLX(ip)=(RPCLX(ip)-X_edge)*IDSX(ip)*12.
    PCLY(ip)=(RPCLY(ip)-Y_edge)*IDSY*12.
end do
 close(9)
 end subroutine settings

subroutine date
! Purpose:
! To calculate the hypotenuse of a right triangle from the two other sides.
! Record of revisions:
!     Date    Programmer     Description of change
!     ====    ==========     ===================== 
!                shuku           Original code
NSSS=NDAY*24*3600        
MAXKAI=INT(NSSS/DDT)
	open(17, file = '/Volumes/erika/CMEMS/set_input/start-timing/20180101.txt')
	read(17,*) dateyear(1:NDAY)
	close(17)
end subroutine date





! *************************************************************************************************

subroutine readin
! Purpose:
! To calculate the hypotenuse of a right triangle from the two other sides.
! Record of revisions:
!     Date    Programmer     Description of change
!     ====    ==========     ===================== 
!                shuku           Original code
       NI=INT(NKAI*DDT/(1*24*3600))+1
		open(4,file='/Volumes/erika/CMEMS/processed_data/U/U_'//dateyear(NI)//'.dat')
		read(4,*)(((U(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)
		close(4)
		open(5,file='/Volumes/erika/CMEMS/processed_data/V/V_'//dateyear(NI)//'.dat')
		read(5,*) (((V(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)
		close(5)
			
end subroutine readin

subroutine vertical!重写
    !*** CHANGE OF PCLZ ******************************垂直迴游的設定

     if (MOD(NKAI*INT(DDT),24*3600).GT.12*3600) THEN
        do I=1,NPCL
           LAYER0=1.                                                                                                                                      !            |
          do LAYER=2.,28.,1.                                                                                                                              !            |
           IF (abs(TEMP(III,JJJ,LAYER0)-5.0) > abs(TEMP(III,JJJ,LAYER)-5.0)) then   !你自己写一个练练手
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
        end do                                                                                                   
        do I=1,NPCL
          PCLZ(I)=200                                                                                                                                      !!!!!!!!!!!!!!!!!!!!!!!!!
        end do                                                                                                                                             !!!!!!!!!!!!!!!!!!!!!!!!!
    endif
end subroutine vertical


! ****************************************************************
subroutine adverage_velocity

         III=INT(PCLX(I)/IDSX(i))+1
         JJJ=INT(PCLY(I)/IDSY)+1
         do  K=1,ll
            if(DD(K).GT.PCLZ(I))then
                LLL=K
                exit
            end if 
        end do

           
! DX, DYは粒子の格子内水平位置を示した
            DX=MOD((PCLX(i)),IDSX(i))/IDSX(i)
            DY=MOD(REAL(PCLY(I)),IDSY)/IDSY
            IF(LLL.EQ.1) THEN
               DZ=PCLZ(I)/HH(III,JJJ,1)
            ELSE
               DZ=(PCLZ(I)-DD(LLL-1))/HH(III,JJJ,LLL)
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
                     DUDX=((DY-0.5)*(UNE-UN)+(1.5-DY)*(UE-UC))/IDSX(i)
                     DUDZ=0.0
                     IF(mask(III,JJJ).LE.1.0) UC=SL*U(III,JJJ+1,LLL)
                     IF(mask(III,JJJ+1).LE.1.0) UN=SL*U(III,JJJ,LLL)
                     IF(mask(III+1,JJJ).LE.1.0) UE=SL*U(III+1,JJJ+1,LLL)
                     IF(mask(III+1,JJJ+1).LE.1.0) UNE=SL*U(III+1,JJJ,LLL)
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

       END IF
       END IF
       END IF
end subroutine adverage_velocity








subroutine SWIMS      
!  ***** BY XX  *****  
!***** MIGRATION DURATION & OTOLITH INCREMENTS  *****
       DAY=NKAI*DDT/3600./24.
!      IF(NKAI==1)THEN
!      OTODAY(I)=DDT/3600./24.
!      ENDIF

!      IF(TEMP(III, JJJ, LLL)>10.)THEN !水温が10度以下耳石成長が停止
!      OTODAY(I)=OTODAY(I)+1./24.
!      ENDIF

!***** TOTAL LENGTH *****
!      IF(DAY <= META_SEIKIB(i))THEN !変態開始時間平均115.2日、SD 19.6日、変態開始後TL成長停止(Pre - leptocephalus 9d, Leptocephalus 106d)GROWTH_SEIKIB
!      TL=GROWTH_SEIKIB(i)*DAY+3.6 !Linear Equation, 単位(㎜)
!      IF(TL_SEIKIB(i)>100) print*, i, TL_SEIKIB(i)
!      ELSE
!      TL = TL_SEIKIB(i)
!      IF(TL>100) print*, '2.  ', i, TL, TL_SEIKIB(i), GROWTH_SEIKIB(i)
!      ENDIF

!***** SWIMMING SPEED *****
!      TL = 0.4315*DAY + 8.5259 !Linear Equation, 単位(㎜)
!      TL = 64.17816355908316*(1 - 1 * EXP(-0.013830852398122474*(NKAI*DDT / 3600. / 24.))) !Von Bertalanffy, 単位(㎜)
!       IF(DAY<= META_SEIKIB(i))THEN
!        SWIMS=0.
!       ELSE

!       ENDIF

!SWIMMING SPEED
   if (MOD(NKAI*INT(DDT),24*3600).GT.12*3600) then
       SWIMS = 0. !(m/sec)
   else
      SWIMS = Speed/100. !(m/sec)
   endif

end subroutine SWIMS

!*************************************************************************
subroutine distance_0
!
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

end subroutine distance_0

subroutine distance_1
        SPCLXS=PCLU+(PCLU*DUDX+PCLV*DUDY+PCLW*DUDZ)*DDT
        SPCLYS=PCLV+(PCLU*DVDX+PCLV*DVDY+PCLW*DVDZ)*DDT
        SPCLZS=PCLW

        SPCLX=PCLXO+DDT*(SPCLXO+SPCLXS)*0.5D0
        SPCLY=PCLYO+DDT*(SPCLYO+SPCLYS)*0.5D0
        SPCLZ=PCLZO-DDT*(SPCLZO+SPCLZS)*0.5D0

        IF(SPCLX.LT.0.0) SPCLX=0.0
        IF(SPCLY.LT.0.0) SPCLY=0.0
        IF(SPCLZ.LT.0.0) SPCLZ=0.0
!边界判断
        IF(SPCLX.GT.li*IDSX(i)) SPCLX=li*IDSX(i)-1
        IF(SPCLY.GT.lj*IDSY) SPCLY=lj*IDSY-1

        SPCLXS=PCLU+(PCLU*DUDX+PCLV*DUDY)*DDT
        SPCLYS=PCLV+(PCLU*DVDX+PCLV*DVDY)*DDT

        PCLX(IP)=PCLXO+DDT*(SPCLXO+SPCLXS)*0.5D0
        PCLY(IP)=PCLYO+DDT*(SPCLYO+SPCLYS)*0.5D0


 end subroutine distance_1
     
subroutine dispersion
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
end subroutine dispersion


subroutine boundary
  NIII=INT(SPCLX/IDSX(i))+1
  NJJJ=INT(SPCLY/IDSY)+1
   
      IF((ABS(NIII-IIIO).GE.2.0).OR.(ABS(NJJJ-JJJO).GE.2.0))&
      PCLX(I)=99999
      PCLY(I)=99999
      PCLZ(I)=99999
    STOP 'TIME STEP IS TOO LARGE FOR PCL'
! 计算移动路线的斜率pclgrad
      IF((ABS(NJJJ-JJJO).LE.0.5).AND.(ABS(NIII-IIIO).LE.0.5)) call output
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

end subroutine boundary

subroutine output

! !   粒子番号、日数、経度、緯度、水深、輸送水深、
! ! 	塩分、水温、体長、海流速度、遊泳速度、日輪数
! ! 	遊泳速度X、遊泳速度Y
! ! 	2回以後各粒子の結果を出力する

PCLX(ip)=SPCLX
PCLY(ip)=SPCLY
PCLZ(ip)=SPCLZ
IDSX(iP) = (111.111*1000*cos(RPCLY(iP)/360.*2*pi))/12.
RPCLX(iP)=(PCLX(IP)/(IDSX(iP)*12.))+X_edge
RPCLY(iP)=(PCLY(IP)/(IDSY*12.))+Y_edge
write(*,'(I6,1X,I4,1X,F11.2,1x,F11.2)')&
IP,NI,RPCLX(IP),RPCLY(IP)


end subroutine output



end module pcl_track

program main 
use pcl_track
do NKAI = 0, MAXKAI
     if (NKAI==0) then
          call settings
     else
          if (MOD(NKAI*INT(DDT), 1*24*3600) == 0) then 
          call readin
          end if

     do ip=1,NPCL
              !把这段的输出改掉
  if(HH(III,JJJ,LLL).LE.0.D0) .or.&
  ((H(III,JJJ).LE.PCLZ(I)).AND.(BTMSLIP.LT.0.0)).or.&!海底に着くと輸送STOP
  (H(III,JJJ).LE.0.0).or.&!陸に着いたらSTOP
  (III.GE.866.0 .OR. III.LE.1.0 .OR. JJJ.GE.620.0 .OR. JJJ.LE.1.0)&!境界に着くと輸送STOP
  call output
  IF((H(III,JJJ).LE.PCLZ(I)).AND.(BTMSLIP.GT.0.0)) PCLZ(I)=H(III,JJJ)-EPSB
          call adverage_velocity
          call distance_0
          call adverage_velocity
          call distance_1
          call boundary
     enddo 
end do
end program main




