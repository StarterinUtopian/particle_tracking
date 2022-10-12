






























































     
     
     
          
          
                    
               DX=MOD((PCLX(IP)),IDSX(IP))/IDSX(IP)
               DY=MOD(REAL(PCLY(IP)),IDSY)/IDSY
            UC=U(III,JJJ,LLL)
           III=INT(PCLX(IP)/IDSX(IP))+1
           JJJ=INT(PCLY(IP)/IDSY)+1
          !  ***  SliP(SL = 1.0) OR NO - SliP(SL = -1.0) ? *******
          close(4)
          close(5)
          datenow=dateyear(NI)
          month=datenow(5:6)
          open(4,file='/Volumes/erika/CMEMS/processed_data/U/U_'//datenow//'.dat')
          open(5,file='/Volumes/erika/CMEMS/processed_data/V/V_'//datenow//'.dat')
          read(4,*)(((U(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)
          read(5,*) (((V(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)
          year=datenow(1:4)
        NI = NI + 1
     !     BTMSliP = -1.0  !海底に着くと輸送STOP
     !     REF = -1.0
     !     SL = -1.0　　　 !境界条件、反射する？
     !     SL = 0.0　　　　!境界条件、停止？
     !  ***  PREFECT REFRECTION OR NOT ? **************
     !  ***  SliP CONDITION AT BOTTOM  *****************
     !装置番号7→各个水层的水深
     !装置番号8→各个坐标的实际水深，单个格子的深度
     !装置番号9→投入位置
     !読み込み開始
     close(16)
     close(17)
     close(7)
     close(8)
     IDSX(ip)=(111.111*1000*cos(RPCLY(ip)/360.*2*pi))/12.
     integer, parameter::IJ = 2
     integer, parameter::NPCL = 1000    !投入粒子数
     integer,parameter::li = 640, lj = 522, ll = 50 !读入数 
     integer,parameter::LN=4320,DN=2041,JN=50
     integer,parameter::method=1
     integer,parameter::NDAY=150,DDT=1200
     integer,parameter::releas=1
     open(17, file = '/Volumes/erika/CMEMS/set_input/start-timing/20180101.txt')
     open(7, file = '/Volumes/erika/CMEMS/set_input/grid_cmems.dat')
     open(8, file = '/Volumes/erika/CMEMS/set_input/dini_cmems.dat')
     open(9, file = '/Volumes/erika/CMEMS/set_input/start-point/release1.dat')
     PCLX(ip)=(RPCLX(ip)-X_edge)*IDSX(ip)*12.
     PCLY(ip)=(RPCLY(ip)-Y_edge)*IDSY*12.
     PCLZ(ip)=200.0d0
     read(16, *) ((NY(i, j), i = 1, li), j = 1, lj)
     read(17,*) dateyear(1:NDAY)
     read(7, *) (DD(l), l = 1, ll) !各水層の水深
     read(8, *) (((HH(i, j, k), i = 1, li), j = 1, lj), k = 1, ll)!格子深度
     read(8, *) ((H(i, j), i = 1, li), j = 1, lj) !実際水深
     read(9, *) ((NX(i, j), i = 1, li), j = 1, lj) !MIROCデータの最大水深
     real, parameter::pi = 3.1415926535
     real, parameter::X_edge = 100.000000  !読み込むデータのX方向の最西座標
     real, parameter::Y_edge = -13.4166670  !読み込むデータのY方向の最南座標
     real,parameter::BTMSliP=1.0,EPSB=0.01  !海底に着いても底に沿って輸送する
     real,parameter::IDSY=9259.25
     real,parameter::PDIFH=50.0D0!扩散系数
     real,parameter::REF=1.0
     real,parameter::SL=1.0,EPSH=1.0       !境界条件、そのまま移動する？
  
     
     
     
          
                  
                  
                     
                     
                                                                                
                          ((0.5+DX)*(VNU-VCU)+(0.5-DX)*(VNWU-VWU))*(0.5-DZ))/IDSY
                          ((0.5-DY)*(USEU-USU)+(DY+0.5)*(UEU-UCU))*(0.5-DZ))/IDSX(i)
                          ((DX+0.5)*DY*VNU+(DX+0.5)*(1.0-DY)*VCU+&
                          ((DX-0.5)*(DY-0.5)*WNEU+(DX-0.5)*(1.5-DY)*WEU+&
                          ((DX-0.5)*(VNEU-VEU)+(1.5-DX)*(VNU-VCU))*(0.5-DZ))/IDSY
                          ((DX-0.5)*DY*VNEU+(DX-0.5)*(1.0-DY)*VEU+&
                          ((DY-0.5)*(UNEU-UNU)+(1.5-DY)*(UEU-UCU))*(0.5-DZ))/IDSX(i)
                          (0.5-DX)*(1.0-DY)*(VWU-VW)+(0.5-DX)*DY*(VNWU-VNW))&
                          (0.5-DX)*(1.0-DY)*VW+(0.5-DX)*DY*VNW
                          (0.5-DX)*(1.0-DY)*VW+(0.5-DX)*DY*VNW)*(DZ+0.5)+&
                          (0.5-DX)*(1.0-DY)*VWU+(0.5-DX)*DY*VNWU)*(0.5-DZ)
                          (1.0-DX)*(0.5-DY)*(USU-US)+(1.0-DX)*(DY+0.5)*(UCU-UC))&
                          (1.0-DX)*(0.5-DY)*US+(1.0-DX)*(DY+0.5)*UC
                          (1.0-DX)*(0.5-DY)*US+(1.0-DX)*(DY+0.5)*UC)*(DZ+0.5)+&
                          (1.0-DX)*(0.5-DY)*USU+(1.0-DX)*(DY+0.5)*UCU)*(0.5-DZ)
                          (1.0-DX)*(1.5-DY)*(UCU-UC)+(1.0-DX)*(DY-0.5)*(UNU-UN))&
                          (1.0-DX)*(DY-0.5)*UN+(1.0-DX)*(1.5-DY)*UC
                          (1.0-DX)*(DY-0.5)*UN+(1.0-DX)*(1.5-DY)*UC)*(0.5+DZ)+&
                          (1.0-DX)*(DY-0.5)*UNU+(1.0-DX)*(1.5-DY)*UCU)*(0.5-DZ)
                          (1.5-DX)*(1.0-DY)*(VCU-VC)+(1.5-DX)*DY*(VNU-VN))&
                          (1.5-DX)*(1.0-DY)*VC+(1.5-DX)*DY*VN
                          (1.5-DX)*(1.0-DY)*VC+(1.5-DX)*DY*VN)*(DZ+0.5)+&
                          (1.5-DX)*(1.0-DY)*VCU+(1.5-DX)*DY*VNU)*(0.5-DZ)
                          (1.5-DX)*(1.5-DY)*WC+(1.5-DX)*(DY-0.5)*WN)*DZ+&
                          (1.5-DX)*(1.5-DY)*WCU+(1.5-DX)*(DY-0.5)*WNU)*(1.0-DZ)
                          (DX*(DY+0.5)*UEU+DX*(0.5-DY)*USEU+&
                          (DX*(DY-0.5)*UNEU+DX*(1.5-DY)*UEU+&
                          (DX*(UEU-USEU)+(1.0-DX)*(UCU-USU))*(0.5-DZ))/IDSX(i)
                          (DX*(UNEU-UEU)+(1.0-DX)*(UNU-UCU))*(0.5-DZ))/IDSX(i)
                          (DY*(VNEU-VNU)+(1.0-DY)*(VEU-VCU))*(0.5-DZ))/IDSY
                          (DY*(VNU-VNWU)+(1.0-DY)*(VCU-VWU))*(0.5-DZ))/IDSY
                          /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL-1))
                          /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL-1))
                          /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL-1))
                          /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL-1))
                          UNEU=SL*U(III+1,JJJ,LLL-1)
                          UNU=SL*U(III,JJJ,LLL-1)
                          USEU=SL*U(III+1,JJJ,LLL-1)
                          USU=SL*U(III,JJJ,LLL-1)
                          VEU=SL*V(III,JJJ,LLL-1)
                          VNEU=SL*V(III,JJJ+1,LLL-1)
                          VNWU=SL*V(III,JJJ+1,LLL-1)
                          VWU=SL*V(III,JJJ,LLL-1)
                        IF(NX(III+1,JJJ).LT.LLL-1) WEU=SL*W(III,JJJ,LLL-1)
                        IF(NX(III+1,JJJ+1).LT.LLL-1) WNEU=SL*W(III,JJJ+1,LLL-1)
                        IF(NY(III+1,JJJ+1).LT.LLL-1) WNEU=SL*W(III+1,JJJ,LLL-1)
                        IF(NY(III,JJJ+1).LT.LLL-1) WNU=SL*W(III,JJJ,LLL-1)
                        WCU=0.0
                        WEU=0.0
                        WNEU=0.0
                        WNU=0.0
                       ((0.5+DX)*(VNL-VCL)+(0.5-DX)*(VNWL-VWL))*(DZ-0.5))/IDSY
                       ((0.5-DY)*(USEL-USL)+(DY+0.5)*(UEL-UCL))*(DZ-0.5))/IDSX(i)
                       ((DX+0.5)*DY*VNL+(DX+0.5)*(1.0-DY)*VCL+&
                       ((DX-0.5)*(VNEL-VEL)+(1.5-DX)*(VNL-VCL))*(DZ-0.5))/IDSY
                       ((DX-0.5)*DY*VNEL+(DX-0.5)*(1.0-DY)*VEL+&
                       ((DY-0.5)*(UNEL-UNL)+(1.5-DY)*(UEL-UCL))*(DZ-0.5))/IDSX(i)
                       (0.5-DX)*(1.0-DY)*(VW-VWL)+(0.5-DX)*DY*(VNW-VNWL))&
                       (0.5-DX)*(1.0-DY)*VW+(0.5-DX)*DY*VNW)*(1.5-DZ)+&
                       (0.5-DX)*(1.0-DY)*VWL+(0.5-DX)*DY*VNWL)*(DZ-0.5)
                       (1.0-DX)*(0.5-DY)*(US-USL)+(1.0-DX)*(DY+0.5)*(UC-UCL))&
                       (1.0-DX)*(0.5-DY)*US+(1.0-DX)*(DY+0.5)*UC)*(1.5-DZ)+&
                       (1.0-DX)*(0.5-DY)*USL+(1.0-DX)*(DY+0.5)*UCL)*(DZ-0.5)
                       (1.0-DX)*(1.5-DY)*(UC-UCL)+(1.0-DX)*(DY-0.5)*(UN-UNL))&
                       (1.0-DX)*(DY-0.5)*UN+(1.0-DX)*(1.5-DY)*UC)*(1.5-DZ)+&
                       (1.0-DX)*(DY-0.5)*UNL+(1.0-DX)*(1.5-DY)*UCL)*(DZ-0.5)
                       (1.5-DX)*(1.0-DY)*(VC-VCL)+(1.5-DX)*DY*(VN-VNL))&
                       (1.5-DX)*(1.0-DY)*VC+(1.5-DX)*DY*VN)*(1.5-DZ)+&
                       (1.5-DX)*(1.0-DY)*VCL+(1.5-DX)*DY*VNL)*(DZ-0.5)
                       (DX*(DY+0.5)*UEL+DX*(0.5-DY)*USEL+&
                       (DX*(DY-0.5)*UNEL+DX*(1.5-DY)*UEL+&
                       (DX*(UEL-USEL)+(1.0-DX)*(UCL-USL))*(DZ-0.5))/IDSX(i)
                       (DX*(UNEL-UEL)+(1.0-DX)*(UNL-UCL))*(DZ-0.5))/IDSX(i)
                       (DY*(VNEL-VNL)+(1.0-DY)*(VEL-VCL))*(DZ-0.5))/IDSY
                       (DY*(VNL-VNWL)+(1.0-DY)*(VCL-VWL))*(DZ-0.5))/IDSY
                       /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL+1))
                       /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL+1))
                       /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL+1))
                       /(HH(III,JJJ,LLL)+HH(III,JJJ,LLL+1))
                       UNEL=SL*U(III+1,JJJ,LLL+1)
                       UNL=SL*U(III,JJJ,LLL+1)
                       USEL=SL*U(III+1,JJJ,LLL+1)
                       USL=SL*U(III,JJJ,LLL+1)
                       VEL=SL*V(III,JJJ,LLL+1)
                       VNEL=SL*V(III,JJJ+1,LLL+1)
                       VNWL=SL*V(III,JJJ+1,LLL+1)
                       VWL=SL*V(III,JJJ,LLL+1)
                      ELSE!LLL
                     !
                     !
                     !
                     !
                     !
                     DUDX=(((0.5-DY)*(UUSE-US)+(DY+0.5)*(UE-UC))*(DZ+0.5)+&
                     DUDX=(((DY-0.5)*(UNE-UN)+(1.5-DY)*(UE-UC))*(0.5+DZ)+&
                     DUDX=((0.5-DY)*(UUSE-US)+(DY+0.5)*(UE-UC))/IDSX(i)
                     DUDX=((DY-0.5)*(UNE-UN)+(1.5-DY)*(UE-UC))/IDSX(i)
                     DUDY=((DX*(UE-UUSE)+(1.0-DX)*(UC-US))*(DZ+0.5)+&
                     DUDY=((DX*(UNE-UE)+(1.0-DX)*(UN-UC))*(0.5+DZ)+&
                     DUDY=(DX*(UE-UUSE)+(1.0-DX)*(UC-US))/IDSX(i)
                     DUDY=(DX*(UNE-UE)+(1.0-DX)*(UN-UC))/IDSX(i)
                     DUDZ=0.0
                     DUDZ=0.0
                     DUDZ=2*(DX*(DY+0.5)*(UEU-UE)+DX*(0.5-DY)*(USEU-UUSE)+&
                     DUDZ=2*(DX*(DY-0.5)*(UNEU-UNE)+DX*(1.5-DY)*(UEU-UE)+&
                     DVDX=((DY*(VN-VNW)+(1.0-DY)*(VC-VW))*(DZ+0.5)+&
                     DVDX=((DY*(VNE-VN)+(1.0-DY)*(VE-VC))*(DZ+0.5)+&
                     DVDX=(DY*(VN-VNW)+(1.0-DY)*(VC-VW))/IDSY
                     DVDX=(DY*(VNE-VN)+(1.0-DY)*(VE-VC))/IDSY
                     DVDY=(((0.5+DX)*(VN-VC)+(0.5-DX)*(VNW-VW))*(DZ+0.5)+&
                     DVDY=(((DX-0.5)*(VNE-VE)+(1.5-DX)*(VN-VC))*(DZ+0.5)+&
                     DVDY=((0.5+DX)*(VN-VC)+(0.5-DX)*(VNW-VW))/IDSY
                     DVDY=((DX-0.5)*(VNE-VE)+(1.5-DX)*(VN-VC))/IDSY
                     DVDZ=0.0
                     DVDZ=0.0
                     DVDZ=2*((0.5+DX)*DY*(VNU-VN)+(0.5+DX)*(1.0-DY)*(VCU-VC)+&
                     DVDZ=2*((DX-0.5)*DY*(VNEU-VNE)+(DX-0.5)*(1.0-DY)*(VEU-VE)+&
                     ELSE
                     END IF
                     IF((NX(III+1,JJJ).LT.LLL-1).OR.(NX(III+1,JJJ+1).LT.LLL-1))&
                     IF((NX(III+1,JJJ).LT.LLL-1).OR.(NX(III+1,JJJ-1).LT.LLL-1))&
                     IF((NX(III,JJJ).LT.LLL-1).OR.(NX(III,JJJ+1).LT.LLL-1))&
                     IF((NX(III,JJJ).LT.LLL-1).OR.(NX(III,JJJ-1).LT.LLL-1))&
                     IF((NY(III,JJJ).LT.LLL-1).OR.(NY(III+1,JJJ).LT.LLL-1))&
                     IF((NY(III,JJJ).LT.LLL-1).OR.(NY(III-1,JJJ).LT.LLL-1))&
                     IF((NY(III,JJJ+1).LT.LLL-1).OR.(NY(III+1,JJJ+1).LT.LLL-1))&
                     IF((NY(III,JJJ+1).LT.LLL-1).OR.(NY(III-1,JJJ+1).LT.LLL-1))&
                     IF(LLL.EQ.1) THEN
                     IF(NX(III+1,JJJ).EQ.LLL-1) UE=SL*U(III+1,JJJ,LLL-1)
                     IF(NX(III+1,JJJ).EQ.LLL-1) UE=SL*U(III+1,JJJ,LLL-1)
                     IF(NX(III+1,JJJ).LE.0.0) UE=SL*U(III+1,JJJ+1,LLL)
                     IF(NX(III+1,JJJ).LE.0.0) UE=SL*U(III+1,JJJ-1,LLL)
                     IF(NX(III+1,JJJ).LE.LLL-1) UE=SL*U(III+1,JJJ+1,LLL)
                     IF(NX(III+1,JJJ).LE.LLL-1) UE=SL*U(III+1,JJJ-1,LLL)
                     IF(NX(III+1,JJJ).LT.LLL) WE=SL*W(III,JJJ,LLL)
                     IF(NX(III+1,JJJ).LT.LLL-1) UE=0.0
                     IF(NX(III+1,JJJ).LT.LLL-1) UE=0.0
                     IF(NX(III+1,JJJ).LT.LLL-1) UEU=0.0
                     IF(NX(III+1,JJJ).LT.LLL-1) UEU=0.0
                     IF(NX(III+1,JJJ).LT.LLL-1) UEU=SL*U(III+1,JJJ+1,LLL-1)
                     IF(NX(III+1,JJJ).LT.LLL-1) UEU=SL*U(III+1,JJJ-1,LLL-1)
                     IF(NX(III+1,JJJ+1).EQ.LLL-1) UNE=SL*U(III+1,JJJ+1,LLL-1)
                     IF(NX(III+1,JJJ+1).LE.0.0) UNE=SL*U(III+1,JJJ,LLL)
                     IF(NX(III+1,JJJ+1).LE.LLL-1) UNE=SL*U(III+1,JJJ,LLL)
                     IF(NX(III+1,JJJ+1).LT.LLL) WNE=SL*W(III,JJJ+1,LLL)
                     IF(NX(III+1,JJJ+1).LT.LLL-1) UNE=0.0
                     IF(NX(III+1,JJJ+1).LT.LLL-1) UNEU=0.0
                     IF(NX(III+1,JJJ+1).LT.LLL-1) UNEU=SL*U(III+1,JJJ,LLL-1)
                     IF(NX(III+1,JJJ-1).EQ.LLL-1) UUSE=SL*U(III+1,JJJ-1,LLL-1)
                     IF(NX(III+1,JJJ-1).LE.0.0) UUSE=SL*U(III+1,JJJ,LLL)
                     IF(NX(III+1,JJJ-1).LE.LLL-1) UUSE=SL*U(III+1,JJJ,LLL)
                     IF(NX(III+1,JJJ-1).LT.LLL-1) USEU=0.0
                     IF(NX(III+1,JJJ-1).LT.LLL-1) USEU=SL*U(III+1,JJJ,LLL-1)
                     IF(NX(III+1,JJJ-1).LT.LLL-1) UUSE=0.0
                     IF(NX(III,JJJ).EQ.LLL-1) UC=SL*U(III,JJJ,LLL-1)
                     IF(NX(III,JJJ).EQ.LLL-1) UC=SL*U(III,JJJ,LLL-1)
                     IF(NX(III,JJJ).LE.0.0) UC=SL*U(III,JJJ+1,LLL)
                     IF(NX(III,JJJ).LE.0.0) UC=SL*U(III,JJJ-1,LLL)
                     IF(NX(III,JJJ).LE.LLL-1) UC=SL*U(III,JJJ+1,LLL)
                     IF(NX(III,JJJ).LE.LLL-1) UC=SL*U(III,JJJ-1,LLL)
                     IF(NX(III,JJJ).LT.LLL-1) UC=0.0
                     IF(NX(III,JJJ).LT.LLL-1) UC=0.0
                     IF(NX(III,JJJ).LT.LLL-1) UCU=0.0
                     IF(NX(III,JJJ).LT.LLL-1) UCU=0.0
                     IF(NX(III,JJJ).LT.LLL-1) UCU=SL*U(III,JJJ+1,LLL-1)
                     IF(NX(III,JJJ).LT.LLL-1) UCU=SL*U(III,JJJ-1,LLL-1)
                     IF(NX(III,JJJ+1).EQ.LLL-1) UN=SL*U(III,JJJ+1,LLL-1)
                     IF(NX(III,JJJ+1).LE.0.0) UN=SL*U(III,JJJ,LLL)
                     IF(NX(III,JJJ+1).LE.LLL-1) UN=SL*U(III,JJJ,LLL)
                     IF(NX(III,JJJ+1).LT.LLL-1) UN=0.0
                     IF(NX(III,JJJ+1).LT.LLL-1) UNU=0.0
                     IF(NX(III,JJJ+1).LT.LLL-1) UNU=SL*U(III,JJJ,LLL-1)
                     IF(NX(III,JJJ-1).EQ.LLL-1) US=SL*U(III,JJJ-1,LLL-1)
                     IF(NX(III,JJJ-1).LE.0.0) US=SL*U(III,JJJ,LLL)
                     IF(NX(III,JJJ-1).LE.LLL-1) US=SL*U(III,JJJ,LLL)
                     IF(NX(III,JJJ-1).LT.LLL-1) US=0.0
                     IF(NX(III,JJJ-1).LT.LLL-1) USU=0.0
                     IF(NX(III,JJJ-1).LT.LLL-1) USU=SL*U(III,JJJ,LLL-1)
                     IF(NY(III+1,JJJ).EQ.LLL-1) VE=SL*V(III+1,JJJ,LLL-1)
                     IF(NY(III+1,JJJ).LE.0.0) VE=SL*V(III,JJJ,LLL)
                     IF(NY(III+1,JJJ).LE.LLL-1) VE=SL*V(III,JJJ,LLL)
                     IF(NY(III+1,JJJ).LT.LLL-1) VE=0.0
                     IF(NY(III+1,JJJ).LT.LLL-1) VEU=0.0
                     IF(NY(III+1,JJJ).LT.LLL-1) VEU=SL*V(III,JJJ,LLL-1)
                     IF(NY(III+1,JJJ+1).EQ.LLL-1) VNE=SL*V(III+1,JJJ+1,LLL-1)
                     IF(NY(III+1,JJJ+1).LE.0.0) VNE=SL*V(III,JJJ+1,LLL)
                     IF(NY(III+1,JJJ+1).LE.LLL-1) VNE=SL*V(III,JJJ+1,LLL)
                     IF(NY(III+1,JJJ+1).LT.LLL) WNE=SL*W(III+1,JJJ,LLL)
                     IF(NY(III+1,JJJ+1).LT.LLL-1) VNE=0.0
                     IF(NY(III+1,JJJ+1).LT.LLL-1) VNEU=0.0
                     IF(NY(III+1,JJJ+1).LT.LLL-1) VNEU=SL*V(III,JJJ+1,LLL-1)
                     IF(NY(III,JJJ).EQ.LLL-1) VC=SL*V(III,JJJ,LLL-1)
                     IF(NY(III,JJJ).EQ.LLL-1) VC=SL*V(III,JJJ,LLL-1)
                     IF(NY(III,JJJ).LE.0.0) VC=SL*V(III+1,JJJ,LLL)
                     IF(NY(III,JJJ).LE.0.0) VC=SL*V(III-1,JJJ,LLL)
                     IF(NY(III,JJJ).LE.LLL-1) VC=SL*V(III+1,JJJ,LLL)
                     IF(NY(III,JJJ).LE.LLL-1) VC=SL*V(III-1,JJJ,LLL)
                     IF(NY(III,JJJ).LT.LLL-1) VC=0.0
                     IF(NY(III,JJJ).LT.LLL-1) VC=0.0
                     IF(NY(III,JJJ).LT.LLL-1) VCU=0.0
                     IF(NY(III,JJJ).LT.LLL-1) VCU=0.0
                     IF(NY(III,JJJ).LT.LLL-1) VCU=SL*V(III+1,JJJ,LLL-1)
                     IF(NY(III,JJJ).LT.LLL-1) VCU=SL*V(III-1,JJJ,LLL-1)
                     IF(NY(III,JJJ+1).EQ.LLL-1) VN=SL*V(III,JJJ+1,LLL-1)
                     IF(NY(III,JJJ+1).EQ.LLL-1) VN=SL*V(III,JJJ+1,LLL-1)
                     IF(NY(III,JJJ+1).LE.0.0) VN=SL*V(III+1,JJJ+1,LLL)
                     IF(NY(III,JJJ+1).LE.0.0) VN=SL*V(III-1,JJJ+1,LLL)
                     IF(NY(III,JJJ+1).LE.LLL-1) VN=SL*V(III+1,JJJ+1,LLL)
                     IF(NY(III,JJJ+1).LE.LLL-1) VN=SL*V(III-1,JJJ+1,LLL)
                     IF(NY(III,JJJ+1).LT.LLL) WN=SL*W(III,JJJ,LLL)
                     IF(NY(III,JJJ+1).LT.LLL-1) VN=0.0
                     IF(NY(III,JJJ+1).LT.LLL-1) VN=0.0
                     IF(NY(III,JJJ+1).LT.LLL-1) VNU=0.0
                     IF(NY(III,JJJ+1).LT.LLL-1) VNU=0.0
                     IF(NY(III,JJJ+1).LT.LLL-1) VNU=SL*V(III+1,JJJ+1,LLL-1)
                     IF(NY(III,JJJ+1).LT.LLL-1) VNU=SL*V(III-1,JJJ+1,LLL-1)
                     IF(NY(III-1,JJJ).EQ.LLL-1) VW=SL*V(III-1,JJJ,LLL-1)
                     IF(NY(III-1,JJJ).LE.0.0) VW=SL*V(III,JJJ,LLL)
                     IF(NY(III-1,JJJ).LE.LLL-1) VW=SL*V(III,JJJ,LLL)
                     IF(NY(III-1,JJJ).LT.LLL-1) VW=0.0
                     IF(NY(III-1,JJJ).LT.LLL-1) VWU=0.0
                     IF(NY(III-1,JJJ).LT.LLL-1) VWU=SL*V(III,JJJ,LLL-1)
                     IF(NY(III-1,JJJ+1).EQ.LLL-1) VNW=SL*V(III-1,JJJ+1,LLL-1)
                     IF(NY(III-1,JJJ+1).LE.0.0) VNW=SL*V(III,JJJ+1,LLL)
                     IF(NY(III-1,JJJ+1).LE.LLL-1) VNW=SL*V(III,JJJ+1,LLL)
                     IF(NY(III-1,JJJ+1).LT.LLL-1) VNW=0.0
                     IF(NY(III-1,JJJ+1).LT.LLL-1) VNWU=0.0
                     IF(NY(III-1,JJJ+1).LT.LLL-1) VNWU=SL*V(III,JJJ+1,LLL-1)
                     PCLU=(DX*(DY+0.5)*UE+DX*(0.5-DY)*UUSE+&
                     PCLU=(DX*(DY-0.5)*UNE+DX*(1.5-DY)*UE+&
                     PCLU=DX*(DY+0.5)*UE+DX*(0.5-DY)*UUSE+&
                     PCLU=DX*(DY-0.5)*UNE+DX*(1.5-DY)*UE+&
                     PCLV=((DX+0.5)*DY*VN+(DX+0.5)*(1.0-DY)*VC+&
                     PCLV=((DX-0.5)*DY*VNE+(DX-0.5)*(1.0-DY)*VE+&
                     PCLV=(DX+0.5)*DY*VN+(DX+0.5)*(1.0-DY)*VC+&
                     PCLV=(DX-0.5)*DY*VNE+(DX-0.5)*(1.0-DY)*VE+&
                     PCLW=((DX-0.5)*(DY-0.5)*WNE+(DX-0.5)*(1.5-DY)*WE+&
                     PCLW=DZ*WC+(1.0-DZ)*WCU
                     PCLW=DZ*WC+(1.0-DZ)*WCU
                    UN=SL*U(III,JJJ,LLL)
                    UNE=SL*U(III+1,JJJ,LLL)
                    US=SL*U(III,JJJ,LLL)
                    UUSE=SL*U(III+1,JJJ,LLL)
                    VE=SL*V(III,JJJ,LLL)
                    VNE=SL*V(III,JJJ+1,LLL)
                    VNW=SL*V(III,JJJ+1,LLL)
                    VW=SL*V(III,JJJ,LLL)
                  !
                  !
                  !
                  !
                  DUDX=(((0.5-DY)*(UUSE-US)+(DY+0.5)*(UE-UC))*(1.5-DZ)+&
                  DUDX=(((DY-0.5)*(UNE-UN)+(1.5-DY)*(UE-UC))*(1.5-DZ)+&
                  DUDY=((DX*(UE-UUSE)+(1.0-DX)*(UC-US))*(1.5-DZ)+&
                  DUDY=((DX*(UNE-UE)+(1.0-DX)*(UN-UC))*(1.5-DZ)+&
                  DUDZ=2*(DX*(DY+0.5)*(UE-UEL)+DX*(0.5-DY)*(UUSE-USEL)+&
                  DUDZ=2*(DX*(DY-0.5)*(UNE-UNEL)+DX*(1.5-DY)*(UE-UEL)+&
                  DVDX=((DY*(VN-VNW)+(1.0-DY)*(VC-VW))*(1.5-DZ)+&
                  DVDX=((DY*(VNE-VN)+(1.0-DY)*(VE-VC))*(1.5-DZ)+&
                  DVDY=(((0.5+DX)*(VN-VC)+(0.5-DX)*(VNW-VW))*(1.5-DZ)+&
                  DVDY=(((DX-0.5)*(VNE-VE)+(1.5-DX)*(VN-VC))*(1.5-DZ)+&
                  DVDZ=2*((0.5+DX)*DY*(VN-VNL)+(0.5+DX)*(1.0-DY)*(VC-VCL)+&
                  DVDZ=2*((DX-0.5)*DY*(VNE-VNEL)+(DX-0.5)*(1.0-DY)*(VE-VEL)+&
                  ELSE
                  ELSE
                  ELSE
                  ELSE!LLL
                  END IF
                  END IF
                  END IF
                  END IF
                  END IF
                  IF((NX(III+1,JJJ).LT.LLL).AND.(NY(III,JJJ).LT.LLL)) THEN
                  IF((NX(III+1,JJJ).LT.LLL).AND.(NY(III,JJJ+1).LT.LLL)) THEN
                  IF((NX(III+1,JJJ).LT.LLL+1).OR.(NX(III+1,JJJ+1).LT.LLL+1))&
                  IF((NX(III+1,JJJ).LT.LLL+1).OR.(NX(III+1,JJJ-1).LT.LLL+1))&
                  IF((NX(III,JJJ).LT.LLL+1).OR.(NX(III,JJJ+1).LT.LLL+1))&
                  IF((NX(III,JJJ).LT.LLL+1).OR.(NX(III,JJJ-1).LT.LLL+1))&
                  IF((NY(III,JJJ).LT.LLL+1).OR.(NY(III+1,JJJ).LT.LLL+1))&
                  IF((NY(III,JJJ).LT.LLL+1).OR.(NY(III-1,JJJ).LT.LLL+1))&
                  IF((NY(III,JJJ+1).LT.LLL+1).OR.(NY(III+1,JJJ+1).LT.LLL+1))&
                  IF((NY(III,JJJ+1).LT.LLL+1).OR.(NY(III-1,JJJ+1).LT.LLL+1))&
                  IF(LLL.EQ.1) THEN
                  IF(LLL.EQ.1) THEN
                  IF(LLL.EQ.1) THEN
                  IF(LLL.EQ.1) THEN
                  IF(LLL.EQ.NX(III+1,JJJ)) UEL=SL*U(III+1,JJJ,LLL)
                  IF(LLL.EQ.NX(III+1,JJJ)) UEL=SL*U(III+1,JJJ,LLL)
                  IF(LLL.EQ.NX(III+1,JJJ+1)) UNEL=SL*U(III+1,JJJ+1,LLL)
                  IF(LLL.EQ.NX(III+1,JJJ-1)) USEL=SL*U(III+1,JJJ-1,LLL)
                  IF(LLL.EQ.NX(III,JJJ)) UCL=SL*U(III,JJJ,LLL)
                  IF(LLL.EQ.NX(III,JJJ)) UCL=SL*U(III,JJJ,LLL)
                  IF(LLL.EQ.NX(III,JJJ+1)) UNL=SL*U(III,JJJ+1,LLL)
                  IF(LLL.EQ.NX(III,JJJ-1)) USL=SL*U(III,JJJ-1,LLL)
                  IF(LLL.EQ.NY(III+1,JJJ)) VEL=SL*V(III+1,JJJ,LLL)
                  IF(LLL.EQ.NY(III+1,JJJ+1)) VNEL=SL*V(III+1,JJJ+1,LLL)
                  IF(LLL.EQ.NY(III,JJJ)) VCL=SL*V(III,JJJ,LLL)
                  IF(LLL.EQ.NY(III,JJJ)) VCL=SL*V(III,JJJ,LLL)
                  IF(LLL.EQ.NY(III,JJJ+1)) VNL=SL*V(III,JJJ+1,LLL)
                  IF(LLL.EQ.NY(III,JJJ+1)) VNL=SL*V(III,JJJ+1,LLL)
                  IF(LLL.EQ.NY(III-1,JJJ)) VWL=SL*V(III-1,JJJ,LLL)
                  IF(LLL.EQ.NY(III-1,JJJ+1)) VNWL=SL*V(III-1,JJJ+1,LLL)
                  IF(NX(III+1,JJJ).EQ.LLL) UEL=SL*U(III+1,JJJ,LLL)
                  IF(NX(III+1,JJJ).EQ.LLL) UEL=SL*U(III+1,JJJ,LLL)
                  IF(NX(III+1,JJJ).LE.LLL) UEL=SL*U(III+1,JJJ+1,LLL+1)
                  IF(NX(III+1,JJJ).LE.LLL) UEL=SL*U(III+1,JJJ-1,LLL+1)
                  IF(NX(III+1,JJJ).LT.LLL) UE=0.0
                  IF(NX(III+1,JJJ).LT.LLL) UE=0.0
                  IF(NX(III+1,JJJ).LT.LLL) UE=SL*U(III+1,JJJ+1,LLL)
                  IF(NX(III+1,JJJ).LT.LLL) UE=SL*U(III+1,JJJ-1,LLL)
                  IF(NX(III+1,JJJ).LT.LLL) UEL=0.0
                  IF(NX(III+1,JJJ).LT.LLL) UEL=0.0
                  IF(NX(III+1,JJJ+1).EQ.LLL) UNEL=SL*U(III+1,JJJ+1,LLL)
                  IF(NX(III+1,JJJ+1).LE.LLL) UNEL=SL*U(III+1,JJJ,LLL+1)
                  IF(NX(III+1,JJJ+1).LT.LLL) UNE=0.0
                  IF(NX(III+1,JJJ+1).LT.LLL) UNE=SL*U(III+1,JJJ,LLL)
                  IF(NX(III+1,JJJ+1).LT.LLL) UNEL=0.0
                  IF(NX(III+1,JJJ-1).EQ.LLL) USEL=SL*U(III+1,JJJ-1,LLL)
                  IF(NX(III+1,JJJ-1).LE.LLL) USEL=SL*U(III+1,JJJ,LLL+1)
                  IF(NX(III+1,JJJ-1).LT.LLL) USEL=0.0
                  IF(NX(III+1,JJJ-1).LT.LLL) UUSE=0.0
                  IF(NX(III+1,JJJ-1).LT.LLL) UUSE=SL*U(III+1,JJJ,LLL)
                  IF(NX(III,JJJ).EQ.LLL) UCL=SL*U(III,JJJ,LLL)
                  IF(NX(III,JJJ).EQ.LLL) UCL=SL*U(III,JJJ,LLL)
                  IF(NX(III,JJJ).LE.LLL) UCL=SL*U(III,JJJ+1,LLL+1)
                  IF(NX(III,JJJ).LE.LLL) UCL=SL*U(III,JJJ-1,LLL+1)
                  IF(NX(III,JJJ).LT.LLL) UC=0.0
                  IF(NX(III,JJJ).LT.LLL) UC=0.0
                  IF(NX(III,JJJ).LT.LLL) UC=SL*U(III,JJJ+1,LLL)
                  IF(NX(III,JJJ).LT.LLL) UC=SL*U(III,JJJ-1,LLL)
                  IF(NX(III,JJJ).LT.LLL) UCL=0.0
                  IF(NX(III,JJJ).LT.LLL) UCL=0.0
                  IF(NX(III,JJJ+1).EQ.LLL) UNL=SL*U(III,JJJ+1,LLL)
                  IF(NX(III,JJJ+1).LE.LLL) UNL=SL*U(III,JJJ,LLL+1)
                  IF(NX(III,JJJ+1).LT.LLL) UN=0.0
                  IF(NX(III,JJJ+1).LT.LLL) UN=SL*U(III,JJJ,LLL)
                  IF(NX(III,JJJ+1).LT.LLL) UNL=0.0
                  IF(NX(III,JJJ-1).EQ.LLL) USL=SL*U(III,JJJ-1,LLL)
                  IF(NX(III,JJJ-1).LE.LLL) USL=SL*U(III,JJJ,LLL+1)
                  IF(NX(III,JJJ-1).LT.LLL) US=0.0
                  IF(NX(III,JJJ-1).LT.LLL) US=SL*U(III,JJJ,LLL)
                  IF(NX(III,JJJ-1).LT.LLL) USL=0.0
                  IF(NY(III+1,JJJ).EQ.LLL) VEL=SL*V(III+1,JJJ,LLL)
                  IF(NY(III+1,JJJ).LE.LLL) VEL=SL*V(III,JJJ,LLL+1)
                  IF(NY(III+1,JJJ).LT.LLL) VE=0.0
                  IF(NY(III+1,JJJ).LT.LLL) VE=SL*V(III,JJJ,LLL)
                  IF(NY(III+1,JJJ).LT.LLL) VEL=0.0
                  IF(NY(III+1,JJJ+1).EQ.LLL) VNEL=SL*V(III+1,JJJ+1,LLL)
                  IF(NY(III+1,JJJ+1).LE.LLL) VNEL=SL*V(III,JJJ+1,LLL+1)
                  IF(NY(III+1,JJJ+1).LT.LLL) VNE=0.0
                  IF(NY(III+1,JJJ+1).LT.LLL) VNE=SL*V(III,JJJ+1,LLL)
                  IF(NY(III+1,JJJ+1).LT.LLL) VNEL=0.0
                  IF(NY(III,JJJ).EQ.LLL) VCL=SL*V(III,JJJ,LLL)
                  IF(NY(III,JJJ).EQ.LLL) VCL=SL*V(III,JJJ,LLL)
                  IF(NY(III,JJJ).LE.LLL) VCL=SL*V(III+1,JJJ,LLL+1)
                  IF(NY(III,JJJ).LE.LLL) VCL=SL*V(III-1,JJJ,LLL+1)
                  IF(NY(III,JJJ).LT.LLL) VC=0.0
                  IF(NY(III,JJJ).LT.LLL) VC=0.0
                  IF(NY(III,JJJ).LT.LLL) VC=SL*V(III+1,JJJ,LLL)
                  IF(NY(III,JJJ).LT.LLL) VC=SL*V(III-1,JJJ,LLL)
                  IF(NY(III,JJJ).LT.LLL) VCL=0.0
                  IF(NY(III,JJJ).LT.LLL) VCL=0.0
                  IF(NY(III,JJJ+1).EQ.LLL) VNL=SL*V(III,JJJ+1,LLL)
                  IF(NY(III,JJJ+1).EQ.LLL) VNL=SL*V(III,JJJ+1,LLL)
                  IF(NY(III,JJJ+1).LE.LLL) VNL=SL*V(III+1,JJJ+1,LLL+1)
                  IF(NY(III,JJJ+1).LE.LLL) VNL=SL*V(III-1,JJJ+1,LLL+1)
                  IF(NY(III,JJJ+1).LT.LLL) VN=0.0
                  IF(NY(III,JJJ+1).LT.LLL) VN=0.0
                  IF(NY(III,JJJ+1).LT.LLL) VN=SL*V(III+1,JJJ+1,LLL)
                  IF(NY(III,JJJ+1).LT.LLL) VN=SL*V(III-1,JJJ+1,LLL)
                  IF(NY(III,JJJ+1).LT.LLL) VNL=0.0
                  IF(NY(III,JJJ+1).LT.LLL) VNL=0.0
                  IF(NY(III-1,JJJ).EQ.LLL) VWL=SL*V(III-1,JJJ,LLL)
                  IF(NY(III-1,JJJ).LE.LLL) VWL=SL*V(III,JJJ,LLL+1)
                  IF(NY(III-1,JJJ).LT.LLL) VW=0.0
                  IF(NY(III-1,JJJ).LT.LLL) VW=SL*V(III,JJJ,LLL)
                  IF(NY(III-1,JJJ).LT.LLL) VWL=0.0
                  IF(NY(III-1,JJJ+1).EQ.LLL) VNWL=SL*V(III-1,JJJ+1,LLL)
                  IF(NY(III-1,JJJ+1).LE.LLL) VNWL=SL*V(III,JJJ+1,LLL+1)
                  IF(NY(III-1,JJJ+1).LT.LLL) VNW=0.0
                  IF(NY(III-1,JJJ+1).LT.LLL) VNW=SL*V(III,JJJ+1,LLL)
                  IF(NY(III-1,JJJ+1).LT.LLL) VNWL=0.0
                  PCLU=(DX*(DY+0.5)*UE+DX*(0.5-DY)*UUSE+&
                  PCLU=(DX*(DY-0.5)*UNE+DX*(1.5-DY)*UE+&
                  PCLV=((DX+0.5)*DY*VN+(DX+0.5)*(1.0-DY)*VC+&
                  PCLV=((DX-0.5)*DY*VNE+(DX-0.5)*(1.0-DY)*VE+&
               DZ=(PCLZ(IP)-DD(LLL))/HH(III,JJJ,LLL)
               ELSE
               ELSE
               ELSE
               ELSE!DZ
               ELSE!DZ.GE.0.5
               END IF
               END IF
               END IF
               END IF
               IF((NX(III+1,JJJ).LT.LLL).OR.(NX(III+1,JJJ+1).LT.LLL))&
               IF((NX(III+1,JJJ).LT.LLL).OR.(NX(III+1,JJJ-1).LT.LLL))&
               IF((NX(III,JJJ).LT.LLL).OR.(NX(III,JJJ+1).LT.LLL))&
               IF((NX(III,JJJ).LT.LLL).OR.(NX(III,JJJ-1).LT.LLL))&
               IF((NY(III,JJJ).LT.LLL).OR.(NY(III+1,JJJ).LT.LLL))&
               IF((NY(III,JJJ).LT.LLL).OR.(NY(III-1,JJJ).LT.LLL))&
               IF((NY(III,JJJ+1).LT.LLL).OR.(NY(III+1,JJJ+1).LT.LLL))&
               IF((NY(III,JJJ+1).LT.LLL).OR.(NY(III-1,JJJ+1).LT.LLL))&
               IF(DD(K).GT.PCLZ(I)) GOTO 250 
               IF(DY.GE.0.5) THEN
               IF(DZ.LT.0.5) THEN
               IF(DZ.LT.0.5) THEN
               IF(DZ.LT.0.5) THEN
               IF(DZ.LT.0.5) THEN
               IF(LLL.GT.1) THEN
               if(PCLZ(IP).GE.DD(k)) then
               LLL=k
               UCU=U(III,JJJ,LLL-1)
            ELSE!DX
            ELSE!DY
            END IF
            END IF
            IF(DX.GE.0.5) THEN
            IF(DX.GE.0.5) THEN
            IF(DY.GE.0.5) THEN
            IF(HH(III,JJJ,LLL).LE.0.D0) GOTO 300!output
            UCL=U(III,JJJ,LLL+1)
            UE=U(III+1,JJJ,LLL)
            UEL=U(III+1,JJJ,LLL+1)
            UEU=U(III+1,JJJ,LLL-1)
            UN=U(III,JJJ+1,LLL)
            UNE=U(III+1,JJJ+1,LLL)
            UNEL=U(III+1,JJJ+1,LLL+1)
            UNEU=U(III+1,JJJ+1,LLL-1)
            UNL=U(III,JJJ+1,LLL+1)
            UNU=U(III,JJJ+1,LLL-1)
            US=U(III,JJJ-1,LLL)
            USEL=U(III+1,JJJ-1,LLL+1)
            USEU=U(III+1,JJJ-1,LLL-1)
            USL=U(III,JJJ-1,LLL+1)
            USU=U(III,JJJ-1,LLL-1)
            UUSE=U(III+1,JJJ-1,LLL)
            VC=V(III,JJJ,LLL)
            VCL=V(III,JJJ,LLL+1)
            VCU=V(III,JJJ,LLL-1)
            VE=V(III+1,JJJ,LLL)
            VEL=V(III+1,JJJ,LLL+1)
            VEU=V(III+1,JJJ,LLL-1)
            VN=V(III,JJJ+1,LLL)
            VNE=V(III+1,JJJ+1,LLL)
            VNEL=V(III+1,JJJ+1,LLL+1)
            VNEU=V(III+1,JJJ+1,LLL-1)
            VNL=V(III,JJJ+1,LLL+1)
            VNU=V(III,JJJ+1,LLL-1)
            VNW=V(III-1,JJJ+1,LLL)
            VNWL=V(III-1,JJJ+1,LLL+1)
            VNWU=V(III-1,JJJ+1,LLL-1)
            VW=V(III-1,JJJ,LLL)
            VWL=V(III-1,JJJ,LLL+1)
            VWU=V(III-1,JJJ,LLL-1)
            WC=W(III,JJJ,LLL)
            WCU=0.0
            WCU=W(III,JJJ,LLL-1)
            WE=W(III+1,JJJ,LLL)
            WEU=0.0
            WEU=W(III+1,JJJ,LLL-1)
            WN=W(III,JJJ+1,LLL)
            WNE=W(III+1,JJJ+1,LLL)
            WNEU=0.0
            WNEU=W(III+1,JJJ+1,LLL-1)
            WNU=0.0
            WNU=W(III,JJJ+1,LLL-1)
            WNW=W(III-1,JJJ+1,LLL)
            WNWU=0.0
            WNWU=W(III-1,JJJ+1,LLL-1)
            WS=W(III,JJJ-1,LLL)
            WSE=W(III+1,JJJ-1,LLL)
            WSEU=0.0
            WSEU=W(III+1,JJJ-1,LLL-1)
            WSU=0.0
            WSU=W(III,JJJ-1,LLL-1)
            WSW=W(III-1,JJJ-1,LLL)
            WSWU=0.0
            WSWU=W(III-1,JJJ-1,LLL-1)
            WW=W(III-1,JJJ,LLL)
            WWU=0.0
            WWU=W(III-1,JJJ,LLL-1)
           ((DX+0.5)*(0.5+DY)*WCU+(DX+0.5)*(0.5-DY)*WSU+&
           ((DX+0.5)*(DY-0.5)*WNU+(DX+0.5)*(1.5-DY)*WCU+&
           ((DX-0.5)*(0.5+DY)*WEU+(DX-0.5)*(0.5-DY)*WSEU+&
           (0.5-DX)*(0.5-DY)*WSW+(0.5-DX)*(0.5+DY)*WW)*DZ+&
           (0.5-DX)*(0.5-DY)*WSWU+(0.5-DX)*(0.5+DY)*WWU)*(1.0-DZ)
           (0.5-DX)*(1.5-DY)*WW+(0.5-DX)*(DY-0.5)*WNW)*DZ+&
           (0.5-DX)*(1.5-DY)*WWU+(0.5-DX)*(DY-0.5)*WNWU)*(1.0-DZ)
           (1.5-DX)*(0.5-DY)*WS+(1.5-DX)*(0.5+DY)*WC)*DZ+&
           (1.5-DX)*(0.5-DY)*WSU+(1.5-DX)*(0.5+DY)*WCU)*(1.0-DZ)
           do k=1,ll
           ELSE
           IF((H(III,JJJ).LE.PCLZ(I)).AND.(BTMSLIP.GT.0.0)) PCLZ(I)=H(III,JJJ)-EPSB  !海底に沿って輸送する(現在の設定)
           IF((H(III,JJJ).LE.PCLZ(I)).AND.(BTMSLIP.LT.0.0)) GOTO 300  !海底に着くと輸送STOP
           IF(H(III,JJJ).LE.0.0) GOTO 300  !陸に着いたらoutput
          !IF(H(III,JJJ).LE.10.0) GOTO 300  !陸に着いたらSTOP
          call output
          call readin_velocity
          call readin_velocity
          call settings
          end if
          end if
          IDSX(iP) = (111.111*1000*cos(RPCLY(iP)/360.*2*pi))/12.
          if (MOD(NKAI*INT(DDT), 1*24*3600) == 0) then 
          IF(NX(III+1,JJJ).LT.LLL-1) WEU=SL*W(III,JJJ,LLL-1)
          IF(NX(III+1,JJJ-1).LT.LLL-1) WSEU=SL*W(III,JJJ-1,LLL-1)
          IF(NY(III+1,JJJ).LT.LLL-1) WSEU=SL*W(III+1,JJJ,LLL-1)
          IF(NY(III,JJJ).LT.LLL-1) WSU=SL*W(III,JJJ,LLL-1)
          NI=0
          RPCLX(iP)=(PCLX(IP)/(IDSX(iP)*12.))+X_edge
          RPCLY(iP)=(PCLY(IP)/(IDSY*12.))+Y_edge
          WCU=0.0
          WEU=0.0
          WSEU=0.0
          WSU=0.0
         do 200 K=1,ll
         ELSE
         ELSE
         END IF                                                                 
         IF(III.GE.866.0 .OR. III.LE.1.0 .OR. JJJ.GE.620.0 .OR. JJJ.LE.1.0) GOTO 300  !境界に着くと輸送STOP
         IF(NX(III,JJJ).LT.LLL-1) WWU=SL*W(III,JJJ,LLL-1)
         IF(NX(III,JJJ).LT.LLL-1) WWU=SL*W(III,JJJ,LLL-1)
         IF(NX(III,JJJ+1).LT.LLL-1) WNWU=SL*W(III,JJJ+1,LLL-1)
         IF(NX(III,JJJ-1).LT.LLL-1) WSWU=SL*W(III,JJJ-1,LLL-1)
         IF(NY(III,JJJ).LT.LLL-1) WSU=SL*W(III,JJJ,LLL-1)
         IF(NY(III,JJJ+1).LT.LLL-1) WNU=SL*W(III,JJJ,LLL-1)
         IF(NY(III-1,JJJ).LT.LLL-1) WSWU=SL*W(III-1,JJJ,LLL-1)
         IF(NY(III-1,JJJ+1).LT.LLL-1) WNWU=SL*W(III-1,JJJ,LLL-1)
         WCU=0.0
         WCU=0.0
         WNU=0.0
         WNWU=0.0
         WSU=0.0
         WSWU=0.0
         WWU=0.0
         WWU=0.0
        ELSE
        ELSE
        ELSE
        ELSE
        END IF
        IF((NX(III,JJJ).LT.LLL).AND.(NY(III,JJJ+1).LT.LLL)) THEN
        IF(LLL.EQ.1) THEN
        IF(NX(III+1,JJJ).LT.LLL) WE=SL*W(III,JJJ,LLL)
        IF(NX(III+1,JJJ-1).LT.LLL) WSE=SL*W(III,JJJ-1,LLL)
        IF(NY(III+1,JJJ).LT.LLL) WSE=SL*W(III+1,JJJ,LLL)
        IF(NY(III,JJJ).LT.LLL) WS=SL*W(III,JJJ,LLL)
        IF(SPCLX.GT.li*IDSX(iP)) SPCLX=li*IDSX(iP)-1
        IF(SPCLX.LT.0.0) SPCLX=0.0
        IF(SPCLY.GT.lj*IDSY) SPCLY=lj*IDSY-1
        IF(SPCLY.LT.0.0) SPCLY=0.0
        IIIO=INT(PCLX(IP)/(IDSX(iP)))+1
        JJJO=INT(PCLY(IP)/real(IDSY))+1
        PCLW=DZ*WC+(1.0-DZ)*WCU
        PCLW=DZ*WC+(1.0-DZ)*WCU
        PCLX(IP)=PCLX(IP)
        PCLX(IP)=PCLXO+DDT*(SPCLXO+SPCLXS)*0.5D0
        PCLX(IP)=PCLXO+DDT*SPCLXO
        PCLX(IP)=PCLXS
        PCLXO=PCLX(IP)
        PCLY(IP)=PCLY(IP)
        PCLY(IP)=PCLYO+DDT*(SPCLYO+SPCLYS)*0.5D0
        PCLY(IP)=PCLYO+DDT*SPCLYO
        PCLY(IP)=PCLYS
        PCLYO=PCLY(IP)
        SPCLXO=PCLU+(PCLU*DUDX+PCLV*DUDY)*DDT
        SPCLXS=PCLU+(PCLU*DUDX+PCLV*DUDY)*DDT
        SPCLYO=PCLV+(PCLU*DVDX+PCLV*DVDY)*DDT
        SPCLYS=PCLV+(PCLU*DVDX+PCLV*DVDY)*DDT
       ELSE
       ELSE
       ELSE
       END IF
       END IF
       END IF
       END IF
       END IF
       END IF
       END IF
       END IF
       IF((NX(III,JJJ).LT.LLL).AND.(NY(III,JJJ).LT.LLL)) THEN
       IF(DY.GE.0.5) THEN
       IF(LLL.EQ.1) THEN
       IF(LLL.EQ.1) THEN
       IF(NX(III,JJJ).LT.LLL) WW=SL*W(III,JJJ,LLL)
       IF(NX(III,JJJ).LT.LLL) WW=SL*W(III,JJJ,LLL)
       IF(NX(III,JJJ+1).LT.LLL) WNW=SL*W(III,JJJ+1,LLL)
       IF(NX(III,JJJ-1).LT.LLL) WSW=SL*W(III,JJJ-1,LLL)
       IF(NY(III,JJJ).LT.LLL) WS=SL*W(III,JJJ,LLL)
       IF(NY(III,JJJ+1).LT.LLL) WN=SL*W(III,JJJ,LLL)
       IF(NY(III-1,JJJ).LT.LLL) WSW=SL*W(III-1,JJJ,LLL)
       IF(NY(III-1,JJJ+1).LT.LLL) WNW=SL*W(III-1,JJJ,LLL)
       PCLW=((DX+0.5)*(0.5+DY)*WC+(DX+0.5)*(0.5-DY)*WS+&
       PCLW=((DX+0.5)*(DY-0.5)*WN+(DX+0.5)*(1.5-DY)*WC+&
       PCLW=((DX-0.5)*(0.5+DY)*WE+(DX-0.5)*(0.5-DY)*WSE+&
      END IF
      IF(H(III,JJJ).LE.0.D0)THEN
      III=INT(PCLXS/IDSX(iP))+1
      JJJ=INT(PCLYS/IDSY)+1
      PCLXS=PCLX(IP)+SEIKIB(1)*SQRT(2.0*DDT*PDIFH)
      PCLYS=PCLY(IP)+SEIKIB(2)*SQRT(2.0*DDT*PDIFH)
     !      call adverage_velocity
     !      call adverage_velocity
     !      call distance_0
     !      call distance_1
     !      call output
     !      call R_P
     ! do ip=1,NPCL
     ! enddo 
     !装置番号11→出力されるファイル
     !装置番号16番→ 最深水深所在的水层（虽然我也不知道nx和ny究竟有啥区别）
     do iP = 1, NPCL
     else
     end do
     end do     
     end if
     if (NKAI==1) then
     open(11, file = '/Volumes/erika/CMEMS/output/test.dat',FORM='FORMATTED',STATUS='unknown')
     open(16, file = '/Volumes/erika/CMEMS/set_input/nxny_cmems.dat')
     write(*,'(f11.2,1x,f11.2)')U(:,:,:),V(:,:,:)
    ! COMPUTING NORMAliZED Ideal NUMBER 
    subroutine settings
 ! ***** EFFECT OF DISPERSION  *****  
 close(9)
 end subroutine distance_1
!
!
!
!
!
!
!
!
!
!
!
!      
!         DX, DYは粒子の格子内水平位置を示した
!       IF(REF.GT.0.0) !GOTO 600
!       PCLY(IP)=SPCLY
!       PCLZ(IP)=SPCLZ
!      DWDX=(((WC-WW)*(0.5+DY)+(WS-WSW)*(0.5-DY))*DZ+
!      DWDX=(((WE-WC)*(0.5+DY)+(WSE-WS)*(0.5-DY))*DZ+&
!      DWDX=(((WE-WC)*(1.5-DY)+(WNE-WN)*(DY-0.5))*DZ+&
!      DWDX=(((WN-WNW)*(DY-0.5)+(WC-WW)*(1.5-DY))*DZ+
!      DWDY=(((WC-WS)*(DX+0.5)+(WW-WSW)*(0.5-DX))*DZ+
!      DWDY=(((WE-WSE)*(DX-0.5)+(WC-WS)*(1.5-DX))*DZ+&
!      DWDY=(((WN-WC)*(0.5-DX)+(WNW-WW)*(DX+0.5))*DZ+
!      DWDY=(((WNE-WE)*(1.5-DX)+(WN-WC)*(DX-0.5))*DZ+&
!      DWDZ=((DX+0.5)*(DY+0.5)*(WCU-WC)+(DX+0.5)*(0.5-DY)*(WSU-WS)+
!      DWDZ=((DX+0.5)*(DY-0.5)*(WNU-WN)+(DX+0.5)*(1.5-DY)*(WCU-WC)+
!      DWDZ=((DX-0.5)*(DY+0.5)*(WEU-WE)+(DX-0.5)*(0.5-DY)*(WSEU-WSE)+&
!      DWDZ=((DX-0.5)*(DY-0.5)*(WNEU-WNE)+(DX-0.5)*(1.5-DY)*(WEU-WE)+&
!      PCLGRAD=(SPCLY-PCLYO)/(SPCLX-PCLXO)!求梯度tan*
!      PCLGRAD=1.0D-5
!      PCLGRAD=1.0D5
!    &       ((WCU-WSU)*(DX+0.5)+(WWU-WSWU)*(0.5-DX))*(1.0-DZ))/IDS
!    &       ((WCU-WWU)*(0.5+DY)+(WSU-WSWU)*(0.5-DY))*(1.0-DZ))/IDS
!    &       ((WEU-WCU)*(0.5+DY)+(WSEU-WSU)*(0.5-DY))*(1.0-DZ))/IDS
!    &       ((WEU-WCU)*(1.5-DY)+(WNEU-WNU)*(DY-0.5))*(1.0-DZ))/IDS
!    &       ((WEU-WSEU)*(DX-0.5)+(WCU-WSU)*(1.5-DX))*(1.0-DZ))/IDS
!    &       ((WNU-WCU)*(0.5-DX)+(WNWU-WWU)*(DX+0.5))*(1.0-DZ))/IDS
!    &       ((WNU-WNWU)*(DY-0.5)+(WCU-WWU)*(1.5-DY))*(1.0-DZ))/IDS
!    &       (WNEU-WEU)*(1.5-DX)+(WNU-WCU)*(DX-0.5))*(1.0-DZ))/IDS
!    &      (0.5-DX)*(0.5-DY)*(WSWU-WSW)+(0.5-DX)*(DY+0.5)*(WWU-WW))
!    &      (0.5-DX)*(1.5-DY)*(WWU-WW)+(0.5-DX)*(DY-0.5)*(WNWU-WNW))
!    &      (1.5-DX)*(0.5-DY)*(WSU-WS)+(1.5-DX)*(DY+0.5)*(WCU-WC))
!    &      (1.5-DX)*(1.5-DY)*(WCU-WC)+(1.5-DX)*(DY-0.5)*(WNU-WN))&
!    &      /HH(III,JJJ,LLL)
!    &      /HH(III,JJJ,LLL)
!    &      /HH(III,JJJ,LLL)
!    &      /HH(III,JJJ,LLL)
!   800 PCLX(IP)=SPCLX
!   DIII=NIII-IIIO
!   DJJJ=NJJJ-JJJO
!   NIII=INT(SPCLX/IDSX(iP))+1
!   NJJJ=INT(SPCLY/IDSY)+1
!  *****  V-COMPONENT VELOCITY  *****
!  *****  W-COMPONENT VELOCITY  *****
! !
! !
! !  2回以後各粒子の結果を出力する
! !  塩分、水温、体長、海流速度、遊泳速度、日輪数
! !  遊泳速度X、遊泳速度Y
! !   粒子番号、日数、経度、緯度、水深、輸送水深、
! ****************************************************************
! ********************************************************************
! ******************************************************************************
! *************************************************************************************************
! ELSE
! ELSE IF(ABS(SPCLY-PCLYO).LT.1.0D-6) THEN
! END IF
! end subroutine check_land
! gfortran PCL_0701.f90 -L/usr/local/Cellar/netcdf/4.8.0_1/lib -lnetcdff
! IF((ABS(DJJJ).LE.0.5).AND.(ABS(DIII).LE.0.5)) GOTO 800!如果两边都没跑出格子
! IF((ABS(NIII-IIIO).GE.2.0).OR.(ABS(NIII-IIIO).GE.2.0))&
! IF(ABS(SPCLX-PCLXO).LT.1.0D-6) THEN!如果x方向跑的距离十分小
! PCLX(IP)=99999
! PCLY(IP)=99999
! PCLZ(IP)=99999
! subroutine check_land!施工中
!****************************************************************
!****************************************************************
!***********************************************************************
!***********************************************************************
!*************************************************************************
!u_component_velocity
!边界判断
200         CONTINUE
250         LLL=K
call random_number(SEIKIB)
character,save:: datenow*8,year*4,month*2
character,save:: dateyear(NDAY)*8
contains
do ip = 1, NPCL
do NKAI = 1, MAXKAI
end do
end do
end module globals
end module pcl_track
end program main
end subroutine adverage_velocity
end subroutine distance_0
end subroutine output
end subroutine R_P
end subroutine random_current
end subroutine readin_velocity
end subroutine settings
implicit none
integer,save:: i,j,l,k,NI,ip,IID(NPCL)
integer,save:: NSSS,MAXKAI,NKAI
integer,save::DIII,DJJJ
integer,save::III,JJJ,LLL,IIIO,JJJO,NIII,NJJJ,ITRA
IP,NI,RPCLX(IP),RPCLY(IP)
MAXKAI=INT(NSSS/DDT)
module globals
module pcl_track
NSSS=NDAY*24*3600        
program main 
read(9, *) IID(ip), RPCLX(ip), RPCLY(ip), PCLZ(ip)
real,intent(in) :: DD(ll), HH(li, lj, ll), H(li, lj), NX(li, lj), NY(li, lj),mask(li,lj)
real,intent(in):: U(li, lj, ll), V(li, lj, ll), W(li, lj, ll)   
real,save:: IDSX(NPCL)
real,save:: PCLX(NPCL), PCLY(NPCL), PCLZ(NPCL)!距离表示的现在位置
real,save:: RPCLX(NPCL), RPCLY(NPCL)!纬度表示的初始位置
real,save:: SEIKIB(IJ)   
real,save:: WC,WE,WW,WS,WN,WNE,WNW,WSE,WSW
real,save::DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ,DWDX,DWDY,DWDZ
real,save::DX,DY,DZ
real,save::PCLU,PCLV,PCLW
real,save::PCLXO,PCLYO,PCLZO,PCLXS,PCLYS,PCLZS
real,save::SPCLX,SPCLY,SPCLZ,SPCLXO,SPCLYO,SPCLZO,SPCLXS,SPCLYS,SPCLZS
real,save::UC,UE,UN,UNE,US,UUSE,UCL,UEL,UNL,UNEL,USL,USEL
real,save::UCU,UEU,UNU,UNEU,USU,USEU,VCU,VEU,VNEU,VNU,VWU,VNWU
real,save::VC,VE,VNE,VN,VW,VNW,VCL,VEL,VNEL,VNL,VWL,VNWL
real,save::WCU,WEU,WWU,WSU,WNU,WNEU,WNWU,WSEU,WSWU
real,save::XINTC,YINTC,PCLGRAD
SUBROUTINE adverage_velocity
subroutine distance_0
subroutine distance_1
subroutine output
subroutine R_P
subroutine random_current
subroutine readin_velocity
use globals
use pcl_track
write(*,'(I6,1X,I4,1X,F11.2,1x,F11.2)')&