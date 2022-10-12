! gfortran PCL_0701.f90 -L/usr/local/Cellar/netcdf/4.8.0_1/lib -lnetcdff

module pcl_track
implicit none
     integer,parameter::li = 640, lj = 522, ll = 50!読み込みグリッド数 
     integer, parameter::NPCL = 1000    !投入粒子数
     real,parameter::IDSY=9259.25
     real, parameter::pi = 3.1415926535
     real, parameter::X_edge = 100.000000  !読み込むデータのX方向の最西座標
     real, parameter::Y_edge = -13.4166670  !読み込むデータのY方向の最南座標
     integer,parameter::NDAY=300,DDT=1200!計算
     real,parameter::PDIFH=100!扩散系数
     real::H(li, lj), NX(li, lj), NY(li, lj),mask(li,lj)
     real:: U(li, lj, ll), V(li, lj, ll)
     real:: IDSX(NPCL)
     real:: RPCLX(NPCL), RPCLY(NPCL)!粒子の経緯度
     real:: PCLX(NPCL), PCLY(NPCL)!粒子
     real:: SEIKIB	
     character:: dateyear(NDAY)*8,file_name*48,NNDAY*3
     character:: datenow*8,year*4,month*2
     integer:: i,j,NI,ip,IID(NPCL)
     integer::III,JJJ,LLL
     real::PCLXO,PCLYO,PCLXS,PCLYS
     real::UC,UE,UN,UNE
     real::VC,VE,VNE,VN
     real::PCLU1,PCLV1,PCLU2,PCLV2,PCLU,PCLV
     real::DX,DY
     real::rad1,rad2
     integer:: NSSS,MAXKAI,NKAI
     NSSS=NDAY*24*3600        
     MAXKAI=INT(NSSS/DDT)
     open(8, file = '/Volumes/erika/CMEMS/set_input/dini_cmems.dat')
     open(9, file = '/Volumes/erika/CMEMS/set_input/nxny_cmems.dat')
     read(8, *) ((H(i, j), i = 1, li), j = 1, lj) !実際水深
     read(9, *) ((NX(i, j), i = 1, li), j = 1, lj) !MIROCデータの最大水深
     read(9, *) ((NY(i, j), i = 1, li), j = 1, lj)
     close(8)
     close(9)
     open(10, file = '/Volumes/erika/CMEMS/set_input/start-point/release1.dat')
     open(11, file = '/Volumes/erika/CMEMS/set_input/start-timing/20180101.txt')

     NI=0
     do IP=1,NPCL
          read(10, *) IID(IP), RPCLX(IP), RPCLY(IP)
          IDSX(IP)=(111.111*1000*cos(RPCLY(IP)/360.*2*pi))/12.
          PCLX(IP)=(RPCLX(IP)-X_edge-1/24.)*IDSX(IP)*12.
          PCLY(IP)=(RPCLY(IP)-Y_edge-1/24.)*IDSY*12.
          ! write(*,'(I6,1x,f11.2,1x,f11.2)')IP,PCLX(IP),PCLY(IP)
     end do
     close(10)

     do NKAI = 1, MAXKAI
          if (NKAI.EQ.MAXKAI) then
               NI=NDAY
          else
               NI=INT(NKAI*DDT/(1*24*3600))+1
               if((NKAI==1).OR.(MOD(NKAI*INT(DDT), 1*24*3600) == 0)) then
                    read(11,'(A8)')dateyear(NI)
                    open(12,file='/Volumes/erika/CMEMS/processed_data/U/U_'//dateyear(NI)//'.dat')
                    open(13,file='/Volumes/erika/CMEMS/processed_data/V/V_'//dateyear(NI)//'.dat')
                    write(*,*)''//dateyear(NI)//'计算中...'

                    read(12,*)((U(i, j),i = 1, li), j = 1, lj)
                    read(13,*)((V(i, j),i = 1, li), j = 1, lj)
                    do j=1,lj
                         do i=1,li
                              if (abs(U(i, j)).GE.5.) U(i,j)=0.
                              if (abs(V(i, j)).GE.5.) V(i,j)=0.
                         
                         end do 
                    end do
               end if
          end if
          close(12)
               close(13)
               ! write(*,'(f11.6)')((U(i, j), i = 1, li), j = 1, lj)
               ! write(*,'(f11.6)')((V(i, j), i = 1, li), j = 1, lj)
               !有-20的异常值
              
          do IP=1,NPCL
               III=INT(PCLX(IP)/IDSX(IP))+1
               JJJ=INT(PCLY(IP)/IDSY)+1
               ! write(*,'(I6,1x,I6,1x,I6)')IP,III,JJJ
               

               !         DX, DYは粒子の格子内水平位置を示した
               DX=MOD((PCLX(IP)),IDSX(IP))/IDSX(IP)
               DY=MOD(REAL(PCLY(IP)),IDSY)/IDSY
               if (III.LT.640 .AND. JJJ.LT.522)then
                    UC=U(III,JJJ)
                    UE=U(III+1,JJJ)
                    UN=U(III,JJJ+1)
                    UNE=U(III+1,JJJ+1)
                
                    VC=V(III,JJJ)
                    VE=V(III+1,JJJ)
                    VNE=V(III+1,JJJ+1)
                    VN=V(III,JJJ+1)

               else
                    UC=U(III,JJJ)
                    UE=0.d0
                    UN=0.d0
                    UNE=0.d0

                    VC=V(III,JJJ)
                    VE=0.d0
                    VNE=0.d0
                    VN=0.d0
               end if
               ! write(*,'(I6,1X,I6,f11.2,1X,f11.2,1X,f11.2,1X,f11.2)')IP,NKAI,UC,UE,UN,UNE
               PCLU=((UC*DX+UE*(1-DX))*DY)+(UN*DX+UNE*(1-DX))*(1.0-DY)
               PCLV=(VC*DX+VE*(1-DX))*DY+(VN*DX+VNE*(1-DX))*(1-DY)
               ! write(*,'(I6,1X,I6,1X,f11.8,1X,f11.8)')IP,NKAI,PCLU,PCLV

               PCLXO=PCLX(IP)
               PCLYO=PCLY(IP)

               PCLX(IP)=PCLXO+DDT*PCLU
               PCLY(IP)=PCLYO+DDT*PCLV

                ! write(*,'(I6,1X,I6,1X,D11.2,1X,D11.2)')IP,NKAI,PCLXO,PCLYO
               subroutine dispersion(PCLX,PCLY)
               ! ! ! ***** EFFECT OF DISPERSION  *****  
               ! ! ! COMPUTING NORMAliZED Ideal NUMBER 
               
               call random_number(SEIKIB)
               rad1=cos(SEIKIB*2*pi)
               rad2=sin(SEIKIB*2*pi)
               PCLXS=PCLX(IP)+rad1*SQRT(2.0*DDT*PDIFH)
               PCLYS=PCLY(IP)+rad2*SQRT(2.0*DDT*PDIFH)
               PCLX(IP)=PCLXS
               PCLY(IP)=PCLYS


               RPCLX(IP)=(PCLX(IP)/(IDSX(IP)*12.))+X_edge
               RPCLY(IP)=(PCLY(IP)/(IDSY*12.))+Y_edge
               IDSX(iP) = (111.111*1000*cos(RPCLY(iP)/360.*2*pi))/12.
               if((NKAI==1).OR.(MOD(NKAI*INT(DDT), 1*24*3600) == 0)) then
               open(14,file='/Volumes/erika/CMEMS/output/test.dat')
               open(15,file='/Volumes/erika/CMEMS/output/20180101/'//dateyear(NI)//'.dat')
               write(14,'(I6,1X,I6,1X,F11.2,1X,F11.2)')&
               NI,IP,RPCLX(IP),RPCLY(IP)
               write(15,'(F6.2,1X,F5.2)')RPCLX(IP),RPCLY(IP)
               write(*,'(I6,1X,I6,1X,F11.2,1x,F11.2)')&
               IP,NI,RPCLX(IP),RPCLY(IP)
               end if
          end do 
     end do
     close(14)
     close(11)
end module pcl_track




