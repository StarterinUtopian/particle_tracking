! gfortran PCL_0921.f90 -L/usr/local/Cellar/netcdf/4.8.0_1/lib -lnetcdff
module globals
     implicit none
     save     
     integer,parameter::li = 640, lj = 522, ll = 2!読み込みグリッド数 
     integer,parameter::NPCL=10000   !投入粒子数
     real,parameter::IDSY=9259.25
     real, parameter::pi = 3.1415926535
     real, parameter::X_edge = 100.000000  !読み込むデータのX方向の最西座標
     real, parameter::Y_edge = -13.4166670  !読み込むデータのY方向の最南座標
     integer,parameter::NSTART=12,NDAY=200,DDT=1200!計算
     real,parameter::PDIFH=100!扩散系数


     contains
     subroutine settings(H,MASK,IDSX,PCLX,PCLY,STARTDATE)

          real,intent(out)::H(li,lj)
          integer,intent(out)::MASK(li,lj,ll)
          real,intent(out),dimension(NPCL)::PCLX,PCLY,IDSX
          character,intent(out),dimension(NSTART)::startdate*8
          integer::i,j,k,IP,YS
          real,dimension(NPCL)::RPCLX,RPCLY
     ! 读格子
          open(7, file = '/Volumes/erika/CMEMS/set_input/grid_cmems.dat')
          read(7, *) ((H(i, j), i = 1, li), j = 1, lj) !実際水深
          read(7,*) (((MASK(i, j, k), i = 1, li), j = 1, lj),k = 1, ll)
          close(7)

     ! 读初始坐标转换成距离
     open(8, file = '/Volumes/erika/CMEMS/set_input/start-point/release1.dat')
     do IP=1,NPCL
          read(8, *) RPCLX(IP), RPCLY(IP)

          IDSX(IP)=(111.111*1000*cos(RPCLY(IP)/360.*2*pi))/12.
          PCLX(IP)=(RPCLX(IP)-X_edge)*IDSX(IP)*12.
          PCLY(IP)=(RPCLY(IP)-Y_edge)*IDSY*12.
          ! write(*,'(I6,1x,f11.2,1x,f11.2)')IP,PCLX(IP),PCLY(IP)
     end do
     close(8)

     ! 读所有初始日期
          open(9, file = '/Volumes/erika/CMEMS/set_input/start-timing/start.txt')
          read(9,'(A8)')(startdate(YS),YS=1,NSTART)
          close(9)

     end subroutine settings

     subroutine readin(dateyear,U,V,S)

          character,intent(in)::dateyear*8
          real,dimension(li,lj,ll),intent(out)::U,V,S
          integer ::i,j,k
          open(11,file='/Volumes/erika/CMEMS/processed_data/U/U_'//dateyear//'.dat')
          read(11,*)(((U(i, j, k),i = 1, li), j = 1, lj),k = 1, ll)
          close(11)
          open(12,file='/Volumes/erika/CMEMS/processed_data/V/V_'//dateyear//'.dat')
          read(12,*)(((V(i, j, k),i = 1, li), j = 1, lj),k = 1, ll)
          close(12)
          open(13,file='/Volumes/erika/CMEMS/processed_data/S/S_'//dateyear//'.dat')
          read(13,*)(((S(i, j, k),i = 1, li), j = 1, lj),k = 1, ll)
          close(13)
     end subroutine readin

     subroutine adverage(U,V,S,PCLX,PCLY,IDSX,NKAI,LLL,PCLU,PCLV,SX,SY)

          real,dimension(li,lj,ll),intent(in)::U, V, S
          real,intent(in)::PCLX,PCLY,IDSX
          integer,intent(in)::NKAI,LLL
          real,intent(out)::PCLU,PCLV
          real,intent(out)::SX,SY

          integer::III,JJJ
          real::UC,UE,UN,UNE,VC,VE,VNE,VN,SC,SE,SNE,SN
          real::DX,DY


               III=INT(PCLX/IDSX)+1
               JJJ=INT(PCLY/IDSY)+1



               UC=U(III,JJJ,LLL)
               UE=U(III+1,JJJ,LLL)
               UN=U(III,JJJ+1,LLL)
               UNE=U(III+1,JJJ+1,LLL)



               VC=V(III,JJJ,LLL)
               VE=V(III+1,JJJ,LLL)
               VNE=V(III+1,JJJ+1,LLL)
               VN=V(III,JJJ+1,LLL)

               SC=V(III,JJJ,LLL)
               SE=V(III+1,JJJ,LLL)
               SNE=V(III+1,JJJ+1,LLL)
               SN=V(III,JJJ+1,LLL)       

               !DX, DYは粒子の格子内水平位置を示した
               DX=MOD(PCLX,IDSX)/IDSX
               DY=MOD(PCLY,real(IDSY))/IDSY

               PCLU=((UC*(1.-DX)+UE*DX)*(1.-DY))+((UN*(1.-DX)+UNE*DX)*DY)
               PCLV=((VC*(1.-DX)+VE*DX)*(1.-DY))+((VN*(1.-DX)+VNE*DX)*DY)
               SX=(1-DY)*(SE-SC)+DY*(SNE-SN)
               SY=(1-DX)*(SN-SC)+DX*(SNE-SE)

     end subroutine adverage

     subroutine current(PCLUN,PCLU0,PCLVN,PCLV0,PCLXC,PCLYC,NKAI)
          real,intent(in)::PCLUN,PCLU0,PCLVN,PCLV0
          integer,intent(in)::NKAI
          real,intent(out)::PCLXC,PCLYC
          real::di_u,di_v
          real::PCLU,PCLV



          di_u=(PCLUN-PCLU0)*DDT/(24*3600)!每次变化多少
          di_v=(PCLVN-PCLV0)*DDT/(24*3600)



          PCLU=PCLU0+di_u*MOD(INT(NKAI*DDT),24*3600)/DDT
          PCLV=PCLV0+di_v*MOD(INT(NKAI*DDT),24*3600)/DDT


          PCLXC=DDT*(PCLU+0.5*di_u)
          PCLYC=DDT*(PCLV+0.5*di_v)

     end subroutine current

     subroutine dispersion(PCLXD,PCLYD)
          ! write(*,'(I6,1X,I6,1X,D11.2,1X,D11.2)')IP,NKAI,PCLXO,PCLYO

          real,intent(out)::PCLXD,PCLYD
          real::SEIKIB,rad1,rad2

          call random_number(SEIKIB)
          rad1=cos(SEIKIB*2*pi)
          rad2=sin(SEIKIB*2*pi)

          PCLXD=rad1*SQRT(2.0*DDT*PDIFH)
          PCLYD=rad2*SQRT(2.0*DDT*PDIFH)

     end subroutine dispersion

     subroutine boundary(H,MASK,PCLX0,PCLY0,PCLXC,PCLXD,PCLXS,PCLYC,PCLYD,PCLYS,PCLX,PCLY,IDSX,LLL)! 判断是否触岸和触底
          real,intent(out)::PCLX,PCLY
          real,intent(in)::PCLX0,PCLY0,PCLXC,PCLXD,PCLXS,PCLYC,PCLYD,PCLYS,IDSX
          real,intent(in),dimension(li,lj)::H
          integer,intent(in),dimension(li,lj,ll)::MASK
          integer,intent(in)::LLL
          integer::III,JJJ

          PCLX=PCLX0+PCLXC+PCLXD+PCLXS
          PCLY=PCLY0+PCLYC+PCLYD+PCLYS
          III=INT(PCLX/IDSX)+1
          JJJ=INT(PCLY/IDSY)+1

          if (H(III,JJJ).LE.0.5)call return(PCLX0,PCLY0,PCLX,PCLY)
          if((III.LE.1).OR.(JJJ.LE.1))call return(PCLX0,PCLY0,PCLX,PCLY)
          if((III.GE.li).OR.(JJJ.GE.lj)) call return(PCLX0,PCLY0,PCLX,PCLY)

     end subroutine boundary

     subroutine return(PCLX0,PCLY0,PCLX,PCLY)

          real,intent(in)::PCLX0,PCLY0
          real,intent(out)::PCLX,PCLY
          PCLX=PCLX0
          PCLY=PCLY0
     end subroutine return

     subroutine output(PCLX,PCLY,NKAI,IP,YI,RPCLX,RPCLY,IDSX0,IDSX,startdate)

          real,intent(in)::PCLX,PCLY
          integer,intent(in)::NKAI,IP,YI
          character,intent(in)::startdate*8
          real,intent(in)::IDSX0
          real,intent(out)::IDSX
          real,intent(out)::RPCLX,RPCLY

          RPCLX=(PCLX/IDSX/12.)+X_edge
          RPCLY=(PCLY/IDSY/12.)+Y_edge
          IDSX= (111.111*1000*cos(RPCLY/360.*2*pi))/12.

          write(14,'(I6,1X,I6,1X,I6,1X,F11.6,1x,F11.6)')&
          NKAI,IP,YI,RPCLX,RPCLY
          write(*,'(I6,1X,I6,1X,I6,1X,F11.6,1x,F11.6)')&
          NKAI,IP,YI,RPCLX,RPCLY
          
     end subroutine output

     subroutine swim(SX0,SY0,SXN,SYN,NKAI,YN,PCLXS,PCLYS)

          real,intent(in)::SX0,SY0,SXN,SYN
          integer,intent(in)::NKAI,YN
          real,intent(out)::PCLXS,PCLYS
          real::di_SX,di_SY
          real::SX,SY
          real::A,TL,GR,TB
          A=0.69
          GR=0.1/365
          TB=0.003
          TL=YN*GR+TB
          di_SX=(SXN-SX0)*DDT/(24*3600)!每次变化多少
          di_SY=(SYN-SY0)*DDT/(24*3600)

          SX=SX0+di_SX*MOD(INT(NKAI*DDT),24*3600)/DDT
          SY=SY0+di_SY*MOD(INT(NKAI*DDT),24*3600)/DDT

          if ((SX.NE.0.).OR.(SY.NE.0))then
               PCLXS=A*TL*SX/SQRT(SX**2+SY**2)
               PCLYS=A*TL*SX/SQRT(SX**2+SY**2)
          end if 
     end subroutine swim







end module


program pcl_track
     use globals
     implicit none

     real,dimension(li,lj)::H
     integer,dimension(li,lj,ll)::MASK
     integer:: NSSS,MAXKAI
     real,dimension(NPCL):: IDSX,RPCLX, RPCLY,PCLX, PCLY!格子宽!粒子的经纬度!离原点距离
     real:: SEIKIB,IDSX0!随机值
     ! LOOP INDEX
     integer:: i,j,k,LLL
     integer::YI,YN,YS,ip
     !CURRENT VELOVITY
     real,dimension(li,lj,ll):: U0, V0, U1, V1!前后速度
     real,dimension(li,lj,ll):: S0, S1!前后盐分
     real::PCLU0,PCLV0,PCLUN,PCLVN!前后平均速度
     real::SX0,SY0,SXN,SYN!前后平均速度
     integer::hour
     real::PCLX0,PCLY0
     real::PCLXS,PCLYS,PCLXC,PCLYC,PCLXD,PCLYD
     integer:: NKAI
     character:: startdate(NSTART)*8,dateyear(NDAY+1)*8


     ! 读格子,初始位置，开始时间
     



     do YS=6,NSTART
          call settings(H,MASK,IDSX,PCLX,PCLY,STARTDATE)
          open(10, file = '/Volumes/erika/CMEMS/set_input/start-timing/'//startdate(YS)//'.txt')
          read(10,'(A8)')(dateyear(YI),YI=1,NDAY+1)
          close(10)
          open(14,file='/Volumes/erika/CMEMS/output/'//startdate(YS)//'.dat')
          NSSS=NDAY*24*3600        
          MAXKAI=INT(NSSS/DDT)
          do NKAI = 0, MAXKAI
               hour=INT((MOD(INT(NKAI*DDT),24*3600)/3600))
               ! 读入速度
               if(MOD(NKAI*DDT, 24*3600) == 0)then
                    ! 求YI
                    if (NKAI==MAXKAI) then
                         YI=NDAY
                    else
                         YI=INT(NKAI*DDT/(24*3600))+1
                         YN=YI+1
                    end if
                    write(*,*)''//dateyear(YI)//'计算中...'


                    if (NKAI==0) call readin(dateyear(1),U0,V0,S0)
                    call readin(dateyear(YN),U1,V1,S1)
                    U0=U1
                    V0=V1
                    S0=S1
               end if
               do IP=1,NPCL  

                    select case(hour)
                    case(0:5,18:23)
                         LLL=2
                    case(6:17)
                         LLL=1
                    end select

                    call adverage(U0,V0,S0,PCLX(IP),PCLY(IP),IDSX(IP),NKAI,LLL,PCLU0,PCLV0,SX0,SY0)

                    ! ! U,V,S,PCLX,PCLY,IDSX,NKAI,LLL,PCLU,PCLV,SX,SY
                    call adverage(U1,V1,S1,PCLX(IP),PCLY(IP),IDSX(IP),NKAI,LLL,PCLUN,PCLVN,SXN,SYN)
                    call current(PCLUN,PCLU0,PCLVN,PCLV0,PCLXC,PCLYC,NKAI)
                    call dispersion(PCLXD,PCLYD)
                    call swim(SX0,SY0,SXN,SYN,NKAI,YI,PCLXS,PCLYS)
                    ! ! SX0,SY0,SXN,SYN,NKAI,YN,PCLXS,PCLYS
                    PCLX0=PCLX(IP)
                    PCLY0=PCLY(IP)
                    call boundary(H,MASK,PCLX0,PCLY0,PCLXC,PCLXD,PCLXS,PCLYC,PCLYD,PCLYS,PCLX(IP),PCLY(IP),IDSX(IP),LLL)
                    IDSX0=IDSX(IP)
                    call output(PCLX(IP),PCLY(IP),NKAI,IP,YI,RPCLX(IP),RPCLY(IP),IDSX0,IDSX(IP),startdate(YS))
               end do
          end do
               close(14)
     end do

end program pcl_track


















