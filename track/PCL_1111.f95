! gfortran PCL_1005.f90 -L/usr/local/Cellar/netcdf/4.8.0_2/lib -lnetcdff
module globals
     implicit none
     save 
     integer,parameter::LN=4320,JN=2041,DN=50    
     integer,parameter::li = 640, lj = 522, ll = 25!読み込みグリッド数 
     integer,parameter::NPCL=10000   !投入粒子数
     real,parameter::IDSY=9259.25
     real, parameter::pi = 3.1415926535
     real, parameter::X_edge = 100.000000  !読み込むデータのX方向の最西座標
     real, parameter::Y_edge = -13.4166670  !読み込むデータのY方向の最南座標
     integer,parameter::NSTART=36,NDAY=250,DDT=1200!計算
     real,parameter::PDIFH=100!扩散系数

     type pcl3D
     real:: X,Y,Z!格子宽!粒子的经纬度!离原点距离
     end type

     type pcl2D
     real::X,Y
     end type

     type phys
     real::U,V,S


     contains

     subroutine count_date(startdate,dateyear)
          character,intent(in)::startdate*8
          character,dimension(NDAY),intent(out)::dateyear*8
          integer,parameter::dall=4924
          character,dimension(dall)::dateall*8
          integer::da   
    open(3,file='/Volumes/erika/CMEMS/set_input/start-timing/dateall.txt')
     read(3,'(A8)')(dateall(da),da=1,dall)
     close(3)

          do da=1,dall
                    if (startdate==dateall(da))then
                         dateyear(:)=dateall(da:(da+NDAY-1))
                    end if
          enddo
     end subroutine count_date
   

     subroutine settings(H,MASK,IDSX,P)

          real,intent(out)::H(li,lj)
          integer,intent(out)::MASK(li,lj,ll),deplev(li,lj)
          TYPE(pcl3D),intent(out),dimension(NPCL)::P
          real,intent(out),dimension(NPCL)::IDSX
 
          integer::i,j,k,IP,YS,da
          TYPE(pcl2D),dimension(NPCL)::RP
     ! 读格子

          open(7, file = '/Volumes/erika/CMEMS/set_input/grid_cmems.dat')
          read(7, *) ((H(i, j), i = 1, li), j = 1, lj) !実際水深
          read(7,*) (((MASK(i, j, k), i = 1, li), j = 1, lj),k = 1, ll)
          read(7,*) (((MASK(i, j, k), i = 1, li), j = 1, lj),k = 1, ll)
          close(7)

     ! 读初始坐标转换成距离
     open(8, file = '/Volumes/erika/CMEMS/set_input/start-point/release1.dat')
     do IP=1,NPCL
          read(8, *) RP(IP)%X, RP(IP)%Y

          IDSX(IP)=(111.111*1000*cos(RP(IP)%Y/360.*2*pi))/12.
          P(IP)%X=(RP(IP)%X-X_edge)*IDSX(IP)*12.
          P(IP)%Y=(RP(IP)%Y-Y_edge)*IDSY*12.
          ! write(*,'(I6,1x,f11.2,1x,f11.2)')IP,P(IP)%X,P(IP)%Y
     end do
     close(8)



     end subroutine settings


     subroutine readin(dateyear,phy0,phy1)

          character,intent(in)::dateyear*8
          real,dimension(li,lj,ll),intent(out)::velo
          real,dimension(LN,JN,DN,1)::Ut,Vt,St
          integer ::ncid,test,rhid,i,j,k,n
          real:: add_offset, scale_factor
          n=0

         
          test=nf_open('/Volumes/CNMES2006-2019/data/'//dateyear(1:4)//'/'//dateyear(5:6)//'/'&
               //dateyear//'.nc',nf_nowrite,ncid)
          call handle_err(test,n)!1
          ! ************读入uo**********

          test=nf_inq_varid(ncid,'uo',rhid)
          call handle_err(test,n)!2
          test=nf_get_var_real(ncid,rhid,Ut)
          call handle_err(test,n)!3
          test=nf_get_att(ncid, rhid, 'add_offset', add_offset)
          call handle_err(test,n)!4
          test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor)
          call handle_err(test,n)!5
          phyU=Ut(3361:4000,800:1321,1:25,1)*scale_factor+add_offset

          test=nf_inq_varid(ncid,'vo',rhid)
          call handle_err(test,n)!2
          test=nf_get_var_real(ncid,rhid,Vt)
          call handle_err(test,n)!3
          test=nf_get_att(ncid, rhid, 'add_offset', add_offset)
          call handle_err(test,n)!4
          test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor)
          call handle_err(test,n)!5
          V(:,:,:)=Vt(3361:4000,800:1321,1:25,1)*scale_factor+add_offset

          test=nf_inq_varid(ncid,'so',rhid)
          call handle_err(test,n)!2
          test=nf_get_var_real(ncid,rhid,St)
          call handle_err(test,n)!3
          test=nf_get_att(ncid, rhid, 'add_offset', add_offset)
          call handle_err(test,n)!4
          test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor)
          call handle_err(test,n)!5
          S(:,:,:)=St(3361:4000,800:1321,1:25,1)*scale_factor+add_offset

          test=nf_close(ncid)
          call handle_err(test,n)!6
     end subroutine readin

     subroutine handle_err(test,n)
     integer::test,n
     n=n+1
     if (test .NE. nf_noerr) then
          write(*,*)n
          write(*,*) nf_strerror(test)
          stop
     endif
end subroutine handle_err

     subroutine adverage(phy,P,IDSX,NKAI,LLL,avg)
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
          real::SEIKIB1,SEIKIB2

          call random_number(SEIKIB1)
          call random_number(SEIKIB2)
         

          PCLXD=SEIKIB1*SQRT(2.0*DDT*PDIFH)
          PCLYD=SEIKIB2*SQRT(2.0*DDT*PDIFH)

     end subroutine dispersion

     subroutine boundary(H,MASK,PCLX,PCLY,IDSX,LLL)! 判断是否触岸和触底
          real,intent(inout)::PCLX,PCLY
          real,intent(in),dimension(li,lj)::H
          integer,intent(in),dimension(li,lj,ll)::MASK
          integer,intent(in)::LLL
          integer::III,JJJ


          III=INT(PCLX/IDSX)+1
          JJJ=INT(PCLY/IDSY)+1

          ! if (HH(III,JJJ).LE.50.0)
          if((III.LE.1).OR.(JJJ.LE.1))call return(PCLX0,PCLY0,PCLX,PCLY)
          if((III.GE.li).OR.(JJJ.GE.lj)) call return(PCLX0,PCLY0,PCLX,PCLY)

     end subroutine boundary

     subroutine return(PCLX0,PCLY0,PCLX,PCLY)

          real,intent(in)::PCLX0,PCLY0
          real,intent(out)::PCLX,PCLY
          PCLX=PCLX0
          PCLY=PCLY0
     end subroutine return

     subroutine output(PCLX,PCLY,￥NKAI,IP,YI,RPCLX,RPCLY,IDSX0,IDSX,startdate)

          real,intent(in)::PCLX,PCLY
          integer,intent(in)::NKAI,IP,YI
          character,intent(in)::startdate*8
          real,intent(in)::IDSX0
          real,intent(out)::IDSX
          real,intent(out)::RPCLX,RPCLY

          RPCLX=(PCLX/IDSX/12.)+X_edge
          RPCLY=(PCLY/IDSY/12.)+Y_edge
          IDSX= (111.111*1000*cos(RPCLY/360.*2*pi))/12.

          write(14,'(F11.6,1x,F11.6,1x,I3)')&
          RPCLX,RPCLY,YI
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
     include  '/usr/local/Cellar/netcdf/4.8.0_2/include/netcdf.inc'
     real,dimension(li,lj)::H
     integer,dimension(li,lj,ll)::MASK
     integer:: NSSS,MAXKAI
     integer::hour,NKAI
     character:: startdate(NSTART)*8,dateyear(NDAY+1)*8
     real:: IDSX,IDSX0!随机值
     ! LOOP INDEX
     integer:: i,j,k,LLL
     integer::YI,YN,YS,ip
     integer,dimension(NPCL)::land
     !CURRENT VELOVITY
     type(phys),dimension(li,lj,ll)::phy0,phy1!U0,U1,V0,V1,S0,S1,T0,T1
     type(phys)::avg0,avg1!PCLU0,PCLV0,PCLUN,PCLVN,S0,SN
     type(pcl3D)::P(NPCL),P0,PN!PCLX,PCLY,PCLX0,PCLY0,PCLXN,PCLYN
     type(pcl2D)::PS,PC,PD!PCLX0,PCLY0,PCLXS,PCLYS,PCLXC,PCLYC,PCLXD,PCLYD
     
     ! 读所有初始日期
     open(9, file = '/Volumes/erika/CMEMS/set_input/start-timing/start.txt')
     read(9,'(A8)')(startdate(YS),YS=1,NSTART)
     close(9)
     ! 读格子,初始位置，开始时间
     call settings(H,MASK,IDSX,P)

     do YS=1,NSTART        
          call count_date(startdate(YS),dateyear)
          open(14,file='/Volumes/erika/CMEMS/output/passive/150/'//startdate(YS)//'.dat')
          NSS
          S=NDAY*24*3600        
          MAXKAI=INT(NSSS/DDT)
     
          do NKAI = 0, MAXKAI
               
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


                    if (NKAI==0) call readin(dateyear(1),phy0)
                    call readin(dateyear(YN),phy1)
                    U0=U1
                    V0=V1
                    S0=S1
               end if

           do IP=1,NPCL      
                hour=INT((MOD(INT(NKAI*DDT),24*3600)/3600))
                    select case(hour)
                    case(0:5,18:23)
                         LLL=18
                    case(6:17)
                         LLL=25
                    end select

                    call adverage(phy0,phy1,P,IDSX(IP),NKAI,avg0,avg1)

                    ! ! U,V,S,PCLX,PCLY,IDSX,NKAI,LLL,PCLU,PCLV,SX,SY
                    call current(avg0,avg1,PC,NKAI)
                    call dispersion(PD)
                    ! call swim(SX0,SY0,SXN,SYN,NKAI,YI,PCLXS,PCLYS)
                    ! ! SX0,SY0,SXN,SYN,NKAI,YN,PCLXS,PCLYS
                    P0=P(IP)
                    PN=P0+C+D+S
                    call boundary(H,MASK,P0,PN,P(IP),IDSX(IP))
                    IDSX0=IDSX(IP)
                    if(MOD(NKAI*DDT, 24*3600) == 0)&
                    call output(P(IP)%X,P(IP)%Y,NKAI,IP,YI,RP(IP)%X,RP(IP)%Y,IDSX0,IDSX(IP),startdate(YS))
               end do
          end do
               close(14)
     end do

end program pcl_track


















