! gfortran PCL_0113.f90 -L/usr/local/Cellar/netcdf/4.8.1/lib -lnetcdff
module globals
     implicit none
     include  '/usr/local/Cellar/netcdf/4.8.1/include/netcdf.inc'
     save
     integer,parameter :: LN=4320,JN=2041,DN=50  
     integer,parameter::li = 640, lj = 522, ll =30!読み込みグリッド数 
     integer,parameter::NPCL=15000  !投入粒子数
     real,parameter::IDSY=9259.25
     real, parameter::pi = 3.1415926535
     real, parameter::X_edge = 100.000000  !読み込むデータのX方向の最西座標
     real, parameter::Y_edge = -13.4166670  !読み込むデータのY方向の最南座標
     integer,parameter::NSTART=156,NDAY=200,DDT=1200!計算
     real,parameter::PDIFH=100!扩散系数

 
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

     subroutine settings(DD,HH,H,MASK,lev,IDSX,PCLX,PCLY,STARTDATE)

          real,intent(out),dimension(ll)::DD
          real,intent(out),dimension(li,lj)::H
          integer,intent(out),dimension(li,lj)::lev
          integer,intent(out),dimension(li,lj,ll)::MASK
          real,intent(out),dimension(li,lj,ll)::HH
          real,intent(out),dimension(NPCL)::PCLX,PCLY,IDSX
          character,intent(out),dimension(NSTART)::startdate*8
          integer::i,j,k,IP,YS,ri,rj
          real,dimension(NPCL)::RPCLX,RPCLY
     ! 读格子
          ! open(3, file = '/Volumes/erika/CMEMS/set_input/grid_coordinates.dat')
          ! read(3,*)(DD(k), k = 1, ll)
          ! read(3,*)(((HH(i, j, k), i = 1, li), j = 1, lj),k = 1, ll)
          ! close(3)

          open(4,file='/Volumes/erika/CMEMS/set_input/grid_mask_bathy.dat')
          read(4,*)((H(i,j), i = 1, li), j = 1, lj)
          read(4,*)((LEV(i,j), i = 1, li), j = 1, lj)
          ! read(4,*)(((MASK(i, j, k), i = 1, li), j = 1, lj),k = 1, ll)
          close(4)
          open(20,file='/Volumes/erika/CMEMS/set_input/mask.dat')
          read(20,*)(((MASK(i,j,k), i = 1, li), j = 1, lj),k=1,ll)
          close(20)


     ! 读初始坐标转换成距离
     open(8, file = '/Volumes/erika/CMEMS/set_input/start-point/release_wide.dat')


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


     ! subroutine readin(dateyear,var0,var1)

     !      character,intent(in)::dateyear*8
     !      character,intent(in)::var0*2
     !      real,dimension(li,lj,ll),intent(out)::var1
     !      real,dimension(LN,JN,DN,1)::vart
     !      integer ::ncid,test,rhid,n
     !      real:: add_offset, scale_factor

     !      test=nf_open('/Volumes/CNMES2006-2019/data/'//dateyear(1:4)//'/'//dateyear(5:6)//'/'&
     !           //dateyear//'.nc',nf_nowrite,ncid)
     !      test=nf_inq_varid(ncid,''//var0//'',rhid)
     !      call handle_err(test,n)!2
     !      test=nf_get_var_real(ncid,rhid,vart)
     !      call handle_err(test,n)!3
     !      test=nf_get_att(ncid, rhid, 'add_offset', add_offset)
     !      call handle_err(test,n)!4
     !      test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor)
     !      call handle_err(test,n)!5
     !      var1(:,:,:)=vart(3361:4000,800:1321,1:25,1)*scale_factor+add_offset
     !      test=nf_close(ncid)
     !      call handle_err(test,n)
     ! end subroutine readin

          subroutine readin(dateyear,U,V,S)

          character,intent(in)::dateyear*8
          real,dimension(li,lj,ll),intent(out)::U,V,S
          integer ::i,j,k
          open(11,file='/Volumes/erika/CMEMS/processed_data/U/U_'//dateyear//'.dat')
          read(11,*)(((U(i, j, k),i = 1, li), j = 1, lj),k = 1, ll)
          close(11)
          ! write(*,*)U
          open(12,file='/Volumes/erika/CMEMS/processed_data/V/V_'//dateyear//'.dat')
          read(12,*)(((V(i, j, k),i = 1, li), j = 1, lj),k = 1, ll)
          close(12)
           ! write(*,*)V
          ! open(13,file='/Volumes/erika/CMEMS/processed_data/S/S_'//dateyear//'.dat')
          ! read(13,*)(((S(i, j, k),i = 1, li), j = 1, lj),k = 1, ll)
          ! close(13)
     end subroutine readin
!      subroutine handle_err(test,n)
!      integer::test,n
!      n=n+1
!      if (test .NE. nf_noerr) then
!           write(*,*)n
!           write(*,*) nf_strerror(test)
!           stop
!      endif
! end subroutine handle_err

subroutine dep_lev(PCLX,PCLY,IDSX,NKAI,H,deplev,LLL)!PCLX(IP),PCLY(IP),IDSX(IP),NKAI,H,deplev,LLL
     real,dimension(li,lj),intent(in)::H
     integer,dimension(li,lj),intent(in)::deplev

     real,intent(in)::PCLX,PCLY,IDSX
     integer,intent(in)::NKAI
     integer,intent(out)::LLL
     integer::hour
     integer::III,JJJ

     III=INT(PCLX/IDSX)+1
     JJJ=INT(PCLY/IDSY)+1
     ! do K=1,ll
     !      if (DD(K).GT.PCLZ)then
     !           LLL=k
     !      end if  
     ! end do 
     ! LLL=18

     hour=INT((MOD(INT(NKAI*DDT),24*3600)/3600))
     select case(hour)
     case(0:5,18:23)
          LLL=min(18,deplev(III,JJJ))
     case(6:17)
          LLL=min(25,deplev(III,JJJ))
     end select


end subroutine dep_lev

     ! subroutine readin(dateyear,U,V,S)

     !      character,intent(in)::dateyear*8
     !      real,dimension(li,lj,ll),intent(out)::U,V,S
     !      integer ::i,j,k
     !      open(11,file='/Volumes/erika/CMEMS/processed_data/U/U_'//dateyear//'.dat')
     !      read(11,*)(((U(i, j, k),i = 1, li), j = 1, lj),k = 1, ll)
     !      close(11)
     !      open(12,file='/Volumes/erika/CMEMS/processed_data/V/V_'//dateyear//'.dat')
     !      read(12,*)(((V(i, j, k),i = 1, li), j = 1, lj),k = 1, ll)
     !      close(12)
     !      open(13,file='/Volumes/erika/CMEMS/processed_data/S/S_'//dateyear//'.dat')
     !      read(13,*)(((S(i, j, k),i = 1, li), j = 1, lj),k = 1, ll)
     !      close(13)
     ! end subroutine readin

     subroutine adverage(U,V,S,PCLX,PCLY,IDSX,NKAI,LLL,PCLU,PCLV,SX,SY)
          !call adverage(U0,V0,S0,PCLX(IP),PCLY(IP),IDSX(IP),NKAI,LLL,PCLU0,PCLV0,SX0,SY0)

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
          ! call current(PCLUN,PCLU0,PCLVN,PCLV0,PCLXC,PCLYC,NKAI)
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

          ! write(*,*)PCLYC


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

     subroutine boundary(H,MASK,LEV,LLL,IDSX,PCLX1,PCLY1,PCLX,PCLY,land) !判断是否触岸和触底
         ! call boundary(H,MASK,deplev,PCLX0,PCLY0,PCLX1,PCLY1,PCLX(IP),PCLY(IP),IDSX0,IDSX(IP))

          real,intent(in)::H(li,lj)
          integer,intent(in)::MASK(li,lj,ll),LLL,LEV(li,lj)
          real,intent(in)::IDSX
          real,intent(in)::PCLX1,PCLY1
          real,intent(out)::PCLX,PCLY
          integer,intent(out)::land
          integer::III,JJJ


          III=INT(PCLX1/IDSX)+1
          JJJ=INT(PCLY1/IDSY)+1
          if(MASK(III,JJJ,LLL)>0.5)LAND=1
          if((MASK(III-1,JJJ,LLL)>0.5).OR.(MASK(III+1,JJJ,LLL)>0.5))LAND=1
          if((MASK(III,JJJ-1,LLL)>0.5).OR.(MASK(III,JJJ+1,LLL)>0.5))LAND=1
          if((H(III,JJJ)<10.).or.(LEV(III,JJJ)<=2))LAND=1
          if((III>=1).and.(III<=li))then
               if ((JJJ>=1).and.(JJJ<=lj))then
                    if(land<0.5)then
                         PCLX=PCLX1
                          PCLY=PCLY1
                     endif 
                end if      
                endif




     end subroutine boundary



     subroutine output(IP,NKAI,YI,PCLX,PCLY,RPCLX,RPCLY,IDSX,land)

          real,intent(in)::PCLX,PCLY
          integer,intent(in)::NKAI,IP,YI
          integer,intent(in)::land
          real,intent(out)::IDSX
          real,intent(out)::RPCLX,RPCLY
          integer::yes(NPCL)

         
          RPCLX=(PCLX/IDSX/12.)+X_edge
          RPCLY=(PCLY/IDSY/12.)+Y_edge
          IDSX= (111.111*1000*cos(RPCLY/360.*2*pi))/12.
           write(*,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')&
          RPCLX,RPCLY,YI,IP,NKAI,land
          write(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')&
          RPCLX,RPCLY,YI,IP,NKAI,land


          ! if(land>0.5)then
          ! write(15,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')&
          ! RPCLX,RPCLY,YI,IP,NKAI,land
          !  end if
          
     end subroutine output



     ! subroutine swim(SX0,SY0,SXN,SYN,NKAI,YN,PCLXS,PCLYS)

     !      real,intent(in)::SX0,SY0,SXN,SYN
     !      integer,intent(in)::NKAI,YN
     !      real,intent(out)::PCLXS,PCLYS
     !      real::di_SX,di_SY
     !      real::SX,SY
     !      real::A,TL,GR,TB
     !      A=0.69
     !      GR=0.1/365
     !      TB=0.003
     !      TL=YN*GR+TB
     !      di_SX=(SXN-SX0)*DDT/(24*3600)!每次变化多少
     !      di_SY=(SYN-SY0)*DDT/(24*3600)

     !      SX=SX0+di_SX*MOD(INT(NKAI*DDT),24*3600)/DDT
     !      SY=SY0+di_SY*MOD(INT(NKAI*DDT),24*3600)/DDT

     !      if ((SX.NE.0.).OR.(SY.NE.0))then
     !           PCLXS=A*TL*SX/SQRT(SX**2+SY**2)
     !           PCLYS=A*TL*SX/SQRT(SX**2+SY**2)
     !      end if 
     ! end subroutine swim


end module


program pcl_track
     use globals
     implicit none
     real,dimension(ll)::DD
     real,dimension(li,lj,ll)::HH
     real,dimension(li,lj)::H
     integer,dimension(li,lj)::LEV
     integer,dimension(li,lj,ll)::MASK
     integer:: NSSS,MAXKAI
     real,dimension(NPCL):: IDSX,RPCLX, RPCLY,PCLX, PCLY!格子宽!粒子的经纬度!离原点距离
     real:: SEIKIB,IDSX0!随机值
     ! LOOP INDEX
     integer:: i,j,k,LLL
     integer::YI,YN,YS,ip
     !CURRENT VELOVITY
     real,dimension(li,lj,ll)::U,V,U0, V0, U1, V1!前后速度
     real,dimension(li,lj,ll)::S,S0,S1!前后盐分
     real::PCLU0,PCLV0,PCLUN,PCLVN!前后平均速度
     real::SX0,SY0,SXN,SYN!前后平均速度
     integer::hour
     real::PCLX0,PCLY0,PCLX1,PCLY1
     real::PCLXS,PCLYS,PCLXC,PCLYC,PCLXD,PCLYD
     integer:: NKAI
     integer::land(NPCL)=0
     character:: startdate(NSTART)*8,dateyear(NDAY+1)*8


     ! 读格子,初始位置，开始时间
     


          NSSS=NDAY*24*3600        
          MAXKAI=INT(NSSS/DDT)
          !H,MASK,deplev,IDSX,PCLX,PCLY,STARTDATE
 
 do da=1,dall!现在是循环所有天数
          ! do YS=1,NSTART  
               do YS=1,1  
               land(:)=0
               call settings(DD,HH,H,MASK,lev,IDSX,PCLX,PCLY,STARTDATE)
               call count_date(startdate(YS),dateyear)
               open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='replace')
               ! open(15,file='/Volumes/erika/CMEMS/output/land/'//startdate(YS)//'.dat',status='replace')
          

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
                         call readin(dateyear(YI),U,V,S)
                         ! call readin(dateyear(YI),'uo',U)
                         ! call readin(dateyear(YI),'vo',V)
                         ! call readin(dateyear(YI),'so',S)
                         if (NKAI==0) then 
                              U0=U
                              V0=V
                              S0=S
                         else 
                              U0=U1
                              V0=V1
                              S0=S1
                              U1=U
                              V1=V
                              S1=S
                         end if
               end if

           do IP=1,NPCL   !着陆开关   
                    call dep_lev(PCLX(IP),PCLY(IP),IDSX(IP),NKAI,H,lev,LLL)
                    call adverage(U0,V0,S0,PCLX(IP),PCLY(IP),IDSX(IP),NKAI,LLL,PCLU0,PCLV0,SX0,SY0)
                    ! ! U,V,S,PCLX,PCLY,IDSX,NKAI,LLL,PCLU,PCLV,SX,SY
                    call adverage(U1,V1,S1,PCLX(IP),PCLY(IP),IDSX(IP),NKAI,LLL,PCLUN,PCLVN,SXN,SYN)
                    call current(PCLUN,PCLU0,PCLVN,PCLV0,PCLXC,PCLYC,NKAI)
                    ! call dispersion(PCLXD,PCLYD)
                    ! call swim(SX0,SY0,SXN,SYN,NKAI,YI,PCLXS,PCLYS)
                    ! ! SX0,SY0,SXN,SYN,NKAI,YN,PCLXS,PCLYS
                    PCLX0=PCLX(IP)
                    PCLY0=PCLY(IP)
                    PCLX1=PCLX0+PCLXC+PCLXD+PCLXS
                    PCLY1=PCLY0+PCLYC+PCLYD+PCLYS
                    call boundary(H,MASK,LEV,LLL,IDSX(IP),PCLX1,PCLY1,PCLX(IP),PCLY(IP),land(IP))
                     if(MOD(NKAI*DDT, 24*3600) == 0)then
                    call output(IP,NKAI,YI,PCLX(IP),PCLY(IP),RPCLX(IP),RPCLY(IP),IDSX(IP),land(IP))!PCLX,PCLY,NKAI,IP,YI,RPCLX,RPCLY,IDSX,land
                    end if
          end do
     end do
               close(14)
               ! close(15)
     end do

end program pcl_track


















