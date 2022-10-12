! 用来画产卵场和到达地关系的，导出两百天后的位置
program spawn_reach
	implicit none
	integer,parameter::li = 640, lj = 522, ll = 30
	integer,parameter::NPCL=7500
	real,parameter::IDSY=9259.25
	 real, parameter::pi = 3.1415926535
     real, parameter::X_edge = 100.000000  !読み込むデータのX方向の最西座標
     real, parameter::Y_edge = -13.4166670   !読み込むデータのY方向の最南座標
     integer,parameter::NSTART=156,NDAY=200,DDT=1200
     integer,parameter::dall=4924!計算
     real,parameter::PDIFH=100!扩散系数
     real,parameter::lon0=120.,lat0=5.,lon1=144.9,lat1=24.9,dlon=0.1,dlat=0.1
	character::startdate(NSTART)*8,date*8,year*4,month*2,dateall(dall)*8,dateyear(NDAY+1)*8,NN*3
	real::lon(li),lat(lj),lon2(li),lat2(lj),RPCLX1,RPCLY1,RPCLX2,RPCLY2,spawnlon(150),spawnlat(50),slon(NPCL),slat(NPCL)
	real::TC,TE,TNE,TN,DX,DY,PCLT,T(li,lj,ll),IDSX,reach_rate
	integer::i,j,k,II,JJ,KK,III,JJJ,LLL
	integer::YS,N,P,PP,YI,NKAI,area,IP,IP1,IP2,s,r
	integer::reach(NPCL,NSTART),num(li,lj,NSTART),spawn_num(150,50,NSTART),land(NPCL,NDAY+1,NSTART),land0
	integer::time_area_num(3,NSTART),time_area_switch(NPCL,3,NSTART)
	integer::spawn_reach_num(6,3,NSTART),seashore_area(li,lj)
	integer::seashore(li,lj),eel(NPCL,NDAY+1,NSTART),num_reach(NSTART),reach_area(NPCL,NDAY+1,NSTART),spawn_area(NPCL)
	integer::density_reach(li,lj,NDAY+1)
	integer::da


	! ================================变量说明=================================================================
	! eel鳗鱼种类，1=marmorata 2=japonica
	! 到达地点
	! reach 200天内有无到达过，land，在这一天是否到达
	! =======================================================================================================

	! =====================================文件编号和说明===================================================
	! 7输入文件，模型网格点
	! 8输入文件，海岸区域
	! 9输入文件，所有开始日期
	! 10输入文件，每个粒子的释放位置
	! 11输入文件，水温
	! 15输出文件，所有可以到达的粒子的所有位置(现在是按天算),像四库全书一样啥都有的
	! 16输出文件，到达后的密度
	! 17能到达粒子的产卵场分布

	! =====================================================================================================
open(3,file='/Volumes/erika/CMEMS/set_input/start-timing/dateall.txt')
read(3,'(A8)')(dateall(da),da=1,dall)
close(3)
! 打开格子点
open(7,file='/Volumes/erika/CMEMS/set_input/grid_lonlat.dat',status='old')
read(7,*)((lon(i),lat(j),i=1,li),j=1,lj)
close(7)
! 打开判断是否着岸的文件
open(8,file='/Volumes/erika/CMEMS/set_input/shore.dat',status='old')
read(8,*)((lon(i),lat(j),seashore(i,j),i=1,li),j=1,lj)
close(8)

open(9, file ='/Volumes/erika/CMEMS/set_input/start-timing/start.txt',status='old')
read(9,'(A8)')(startdate(YS),YS=1,NSTART)
close(9)


reach(:,:)=0

! !==================================读入所有的粒子数===========================================
! !所有的天数都数完再输出 
! open(10,file='/Volumes/erika/CMEMS/set_input/start-point/release2.dat',status='old')
! do P=1,NPCL
! 	read(10,'(I6,1x,f11.2,1x,f11.2)')IP2,RPCLX2,RPCLY2	
! 	slon(p)=RPCLX2
! 	slat(p)=RPCLY2
! 	if(reach(IP2,YS)==1)then
! 		II=(RPCLX2-lon0)/dlon+1
! 		JJ=(RPCLY2-lat0)/dlat+1
! 		spawn_num(II,JJ,YS)=spawn_num(II,JJ,YS)+1
! 	end if		
! end do
! close(10)
! ========================================================================================================

! do YS=1,156
! do YS=1,156
	
! 	! 打开读取的文件
! 	open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')


! 	! do da=1,dall
! 	! 	if (startdate(YS)==dateall(da))then
! 	! 		dateyear(:)=dateall(da:(da+NDAY-1))
! 	! 	end if
! 	! enddo

! ! ============================下面开始数到达的密度==================================================
! ! 在两个循环后再输出,按年和月统计，并制作到达粒子的IP—list
! do N=1,NDAY+1
! 	! open(11,file='/Volumes/erika/CMEMS/processed_data/T/T_'//dateyear(N)//'.dat',status='old')
! 	! read(11,*)(((T(i,j,k),i=1,li),j=1,lj),k=1,ll)
! 	! close(11)
! 	do P=1,NPCL
! 		read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX1,RPCLY1,YI,IP1,NKAI,land0
! 		! 先算出iii,jjj
! 		III=INT((RPCLX1-X_edge)*12)+1
! 		JJJ=INT((RPCLY1-Y_edge)*12)+1
! 		! write(*,*)III,JJJ
! 		! ======================读入水温，判断是哪种鳗鱼===================================

!         ! if((III<li).and.(JJJ<lj))then
!         !        TC=T(III,JJJ,LLL)
!         !        TE=T(III+1,JJJ,LLL)
!         !        TNE=T(III+1,JJJ+1,LLL)
!         !        TN=T(III,JJJ+1,LLL)       

!         !        !DX, DYは粒子の格子内水平位置を示した
               
!         !        DX=(RPCLX1-lon(III))/(1./12.)
!         !        DY=(RPCLY1-lat(JJJ))/(1./12.)

!         !        PCLT=((TC*(1.-DX)+TE*DX)*(1.-DY))+((TN*(1.-DX)+TNE*DX)*DY)
!         !        if((PCLT>24.).and.(PCLT<50.))then
!         !        	eel(P,N,YS)=1
!         !        else if(PCLT<24.)then
!         !        	eel(P,N,YS)=2
!         !        end if


! 		! =============================================================================
! 		if(seashore(iii,jjj)==1)then
! 			! 先判断出所有时间里的可以到达的粒子
! 			num(III,JJJ,YS)=num(III,JJJ,YS)+1!统计到达的粒子数
! 			reach(P,YS)=1

! 			land(P,N,YS)=1
! 		else
! 			land(P,N,YS)=0
! 		end if
! 		write(*,*)reach(P,YS)
    
! 	end do
! end do
! end do
! open(11,file='/Volumes/erika/CMEMS/set_input/reach.txt',status='replace')
! write(11,'(I6)')((reach(P,YS),P=1,NPCL),YS=1,NSTART)
! write(11,'(I6)')(((land(P,N,YS),P=1,NPCL),N=1,NDAY+1),YS=1,NSTART)
! close(11)

! ==============================================================================================



! ======================================输出文件15==================================================================
open(11,file='/Volumes/erika/CMEMS/set_input/reach.txt',status='old')
read(11,'(I6)')((reach(P,YS),P=1,NPCL),YS=1,NSTART)!这粒子最后能到么？
! 如果能到，算进到达率里
read(11,'(I6)')(((land(P,N,YS),P=1,NPCL),N=1,NDAY+1),YS=1,NSTART)!这粒子现在到了么？
! 在第几天到的，位置
close(11)
! ===========================算到达率==============================================
! do YS=1,NSTART
!    do P=1,NPCL
!     if (reach(P,YS)==1)then
!     num_reach(YS)=num_reach(YS)+1
!     end if
!    end do
! end do
! open(12,file='/Volumes/erika/CMEMS/set_input/reach_rate2.csv',status='replace')
! do YS=1,NSTART
! date=startdate(YS)
! year=date(1:4)
! month=date(5:6)
! reach_rate=num_reach(YS)/7500.

! write(12,'(A8,1X,F11.6)')startdate(YS),reach_rate
! write(*,*)startdate(YS),reach_rate
! end do
! close(12)
! ============================================================================

! ===========================================计算到达粒子的密度==============================================================
! ! 打开输出文件



! ! open(15,file='/Volumes/erika/CMEMS/output/land/land_all.txt',status='replace')!可以到达的粒子输出trajectory
! ! write(15,'(A3,1X,A3,1X,A3,1X,A2,1X,A4,1X,A5,1X,A3,1X,A4,1X,A5,1X,A4,1X,A4)')&
! ! 'lon','lat','day','IP','land','reach','eel','year','month','slon','slat'

! open(15,file='/Volumes/erika/CMEMS/output/land/land_'//startdate(YS)//'.txt',status='replace')!可以到达的粒子输出trajectory
! write(15,'(A3,1X,A3,1X,A3,1x,A4)')'lon','lat','day'
! open(16,file='/Volumes/erika/CMEMS/output/land/land_density.txt',status='replace')!到达部分的密度
! write(16,'(A3,1X,A3,1X,A3,1X,A4,1X,A5)')'lon','lat','num','year','month'

! open(17,file='/Volumes/erika/CMEMS/output/land/land_particle_spawn_density.txt',status='replace')
! write(17,'(A3,1X,A3,1X,A3,1X,A4,1X,A2)')'lon','lat','num','year','month'

! do YS=1,156
! ! do YS=1,1
! ! 	! 打开读取的文件
! ! 	write(*,*)startdate(YS)
! 	open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
! 	open(16,file='/Volumes/erika/CMEMS/output/land/land_density'//startdate(YS)//'.txt',status='replace')!到达部分的密度
!        write(16,'(A3,1X,A3,1X,A3)')'lon','lat','num'
       
! 	date=startdate(YS)
! 	year=date(1:4)
! 	month=date(5:6)
! 	do N=1,NDAY+1
! 		do P=1,NPCL
! 		read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX1,RPCLY1,YI,IP1,NKAI,land0
! 		III=INT((RPCLX1-X_edge)*12)+1
! 		JJJ=INT((RPCLY1-Y_edge)*12)+1
! 		! if(reach(IP1,YS)==1)then
! 		! write(15,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX1,RPCLY1,YI

! 		! end if
! 		if(land(IP1,YI,YS)==1)then
! 		num(III,JJJ,YS)=num(III,JJJ,YS)+1
! 		end if
!               end do 
!         end do
!         close(14)
!         ! close(15)
!         do j=1,lj
! 	do i=1,li
! 		if (num(i,j,YS)>0)&
! 		write(16,'(F11.2,1x,F11.2,1x,I6,1x,A4,1X,A2)') lon(i),lat(j),num(i,j,YS)
! 	end do	
!        end do
!        close(16)
! end do


! ! ==============================输出着岸的密度=============================================================




! ! ================================输出产卵场来源的密度==============================================================

! do j=1,50
! 	do i=1,150
! 		if(spawn_num(i,j,YS)>0)then
! 			spawnlon(i)=(lon0+dlon*(i-1))
! 			spawnlat(j)=(lat0+dlat*(j-1))
! 			write(17,'(F11.2,1x,F11.2,1x,I6,1x,A4,1X,A2)') spawnlon(i),spawnlat(j),spawn_num(i,j,YS),year,month
! 		end if
! 	end do
! end do
! ! ===============================================================================================================
! end do
! close(14)
! close(15)
! close(16)
! close(17)

		! write(15,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6,1x,I6,1X,A4,1X,A2,1X,F11.6,1X,F11.6)')RPCLX1,RPCLY1,YI,IP1,&
		! land(P,N,YS),reach(P,YS),eel(P,N,YS),year,month,slon(p),slat(p)
		! if(NKAI==14400)YI=201
! =================================================算到达区域=======================================================

! open(15,file='/Volumes/erika/CMEMS/output/land/land_all.txt',status='replace')!可以到达的粒子输出trajectory
! write(15,'(A3,1X,A3,1X,A3,1X,A2,1X,A4,1X,A5,1X,A3,1X,A4,1X,A5,1X,A4,1X,A4)')&
! 'lon','lat','day','IP','land','reach','eel','year','month','slon','slat'

! open(18,file='/Volumes/erika/CMEMS/output/land/land_area.txt',status='replace')!可以到达的粒子输出trajectory

! reach_area(:,:,:)=0

! do YS=1,156

! 	open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
     
! 	do N=1,NDAY+1
! 		do P=1,NPCL
! 		read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX1,RPCLY1,YI,IP1,NKAI,land0
! 		III=INT((RPCLX1-X_edge)*12)+1
! 		JJJ=INT((RPCLY1-Y_edge)*12)+1
! 		if(land(IP1,YI,YS)==1)then

! 	         if((RPCLY1>=15.0).and.(RPCLY1<30.))then
! 		      if((RPCLX1>=105.0).and.(RPCLX1<135.))then
! 		       reach_area(P,N,YS)=1
! 		       end if
! 		     end if

! 		    if((RPCLY1>=5.0).and.(RPCLY1<15.0))then
! 		       if((RPCLX1>=111.0).and.(RPCLX1<130.))then
! 		       reach_area(P,N,YS)=2
! 		       end if
! 		     end if

! 		 if((RPCLY1>=-10.0).and.(RPCLY1<5.))then
! 		    if((RPCLX1>=110.0).and.(RPCLX1<140.))then
! 		       reach_area(P,N,YS)=3
! 		       end if
! 		     end if
! 		  write(*,*)reach_area(P,N,YS)
! 	end if    
!               end do 
!         end do
!         close(14)
! end do
! do YS=1,NSTART
!    do N=1,NDAY+1
!      do P=1,NPCL
!        write(18,'(I6)')reach_area(P,N,YS)
!      end do
!    end do
! end do
! close(18)

! ! ==================================计算时间和到达区域的关系==========================================
! open(18,file='/Volumes/erika/CMEMS/output/land/land_area.txt',status='old')
! time_area_switch(:,:,:)=0
! do YS=1,NSTART
! do N=1,NDAY+1
! do P=1,NPCL
! read(18,'(I6)')reach_area(P,N,YS)
! if (reach_area(P,N,YS)>0)then
! time_area_switch(P,reach_area(P,N,YS),YS)=1
! end if
! end do
! end do
! end do
! close(18)

! do YS=1,NSTART
! do area=1,3
! do P=1,NPCL
! if (time_area_switch(P,area,YS)==1)then
! time_area_num(area,YS)=time_area_num(area,YS)+1
! end if
! end do
! end do
! end do


!  do NI=1,NDAY
!     do area=1,3
!  	   open(20,file='/Volumes/erika/CMEMS/output/land/time_'//area//'.txt',status='replace')

!  	date=startdate(YS)
!  	year=date(1:4)
! month=date(5:6)

!   write(20,'(A4,1X,A2,1X,I3,1X,I6)')year,month,area,time_area_num(area,YS)
!  end do
! end do
! close(20)
!===================================================================================================

!====================================计算释放区域和到达区域的关系=====================================

! ====================================读入释放区域===================================================
! open(19,file='/Volumes/erika/CMEMS/set_input/start-point/release_region_divide.dat',status='old')
! read(19,*)(IP2,spawn_area(P),p=1,NPCL)
! close(19)
! ==================================读入到达区域==========================================
! open(18,file='/Volumes/erika/CMEMS/output/land/land_area.txt',status='old')
! time_area_switch(:,:,:)=0
! do YS=1,NSTART
! do N=1,NDAY+1
! do P=1,NPCL
! read(18,'(I6)')reach_area(P,N,YS)
! if (reach_area(P,N,YS)>0)then
! time_area_switch(P,reach_area(P,N,YS),YS)=1
! end if
! end do
! end do
! end do
! close(18)

! do YS=1,NSTART
! do area=1,3
! do P=1,NPCL
! if (time_area_switch(P,area,YS)==1)then
! spawn_reach_num(spawn_area(p),area,YS)=spawn_reach_num(spawn_area(p),area,YS)+1
! end if
! end do
! end do
! end do

! open(21,file='/Volumes/erika/CMEMS/output/land/spawn_reach_area.txt',status='replace')
! write(21,'(A10,1X,A10,1X,A6,1X,A4,1X,A5)')'spawn_area','reach_area','number','year','month'
! do YS=1,NSTART
!   do area=1,3
!     do s=1,6
!     date=startdate(YS)
! year=date(1:4)
! month=date(5:6)
!    write(21,'(I6,1X,I6,1X,I6,1X,A4,1X,A2)')s,area,spawn_reach_num(s,area,YS),year,month
!     end do
!   end do
! end do
! close(21)




! ==================================================================================================

! ====================================画到达域的图====================================================

! open(22,file='/Volumes/erika/CMEMS/output/land/land_area_graph.txt',status='replace')

! do j=1,lj
!   do i=1,li

!   	if (seashore(i,j)==1)then
!   		if((lat(j)>=15.0).and.(lat(j)<30.))then
! 		      if((lon(i)>=105.0).and.(lon(i)<135.))then
! 		      seashore_area(i,j)=1
! 		       end if
! 		     end if

! 		    if((lat(j)>=5.0).and.(lat(j)<15.0))then
! 		       if((lon(i)>=111.0).and.(lon(i)<130.))then
! 		       seashore_area(i,j)=2
! 		       end if
! 		     end if

! 		    if((lat(j)>=-10.0).and.(lat(j)<5.))then
! 		    if((lon(i)>=110.0).and.(lon(i)<140.))then
! 		       seashore_area(i,j)=3
! 		       end if
! 		     end if
! 		  write(*,*)seashore_area(i,j)
! 	end if
! 	end do
! 	end do 
  
!   do j=1,lj
!     do i=1,li
!     	write(22,'(F11.6,1X,F11.6,1X,I6)')lon(i),lat(j),seashore_area(i,j)

! 	  end do
! 	end do 

!====================================画到达密度图=======================================================


! do YS=1,156

! 	open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
     
! 	do N=1,NDAY+1
! 		do P=1,NPCL
! 		read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX1,RPCLY1,YI,IP1,NKAI,land0
! 		III=INT((RPCLX1-X_edge)*12)+1
! 		JJJ=INT((RPCLY1-Y_edge)*12)+1
! 		if(land(IP1,YI,YS)==1)then
! 			density_reach(III,JJJ,N)=density_reach(III,JJJ,N)+1
! 		end if
! 	end do
! end do
! end do

! do N=1,NDAY
! 	write(NN,'(I3.3)')N
! 	open(23,file='/Volumes/erika/CMEMS/output/density/byday/density_'//NN//'.txt',status='replace')
! 	do j=1,lj
! 		do i=1,li
! 			write(23,'(F11.2,1X,F11.2,1X,I6)')lon(i),lat(j),density_reach(i,j,N)
! 		end do
! 	end do
! 	close(23)
! end do



! =================================画到达粒子的轨迹============================================
do N=1,NDAY
write(NN,'(I3.3)')N
open(24,file='/Volumes/erika/CMEMS/output/density/byday/distribution_'//NN//'.txt',status='replace')
end do

do YS=1,156
	open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
	do N=1,NDAY	
		do P=1,NPCL
			read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX1,RPCLY1,YI,IP1,NKAI,land0

if((reach(P,YS)==1).and.(land(P,N,YS)==0))then
				close(24)
			end if
		end do
	end do
end do

	do N=1,NDAY	
		write(NN,'(I3.3)')N
		open(24,file='/Volumes/erika/CMEMS/output/density/byday/distribution_'//NN//'.txt',status='old')
			
				
				write(24,'(F11.2,1X,F11.2,1X)')RPCLX1,RPCLY1

end program

