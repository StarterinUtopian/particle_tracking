! 用来画产卵场和到达地关系的，导出两百天后的位置
program spawn_reach
	implicit none
	integer,parameter::li = 640, lj = 522, ll = 30
	integer,parameter::NPCL=7500
	real,parameter::IDSY=9259.25
	 real, parameter::pi = 3.1415926535
     real, parameter::X_edge = 120.000000  !読み込むデータのX方向の最西座標
     real, parameter::Y_edge = -60.000000  !読み込むデータのY方向の最南座標
     integer,parameter::NSTART=156,NDAY=200,DDT=1200
     integer,parameter::dall=4924!計算
     real,parameter::PDIFH=100!扩散系数
     real,parameter::lon0=120.,lat0=5.,lon1=144.9,lat1=24.9,slon=0.1,slat=0.1
	character::startdate(NSTART)*8,date*8,year*4,month*2,dateall(dall)*8,dateyear(NDAY+1)*8
	real::lon(li),lat(lj),RPCLX1,RPCLY1,RPCLX2,RPCLY2,spawnlon(150),spawnlat(50)
	real::TC,TE,TNE,TN,DX,DY,PCLT,T(li,lj,ll),IDSX
	integer::i,j,k,II,JJ,KK,III,JJJ,LLL
	integer::YS,N,P,PP,YI,NKAI,land,area,IP,IP1,IP2
	integer::reach(NPCL,NSTART),num(li,lj),spawn_num(150,50),land(NPCL,NDAY+1,NSTART)
	integer::seashore(li,lj),eel(NPCL,NDAY+1,NSTART)
	integer::da


	! ================================变量说明=================================================================
	! eel鳗鱼种类，1=marmorata 2=japonica
	! reach 200天内有无到达过，land，在这一天是否到达
	! =======================================================================================================

	! =====================================文件编号和说明===================================================
	! 7输入文件，模型网格点
	! 8输入文件，海岸区域
	! 9输入文件，所有开始日期
	! 10输入文件，每个粒子的释放位置
	! 11输入文件，水温
	! 15输出文件，所有可以到达的粒子的所有位置(现在是按天算)
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
read(8,'(I6)')((seashore(i,j),i=1,li),j=1,lj)
close(8)

open(9, file ='/Volumes/erika/CMEMS/set_input/start-timing/start.txt',status='old')
read(9,'(A8)')(startdate(YS),YS=1,NSTART)
close(9)


reach(:,:)=0




do YS=1,156
	write(*,'(A8)')startdate(YS)
	! 打开读取的文件
	open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')

	call count_date(startdate(YS),dateall,dateyear)

! ============================下面开始数到达的密度==================================================
! 在两个循环后再输出,按年和月统计，并制作到达粒子的IP—list
do N=1,NDAY+1
	open(11,file='/Volumes/erika/CMEMS/processed_data/T/T_'//dateyear(N)//'.dat',status='old')
	read(11,*)(((T(i,j,k),i=1,li),j=1,lj),k=1,ll)
	close(11)
	do P=1,NPCL
		read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX1,RPCLY1,YI,IP1,NKAI,land
		! 先算出iii,jjj
		III=INT((RPCLX1-X_edge)*12)+1
		JJJ=INT((RPCLY1-Y_edge)*12)+1
 
		! ======================读入水温，判断是哪种鳗鱼===================================

               TC=T(III,JJJ,LLL)
               TE=T(III+1,JJJ,LLL)
               TNE=T(III+1,JJJ+1,LLL)
               TN=T(III,JJJ+1,LLL)       

               !DX, DYは粒子の格子内水平位置を示した
               
               DX=(RPCLX1-lon(III))/(1./12.)
               DY=(RPCLY1-lat(JJJ))/(1./12.)

               PCLT=((TC*(1.-DX)+TE*DX)*(1.-DY))+((TN*(1.-DX)+TNE*DX)*DY)
               if((PCLT>24.).and.(PCLT<50.))then
               	eel(P,N,YS)=1
               else if(PCLT<24.)then
               	eel(P,N,YS)=2
               end if


		! =============================================================================
		if(seashore(iii,jjj)==1)then
			! 先判断出所有时间里的可以到达的粒子
			num(III,JJJ)=num(III,JJJ)+1!统计到达的粒子数
			reach(P,YS)=1
			land(P,N,YS)=1
		else
			land(P,N,YS)=0
		end if
	end do
end do
! ==============================================================================================

!==================================下面数到达粒子中产卵场的粒子密度===========================================
!所有的天数都数完再输出 
open(10,file='/Volumes/erika/CMEMS/set_input/start-point/release2.dat',status='old')
do P=1,NPCL
	read(10,'(I6,1x,f11.2,1x,f11.2)')IP2,RPCLX2,RPCLY2	
	spawn_lonlat(P)=matmul(RPCLX2,RPCLY2)
	if(reach(IP2,YS)==1)then
		II=(RPCLX2-lon0)/slon+1
		JJ=(RPCLY2-lat0)/slat+1
		spawn_num(II,JJ,YS)=spawn_num(II,JJ,YS)+1
	end if		
end do
close(10)
! ==============================================================================================



close(14)


! ======================================输出文件15==================================================================
! 打开输出文件
open(15,file='/Volumes/erika/CMEMS/output/land/trajectory.txt',status='replace')!可以到达的粒子输出trajectory
write(15,'()')'lon','lat','day','IP','year','month'!RPCLX1,RPCLY1,YI,IP1,&
					! land(P,N,YS),reach(P,YS),eel(P,N,YS)

open(16,file='/Volumes/erika/CMEMS/output/land/land_density.txt',status='replace')
write(16,'(A3,1X,A3,1X,A3,1X,A4,1X,A2)')'lon','lat','num','year','month'

open(17,file='/Volumes/erika/CMEMS/output/land/land_particle_spawn_density.txt',status='replace')
write(17,'(A3,1X,A3,1X,A3,1X,A4,1X,A2)')'lon','lat','num','year','month'

do YS=1,156
	! 打开读取的文件
	open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
	date=startdate(YS)
	year=date(1:4)
	month=date(5:6)
	do N=1,NDAY+1
		do P=1,NPCL
				read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX1,RPCLY1,YI,IP1,NKAI,land
				if(NKAI==14400)YI=201
				

					write(15,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX1,RPCLY1,YI,IP1,&
					land(P,N,YS),reach(P,YS),eel(P,N,YS),year,month

		end do 
	end do
end do
close(14)
close(15)

!======================================输出16，17==================================================================
! ==============================输出着岸的密度=============================================================

do j=1,lj
	do i=1,li
		if (num(i,j)>1)&
		write(16,'(F11.2,1x,F11.2,1x,I6,1x,A4,1X,A2)') lon(i),lat(j),num(i,j),year,month
	end do	
end do
! ================================输出产卵场来源的密度==============================================================

do j=1,50
	do i=1,150
		if(spawn_num(i,j)>1)then
			spawnlon(i)=(lon0+slon*(i-1))
			spawnlat(j)=(lat0+slat*(j-1))
			write(17,'(F11.2,1x,F11.2,1x,I6,1x,A4,1X,A2)') spawnlon(i),spawnlat(j),spawn_num(i,j),year,month
		end if
	end do
end do
! ===============================================================================================================
end do

write(16,'(F11.2,1X,F11.2,1X,I6)')((lon(i),lat(j),num(i,j),i=1,li),j=1,lj)
write(17,'(F11.2,1X,F11.2,1X,I6)')((spawn_lon(II),spawn_lat(JJ),spawn_num,II=1,150),JJ=1,50)

close(16)
close(17)
! ================================================================================================================

end program

subroutine count_date(startdate,dateall,dateyear)!時間換算用

	integer,parameter::dall=4924
	character,intent(in)::startdate*8
	character,intent(in)::dateall(dall)*8
	character,dimension(NDAY),intent(out)::dateyear*8
	integer::da   

	do da=1,dall
		if (startdate==dateall(da))then
			dateyear(:)=dateall(da:(da+NDAY-1))
		end if
	enddo


end subroutine count_date