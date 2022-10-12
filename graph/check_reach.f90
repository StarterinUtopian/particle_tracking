program check_reach
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
	integer::reach(NPCL,NSTART),num(li,lj,YS),spawn_num(150,50,YS),land0,land(NPCL,NDAY+1,NSTART)
	integer::seashore(li,lj),eel(NPCL,NDAY+1,NSTART)
	integer::da,day_land(P,YS)


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

! ------初始值-----
reach(:,:)=0
land(:,:,:)=0
! ----------------
do YS=1,156
   write(*,'(A22)')startdate(YS),'リスト計算中'
	date=startdate(YS)
	year=date(1:4)
	month=date(5:6)
	! 打开读取的文件
	open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
	call count_date(startdate(YS),dateall,dateyear)
	do N=1,NDAY+1
		do P=1,NPCL
			read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX1,RPCLY1,YI,IP1,NKAI,land0
			! 先算出iii,jjj
			III=INT((RPCLX1-X_edge)*12)+1
			JJJ=INT((RPCLY1-Y_edge)*12)+1

			if(seashore(iii,jjj)==1)then
				! 先判断出所有时间里的可以到达的粒子
				num(III,JJJ,YS)=num(III,JJJ,YS)+1!统计到达的粒子数
				reach(P,YS)=1
				land(P,N,YS)=1
			else
				land(P,N,YS)=0
			end if
		end do
	end do
end do

open(15,)
