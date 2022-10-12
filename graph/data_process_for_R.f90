program main
	implicit none 
	integer,parameter::li = 640, lj = 522, ll =30!読み込みグリッド数 
	 integer,parameter::NPCL=7500
     real,parameter::IDSY=9259.25
     real, parameter::pi = 3.1415926535
     real, parameter::X_edge = 100.000000  !読み込むデータのX方向の最西座標
     real, parameter::Y_edge = -13.4166670  !読み込むデータのY方向の最南座標
     integer,parameter::NSTART=156,NDAY=200,DDT=1200!計算
	character::startdate(NSTART)*8,date*8,year*4,month*2
	real::lon(li),lat(lj),RPCLX,RPCLY,RX(NPCL),RY(NPCL)
	integer::seashore(li,lj),switch(14,NPCL),num_spawn(li,lj)
	integer::i,j,ii,jj,III,JJJ,YS,N,P,IP,YI,NKAI,land
	! 读入所有的输出数据
	! 把所有文件打开，然后读每一条记录
open(7, file ='/Volumes/erika/CMEMS/set_input/start-timing/start.txt',status='old')
read(7,'(A8)')(startdate(YS),YS=1,NSTART)
close(7)

! 读入沿岸判断的数据
open(8,file='/Volumes/erika/CMEMS/set_input/shore.dat',status='old')
read(8,'(I6)')((seashore(i,j),i=1,li),j=1,lj)lon(i),lat(j),coast(i,j)
close(8)
! shore.dat格式(lon,lat,shore(0,1)),seashore格式（沿岸区域的lon，lat）
! 读入输出数据
do NS=1,NSTART
open(9,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
open(9,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
do N=1,NDAY+1
	do P=1,NPCL
		read(9,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')&
          RPCLX,RPCLY,YI,IP,NKAI,land
          write(10,'()')



! 判断到达/不到达

! 然后加上“，”，重新输出，
! 读取编号和初始位置表，读取初始位置，
! 然后初始位置的格子的数值加一
end program