program source_by_area
	! 这个文件用于给每个粒子分区以及计数，用到了海岸的判别（还在弄）

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
	integer::i,j,ii,jj,III,JJJ,YS,N,P,IP,YI,area

	open(7,file='/Volumes/erika/CMEMS/output/count/source0320.dat')


	do N=1311903
		read(7,'(F11.6,1X,F11.6,1X,I6)',status='old')RPCLX,RPCLY,area
		open(8,file='/Volumes/erika/CMEMS/output/count/source_area'//area//'.dat')
		write(8,)
	end do 

