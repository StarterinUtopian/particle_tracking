program count
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
	integer::i,j,ii,jj,III,JJJ,YS,N,P,IP,YI,NKAI,land,area,area_number(14),area_number_day(14,NDAY+1),nc,a,y




!遍历所有格子，然后找出0和1交界的格子
! 把所有文件打开，然后读每一条记录
open(7, file ='/Volumes/erika/CMEMS/set_input/start-timing/start.txt',status='old')
read(7,'(A8)')(startdate(YS),YS=1,NSTART)
close(7)

! 先导入所有格子的信息看是否是沿岸
open(8,file='/Volumes/erika/CMEMS/set_input/shore.dat',status='old')
read(8,'(I6)')((seashore(i,j),i=1,li),j=1,lj)
close(8)



open(16,file='/Volumes/erika/CMEMS/output/land/num_ym.csv',status='replace')
write(16,'(A4,1X,A5,1X,A4,1X,A6)')&
'year','month','area','number'

open(18,file='/Volumes/erika/CMEMS/output/land/num_day.csv',status='replace')
write(18,'(A4,1x,A2,1x,A6)')&
'area','yi','number'


open(19,file='/Volumes/erika/CMEMS/set_input/start-point/release2.dat')
read(19,'(I6,1x,f11.2,1x,f11.2)')(nc,RX(nc),RY(nc),nc=1,NPCL)
close(19)

open(17,file='/Volumes/erika/CMEMS/output/count/source.dat',status='replace')
area_number(:)=0


 do YS=1,NSTART

write(*,'(A8,A10)')startdate(YS),'計算中...'
open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
switch(:,:)=0
date=startdate(YS)
year=date(1:4)
month=date(5:6)





do N=1,NDAY+1
	do p=1,NPCL
		read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX,RPCLY,YI,IP,NKAI,land
		! 		! 求iii,jjj
		iii=INT((RPCLX-X_edge)*12)+1
		jjj=INT((RPCLY-Y_edge)*12)+1
		! write(*,*)III,JJJ
		! write(*,*)seashore(III,JJJ)


		if((III<=li).and.(JJJ<=lj))then
			if(seashore(iii,jjj)==1)then!第一层,判断是不是沿岸海域
				! 下面开始分区
				! 1黑潮2ミンダナオ海流3インドネシア通過流
				! 下面定义黑潮域
			if((RPCLY>=15.0).and.(RPCLY<30.))then
				if((RPCLX>=105.0).and.(RPCLX<135.))then
					area=1
				end if
			end if


			 ! 下面定义ミンダナオ流域
			 	if((RPCLY>=5.0).and.(RPCLY<15.0))then
					if((RPCLX>=111.0).and.(RPCLX<130.))then
						area=2
					end if
				end if  

		


			 ! 下面定义インドネシア通過流域
			    if((RPCLY>=-10.0).and.(RPCLY<5.))then
				if(((RPCLX>=110.0).and.(RPCLX<140.)))
					area=3
				end if
			end if



				
				if(switch(area,ip)==0)then!防止重复计算的switch
					area_number(area)=area_number(area)+1!算区域数

					write(17,'(F11.6,1X,F11.6,1X,I6)')RX(IP),RY(IP),area!17，时间的file
					write(16,'(A4,1X,A2,1X,I6,1X,I6)')&
　　　　　　　　　(year,month,area,area_number(area),area=1,14)


! 					switch(area,ip)=1
				end if
				
			
		
		end do!粒子数

	end do!天数
! close(16)

end do!开始时间

do y=1,NDAY+1
do a=1,14
 write(18,'(I4,1X,I4,1X,I10)')&
a,y,area_number_day(a,y)
write(*,'(I4,1X,I4,1X,I10)')&
a,y,area_number_day(a,y)

end do
end do
close(18)


! close(17)

end program

