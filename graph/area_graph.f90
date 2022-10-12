

program area_graph
	implicit none
	integer,parameter::li=551,lj=461
	integer,parameter::NPCL=7500
	integer,parameter::NSTART=156,NDAY=200
	character::startdate(NSTART)*8
	real::lon(li),lat(lj),RPCLX,RPCLY,x,y
	integer::num(li,lj)
	integer::i,j,III,JJJ,YS,N,P,IP,YI,NKAI,land,area

open(7,'/Volumes/erika/CMEMS/set_input/grid_lonlat.dat',status='old')
read(7,*)((lon(i),lat(j),i=1,li),j=1,lj)
close(7)


open(9, file ='/Volumes/erika/CMEMS/set_input/start-timing/start.txt',status='old')
read(9,'(A8)')(startdate(YS),YS=1,NSTART)
close(9)

do YS=1,12
write(*,'(A8)')startdate(YS)
open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
do N=1,201
	do P=1,NPCL
		read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX,RPCLY,YI,IP,NKAI,land
		if (YI==201)then
			III=int((RPCLX-100.0)/0.1)+1
			JJJ=int((RPCLY+15.0)/0.1)+1
			num(III,JJJ)=num(III,JJJ)+1
		! end if
    end do 
end do
close(14)
end do

open(15,file='/Users/aorimbe/Desktop/shuku/graph/area_graph.dat',status='replace')
do j=1,lj
	do i=1,li

	


			if((lat(j)>25.).and.(lat(j)<30.))then
				if((lon(i)>125.0).and.(lon(i)<130.0))then
				area=1
			end if
		   end if


			if(lat(j)>20.0).and.(lat(j)<25.0)then
				if((lon(i)>121.0).and.(lon(i)<125.0))then
				area=2
			end if
		end if



			if(lat(j)>15.0).and.(lat(j)<20.0)then
				if(lon(i)<=121.0)
				area=


			elseif((y>10.).and.(y<13.))then
				if((x>121.3).and.(x<124.3))&
				area=3


			elseif((y>5.).and.(y<10.))then
				if((x>125.).and.(x<128.))&
				area=4


			elseif((y>0.5).and.(y<5.))then
				if((x>117.).and.(x<125.5))&
				area=5

			elseif(y<=0.5)then
				if((x>120.).and.(x<130.))&
				area=6
				if((x>115.).and.(x<120.))&
				area=7


			end if




			! if((area>0).eqv..false.)then
			! 	area=8
			! endif
		write(15,'(F11.2,1x,F11.2,1x,I6)') x,y,area
end if
      
end do	
end do
close(14)
close(15)	





end program
