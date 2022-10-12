program density
	implicit none
	integer,parameter::li=551,lj=461
	integer,parameter::NPCL=7500
	integer,parameter::NSTART=156,NDAY=200
	character::startdate(NSTART)*8,month*2,year*4
	real::lon(li),lat(lj),RPCLX,RPCLY
	integer::num(li,lj,NDAY+1)
	integer::i,j,III,JJJ,YS,N,P,IP,YI,NKAI,land,m,day0,k,y,yy,day


do i=1,li
	lon(i)=100.0+(i-1)*0.1
end do
do j=1,lj
	lat(j)=-15.0+(j-1)*0.1
end do



! do k=1,201
! 	day0=k
! 	write(day1,'(I3.3)')day0
! 	day(k)=day1
! end do


open(9, file ='/Volumes/erika/CMEMS/set_input/start-timing/start.txt',status='old')
read(9,'(A8)')(startdate(YS),YS=1,NSTART)
close(9)

num(:,:,:)=0

do YS=1,NSTART

	write(*,'(A8,A12)')startdate(YS),'計算中。。。'
	
	open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
	do N=1,201
		do P=1,NPCL
			read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX,RPCLY,YI,IP,NKAI,land
			III=int((RPCLX-100.0)/0.1)+1
			JJJ=int((RPCLY+15.0)/0.1)+1
			if(NKAI<14400)then

			num(III,JJJ,yi)=num(III,JJJ,yi)+1

			! write(*,*)num(III,JJJ,yi)
		end do
	end do
end do

do y=1,NDAY+1,10
	write(day,'(I3.3)')y
	open(10,file='/Volumes/erika/CMEMS/output/density/byday/'//day//'.dat',status='replace')
	do j=1,lj
		do i=1,li
			write(10,'(F11.2,1x,F11.2,1X,I10)')lon(i),lat(j),num(i,j,y)
			write(*,'(F11.2,1x,F11.2,1X,I10)')lon(i),lat(j),num(i,j,y)
		end do 
	end do
end do



end program
