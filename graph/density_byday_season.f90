program density
	implicit none
	integer,parameter::li=551,lj=461
	integer,parameter::NPCL=7500
	integer,parameter::NSTART=156,NDAY=200
	character::startdate(NSTART)*8,month*2,year*4,day*3,season(4)*2,datenow*8
	real::lon(li),lat(lj),RPCLX,RPCLY
	integer::num(li,lj,NDAY+1,4)
	integer::i,j,III,JJJ,YS,N,P,IP,YI,NKAI,land,m,day0,k,y,yy,s


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

season(:)=['01','04','07','10']
open(9, file ='/Volumes/erika/CMEMS/set_input/start-timing/start.txt',status='old')
read(9,'(A8)')(startdate(YS),YS=1,NSTART)
close(9)

num(:,:,:,:)=0

! do YS=1,NSTART
	do YS=1,12
	write(*,'(A8,A12)')startdate(YS),'計算中。。。'
		  datenow=startdate(YS)
		  month=datenow(5:6)
    do s=1,4
    if(month==season(s))then
	open(14,file='/Volumes/erika/CMEMS/output/all/0124/'//startdate(YS)//'.dat',status='old')
	do N=1,201
		do P=1,NPCL
			read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX,RPCLY,YI,IP,NKAI,land
			III=int((RPCLX-100.0)/0.1)+1
			JJJ=int((RPCLY+15.0)/0.1)+1
			num(III,JJJ,yi+1,s)=num(III,JJJ,yi+1,s)+1

	  		! write(*,*)num(III,JJJ,yi)
		end do
	 end do
    end if
  end do
end do

do s=1,4
 do y=1,NDAY+1,50
	write(day,'(I3.3)')y
	open(10,file='/Volumes/erika/CMEMS/output/density/byday/'//day//'_'//season(s)//'.dat',status='replace')
	do j=1,lj
		do i=1,li
			write(10,'(F11.2,1x,F11.2,1X,I10)')lon(i),lat(j),num(i,j,y,s)
			write(*,'(F11.2,1x,F11.2,1X,I10)')lon(i),lat(j),num(i,j,y,s)
		end do 
	end do
end do
end do



end program
