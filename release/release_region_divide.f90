program release
implicit none
integer,parameter ::N=7500, ni=150, nj=50
integer::nc,i,j,p,region(N)
real :: lon(ni),lat(nj),RPCLX(N),RPCLY(N)

open(4,file='/Volumes/erika/CMEMS/set_input/start-point/release1.dat',status='old')
open(8,file='/Volumes/erika/CMEMS/set_input/start-point/release_div_pic.txt',status='replace')
do nc=1,N
read(4,*)RPCLX(nc),RPCLY(nc)
if((RPCLX(nc)>=130.).and.(RPCLX(nc)<135.))then
	if((RPCLY(nc)>=12.).and.(RPCLY(nc)<14.5))then
		region(nc)=1
	end if
end if

if((RPCLX(nc)>=135.).and.(RPCLX(nc)<140.))then
	if((RPCLY(nc)>=12.).and.(RPCLY(nc)<14.5))then
		region(nc)=2
	end if
end if

if((RPCLX(nc)>=140.).and.(RPCLX(nc)<145.))then
	if((RPCLY(nc)>=12.).and.(RPCLY(nc)<14.5))then
		region(nc)=3
	end if
end if

if((RPCLX(nc)>=130.).and.(RPCLX(nc)<135.))then
	if((RPCLY(nc)>=14.5).and.(RPCLY(nc)<17.))then
		region(nc)=4
	end if
end if

if((RPCLX(nc)>=135.).and.(RPCLX(nc)<140.))then
	if((RPCLY(nc)>=14.5).and.(RPCLY(nc)<17.))then
		region(nc)=5
	end if
end if

if((RPCLX(nc)>=140.).and.(RPCLX(nc)<145.))then
	if((RPCLY(nc)>=14.5).and.(RPCLY(nc)<17.))then
		region(nc)=6
	end if
end if

write(8 ,'(F11.6,1x,F11.6,1X,I6)')RPCLX(NC),RPCLY(NC),region(nc)
end do
close(4)
! open(7,file='/Volumes/erika/CMEMS/set_input/start-point/release_region_divide.dat',status='replace')
! write(7 ,'(I6,1x,I6)')(nc,region(nc),nc=1,N)
! close(7)



close(8)

end program