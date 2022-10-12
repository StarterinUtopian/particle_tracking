program release
implicit none
! integer,parameter ::N=7500, ni=150, nj=50,np=100
integer,parameter ::N=900, ni=3, nj=3,np=100
integer::nc,i,j,p
real :: lon(ni),lat(nj),RPCLX(N),RPCLY(N),PCLZ(N),lon0,lon1,lat0,lat1,slon,slat
lon0=130.
lat0=12.
lon1=144.9
lat1=16.9
slon=((lon1-lon0)/(ni-1))*1.0d0
slat=((lat1-lat0)/(nj-1))*1.0d0
nc=0
open(4,file='/Volumes/erika/CMEMS/set_input/start-point/release_sixpoints.dat',status='replace')
if (nc.LE.N) then

do p=1,np
	do j=1,nj
		do i=1,ni  
			nc=nc+1
			RPCLX(nc)=(lon0+slon*(i-1))
			RPCLY(nc)=(lat0+slat*(j-1))
		end do 
	end do

end do
end if




! open(4,file='/Volumes/erika/CMEMS/set_input/start-point/release_wide.dat',status='replace')
! write(4 ,'(f11.2,1x,f11.2)')(RPCLX(nc),RPCLY(nc),nc=1,N)
! close(4)

write(4 ,'(f11.2,1x,f11.2)')(RPCLX(nc),RPCLY(nc),nc=1,N)
close(4)
! open(5,file='/Volumes/erika/CMEMS/set_input/start-point/release2.dat',status='replace')
! write(5 ,'(I6,1x,f11.2,1x,f11.2)')(nc,RPCLX(nc),RPCLY(nc),nc=1,N)
! close(5)
open(7,file='/Volumes/erika/CMEMS/set_input/start-point/release_wide_withip.dat',status='replace')
write(7 ,'(I6,1x,f11.2,1x,f11.2)')(nc,RPCLX(nc),RPCLY(nc),nc=1,N)
close(7)
end program