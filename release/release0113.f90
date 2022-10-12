program release
implicit none
integer,parameter ::N=7500, ni=10, nj=10,np=1
integer::nc,i,j,p,g
character::gg*2
real :: lon(ni),lat(nj),RPCLX,RPCLY,lon0,lon1,lat0,lat1,slon,slat
lon0=130.
lat0=12.

slon=0.1
slat=0.1
nc=0
g=1

do while (nc.LE.N) !没到粒子总数就继续
	if (nc==1)then
	open(4,file='/Volumes/erika/CMEMS/set_input/start-point/release01.dat',status='replace')	
	else if (mod(nc,1000)==1)then
	g=g+1
	write(gg,'(A2)')g
	open(4,file='/Volumes/erika/CMEMS/set_input/start-point/release'//gg//'.dat',status='replace')
	lon0=lon0+1.
	if(mod(nc,15000)==1)then 
	lon0=130.
	lat0=lat0+1.
    end if
end if
    do p=1,np
	do j=1,nj
		RPCLY=(lat0+slat*(j-1))
		write(*,*)
		do i=1,ni  
			nc=nc+1
			RPCLX=(lon0+slon*(i-1))
			
			write(* ,'(I6,1x,f11.2,1x,f11.2)')nc,RPCLX,RPCLY
			write(4 ,'(f11.2,1x,f11.2)')RPCLX,RPCLY
		end do 
	end do
end do
close(4)
end do







end program