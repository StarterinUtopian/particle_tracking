! gfortran T_read.f90 -L/usr/local/Cellar/netcdf/4.8.1/lib -lnetcdff
program main
include  '/usr/local/Cellar/netcdf/4.8.1/include/netcdf.inc'
integer,parameter::LN=4320,JN=2041,DN=50
integer,parameter::li = 640, lj = 522, ll = 30,l=5
real :: T(li,lj,ll),S(li,lj,ll)
real :: Tt(LN,JN,DN,1),St(LN,JN,DN,1),lon(li),lat(lj)
real(8) :: add_offset1, scale_factor1,add_offset2, scale_factor2, depth(l),DD(ll),HH(li, lj, ll)
integer :: ncid,test,rhid,i,j,k,v,dev(l)
character::file_name*48,out_v*52,out_u*52
character::dateyear(4924)*8,year*4,month*2,datenow*8


open(3, file = '/Volumes/erika/CMEMS/set_input/grid_coordinates.dat')
read(3,*)(DD(k), k = 1, ll)
read(3,*)(((HH(i, j, k), i = 1, li), j = 1, lj),k = 1, ll)
close(3)
depth(1:5)=(/0.0,50.0,100.0,150.0,200.0/)

open(7,file='/Volumes/erika/CMEMS/set_input/grid_lonlat.dat',status='old')
read(7,*)((lon(i),lat(j),i=1,li),j=1,lj)
close(7)

do v=1,l
	dev(1)=1
	if(v>1)then
		do k=1,ll
			if (depth(v)<DD(k))then
				dev(v)=k
				exit
			end if
		end do
     end if
end do

! write(*,*)(dev(v),v=1,l)



open(4,file='/Volumes/erika/CMEMS/set_input/start-timing/dateall.txt',status="old")
do ni=1,4924
	read(4,'(A8)') dateyear(ni)
end do



do ni=1,4924
	datenow=dateyear(ni)
	year=datenow(1:4)
	month=datenow(5:6)
	file_name='/Volumes/CNMES2006-2019/data/'//year//'/'//month//'/'//datenow//'.nc'



	
! **********************************************************************************
!                             读入v
! **********************************************************************************
test=nf_open(file_name,nf_nowrite,ncid)
	if (test .NE. nf_noerr) then
		write(*,*)'1'
		write(*,*) nf_strerror(test)
		stop
	endif

test=nf_inq_varid(ncid,'thetao',rhid)
	if (test .NE. nf_noerr) then
		write(*,*)'7'
		write(*,*) nf_strerror(test)
		stop
	endif
	test=nf_get_var_real(ncid,rhid,Tt)
		if (test .NE. nf_noerr) then
		write(*,*)'8'
		write(*,*) nf_strerror(test)
		stop
	endif



test=nf_get_att(ncid, rhid, 'add_offset', add_offset2)
	if (test .NE. nf_noerr) then
		write(*,*)'9'
		write(*,*) nf_strerror(test)
		stop
	endif
	
	test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor2)
		if (test .NE. nf_noerr) then
		write(*,*)'10'
		write(*,*) nf_strerror(test)
		stop
	endif

test=nf_close(ncid)
	if (test .NE. nf_noerr) then
		write(*,*)'11'
		write(*,*) nf_strerror(test)
		stop
	endif




T(:,:,:)=Tt(3361:4000,800:1321,1:30,1)*scale_factor2+add_offset2
open(8,file='/Volumes/nozoe/T/T_'//datenow//'.txt',status='replace')
write(8,'(A3,1X,A3,1X,A3,1X,A4)')'lon','lat','dep','temp'
do v=1,l
     do j=1,lj
          do i=1,li
               if (T(i,j,k)>=-2.)then
                write(8,'(F11.2,1X,F11.2,1X,F5.1,1X,F11.6)')lon(i),lat(j),depth(v),T(i,j,dev(v))
               else
               write(8,'(F11.2,1X,F11.2,1X,F5.1,1X,A3)')lon(i),lat(j),depth(v),"N/A"
               end if
          end do 
     end do
end do
close(8)
! ************************************************************************
write(*,*)dateyear(ni)
end do
close(4)




end program main

subroutine handle_err(test,n)
	integer::test,n
	n=n+1
	if (test .NE. nf_noerr) then
		write(*,*)n
		write(*,*) nf_strerror(test)
		stop
	endif
end subroutine handle_err