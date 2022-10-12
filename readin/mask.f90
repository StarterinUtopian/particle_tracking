! gfortran S_read.f90 -L/usr/local/Cellar/netcdf/4.8.1/lib -lnetcdff
program main
include  '/usr/local/Cellar/netcdf/4.8.1/include/netcdf.inc'
integer,parameter::LN=4320,JN=2041,DN=50
integer,parameter::li = 640, lj = 522, ll = 30
real :: S(li,lj,ll)
real :: St(LN,JN,DN,1)
real(8) :: add_offset1, scale_factor1,add_offset2, scale_factor2,lon(li),lat(lj)
integer :: ncid,test,rhid,i,j,k,mask(li,lj,ll)
character::file_name*48,out_v*52,out_u*52
character::dateyear(4924)*8,year*4,month*2,datenow*8

open(4,file='/Volumes/erika/CMEMS/set_input/start-timing/dateall.txt',status="old")
open(7,file='/Volumes/erika/CMEMS/set_input/grid_lonlat.dat',status="old")
	do j=1,lj
		do i=1,li
read(7,*)lon(i),lat(j)
! write(*,*)lon(i),lat(j)
end do 
end do
close(7)

! do ni=1,4924
	do ni=1,1
	read(4,'(A8)') dateyear(ni)
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

test=nf_inq_varid(ncid,'so',rhid)
	if (test .NE. nf_noerr) then
		write(*,*)'7'
		write(*,*) nf_strerror(test)
		stop
	endif
	test=nf_get_var_real(ncid,rhid,St)
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




! S(:,:,:)=St(3361:4000,800:1321,1:30,1)*scale_factor2+add_offset2
S(:,:,1:30)=St(3361:4000,800:1321,1:30,1)*scale_factor2+add_offset2
! open(8,file='/Volumes/erika/CMEMS/processed_data/S/S_'//datenow//'.dat',status='replace')
 open(8,file='/Volumes/erika/CMEMS/set_input/mask.dat',status='replace')

do k=1,ll
     do j=1,lj
          do i=1,li
               if (abs(S(i,j,k)).LE.60.)then
               ! if (S(i,j,k).LE.0.)then
               ! 	! Land-sea mask: 1 = sea ; 0 = land'
               ! 	mask(i,j,k)=0
               ! else
               ! 	mask(i,j,k)=1
               ! end if

                write(8,*)mask(i,j,k)
               ! else
               ! write(*,*)9999999
               
          end do 
     end do
end do
close(8)
! ************************************************************************

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