! gfortran V_read.f90 -L/usr/local/Cellar/netcdf/4.8.1/lib -lnetcdff
program main
include  '/usr/local/Cellar/netcdf/4.8.1/include/netcdf.inc'
integer,parameter::LN=4320,JN=2041,DN=50
integer,parameter::li = 640, lj = 522, ll = 30
real :: U(li,lj,ll),V(li,lj,ll)
real :: Ut(LN,JN,DN,1),Vt(LN,JN,DN,1)
real(8) :: add_offset1, scale_factor1,add_offset2, scale_factor2
integer :: ncid,test,rhid,i,j,k
character::file_name*48,out_v*52,out_u*52
! character::dateyear(4924)*8,year*4,month*2,datenow*8
character::dateyear(622)*8,year*4,month*2,datenow*8

! open(4,file='/Volumes/erika/CMEMS/set_input/start-timing/dateall.txt',status="old")
open(4,file='/Volumes/erika/CMEMS/set_input/start-timing/datefault.txt',status="old")

! do ni=1,4924
do ni=1,622
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

test=nf_inq_varid(ncid,'vo',rhid)
	if (test .NE. nf_noerr) then
		write(*,*)'7'
		write(*,*) nf_strerror(test)
		stop
	endif
	test=nf_get_var_real(ncid,rhid,Vt)
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




V(:,:,:)=Vt(3361:4000,800:1321,1:30,1)*scale_factor2+add_offset2
open(8,file='/Volumes/erika/CMEMS/processed_data/V/V_'//datenow//'.dat',status='replace')
do k=1,ll
     do j=1,lj
          do i=1,li
               if (abs(V(i,j,k)).GE.5.) V(i,j,k)=0.
          end do 
     end do
end do
write(8,*)(((V(i,j,k),i=1,li),j=1,lj),k=1,ll)
close(8)
! ************************************************************************

end do
close(4)
end program main

! subroutine handle_err(test,n)
! 	integer::test,n
! 	n=n+1
! 	if (test .NE. nf_noerr) then
! 		write(*,*)n
! 		write(*,*) nf_strerror(test)
! 		stop
! 	endif
! end subroutine handle_err