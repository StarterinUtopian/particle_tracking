! gfortran V_read_0316.f90 -L/usr/local/Cellar/netcdf/4.8.1/lib -lnetcdff
program main
include  '/usr/local/Cellar/netcdf/4.8.1/include/netcdf.inc'
integer,parameter::LN=4320,JN=2041,DN=50
integer,parameter::li = 1081, lj = 961, ll = 2
real :: U(li,lj,ll),V(li,lj,ll),lon(li),lat(lj)
real :: Ut(LN,JN,DN,1),Vt(LN,JN,DN,1),lont(LN,1),latt(JN,1)
real(8) :: add_offset1, scale_factor1,add_offset2, scale_factor2
integer :: ncid,test,rhid,i,j,k
character::file_name*48,out_v*52,out_u*52
! character::dateyear(4924)*8,year*4,month*2,datenow*8
character::dateyear(4924)*8,year*4,month*2,datenow*8


open(4,file='/Volumes/erika/CMEMS/set_input/start-timing/dateall.txt',status="old")

do ni=1,4924
	read(4,'(A8)') dateyear(ni)
	datenow=dateyear(ni)
	year=datenow(1:4)
	month=datenow(5:6)
	file_name='/Volumes/CNMES2006-2019/data/'//year//'/'//month//'/'//datenow//'.nc'


! 一样要上下分开读哦
! **********************************************************************************
!                             读入U
! **********************************************************************************
test=nf_open(file_name,nf_nowrite,ncid)
	if (test .NE. nf_noerr) then
		write(*,*)'1'
		write(*,*) nf_strerror(test)
		stop
	endif
test=nf_inq_varid(ncid,'vo',rhid)
	if (test .NE. nf_noerr) then
		write(*,*)'2'
		write(*,*) nf_strerror(test)
		stop
	endif
	test=nf_get_var_real(ncid,rhid,Vt)
		if (test .NE. nf_noerr) then
		write(*,*)'3'
		write(*,*) nf_strerror(test)
		stop
	endif

test=nf_get_att(ncid, rhid, 'add_offset', add_offset1)
	if (test .NE. nf_noerr) then
		write(*,*)'4'
		write(*,*) nf_strerror(test)
		stop
	endif
	
	test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor1)
		if (test .NE. nf_noerr) then
		write(*,*)'5'
		write(*,*) nf_strerror(test)
		stop
	endif

test=nf_close(ncid)
	if (test .NE. nf_noerr) then
		write(*,*)'6'
		write(*,*) nf_strerror(test)
		stop
	endif


V(1:720,:,1)=Vt(3601:4320,241:1201,18,1)*scale_factor1+add_offset1
V(1:720,:,2)=Vt(3601:4320,241:1201,25,1)*scale_factor1+add_offset1
V(721:1081,:,1)=Vt(1:361,241:1201,18,1)*scale_factor1+add_offset1
V(721:1081,:,2)=Vt(1:361,241:1201,25,1)*scale_factor1+add_offset1
open(7,file='/Volumes/south_pacific/V/V_'//datenow//'.dat')
do k=1,ll
     do j=1,lj
          do i=1,li
               if (abs(V(i, j,k)).GE.5.) V(i,j,k)=0.
          end do 
     end do
end do
write(7,*)(((V(i,j,k),i=1,li),j=1,lj),k=1,ll)
close(7)

	


end do
! close(4)


! test = nf_inq_varid(ncid,'longitude',rhid)
! 	if (test .NE. nf_noerr) then
! 		write(*,*)'12'
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if
! test=nf_get_var(ncid,rhid,lont)
! 	if (test .NE. nf_noerr) then
! 		write(*,*)'13'
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if
! lon(:)=lont(3361:4000,1)

! test = nf_inq_varid(ncid,'latitude',rhid)
! 	if (test .NE. nf_noerr) then
! 		write(*,*)'12'
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if
! test=nf_get_var(ncid,rhid,latt)
! 	if (test .NE. nf_noerr) then
! 		write(*,*)'13'
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if
! lat(:)=latt(800:1321,1)



! test=nf_close(ncid)
! 	if (test .NE. nf_noerr) then
! 		write(*,*)'14'
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if

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