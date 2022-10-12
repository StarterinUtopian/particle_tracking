! gfortran read_lonlat.f90 -L/usr/local/Cellar/netcdf/4.8.1/lib -lnetcdff

program main
include  '/usr/local/Cellar/netcdf/4.8.1/include/netcdf.inc'
integer,parameter::LN=4320,JN=2041,DN=50
integer,parameter::li = 640, lj = 522, ll = 30
real :: U(li,lj,ll),V(li,lj,ll),lon(li),lat(lj)
real :: Ut(LN,JN,DN,1),Vt(LN,JN,DN,1),lont(LN,1),latt(JN,1)
real(8) :: add_offset1, scale_factor1,add_offset2, scale_factor2
integer :: ncid,test,rhid,i,j,k
character::file_name*62,outfile*46




file_name='/Volumes/CNMES2006-2019/statics/GLO-MFC_001_030_coordinates.nc'
outfile='/Volumes/erika/CMEMS/set_input/grid_lonlat.dat'


! **********************************************************************************
!                             读入U
! **********************************************************************************
test=nf_open(file_name,nf_nowrite,ncid)
	if (test .NE. nf_noerr) then
		write(*,*)'1'
		write(*,*) nf_strerror(test)
		stop
	endif



test = nf_inq_varid(ncid,'longitude',rhid)
	if (test .NE. nf_noerr) then
		write(*,*)'2'
	   write(*,*) nf_strerror(test)
	   stop
	end if
test=nf_get_var(ncid,rhid,lont)
	if (test .NE. nf_noerr) then
		write(*,*)'3'
	   write(*,*) nf_strerror(test)
	   stop
	end if
lon(:)=lont(3361:4000,1)

test = nf_inq_varid(ncid,'latitude',rhid)
	if (test .NE. nf_noerr) then
		write(*,*)'4'
	   write(*,*) nf_strerror(test)
	   stop
	end if
test=nf_get_var(ncid,rhid,latt)
	if (test .NE. nf_noerr) then
		write(*,*)'5'
	   write(*,*) nf_strerror(test)
	   stop
	end if
lat(:)=latt(800:1321,1)



test=nf_close(ncid)
	if (test .NE. nf_noerr) then
		write(*,*)'6'
	   write(*,*) nf_strerror(test)
	   stop
	end if

open(7,file=outfile,status='replace')
do j=1,lj
  do i=1,li
     write(7,*)lon(i),lat(j)
    end do 
    enddo
close(7)


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