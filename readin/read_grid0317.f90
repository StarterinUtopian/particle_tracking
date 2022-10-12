! use gfortran 
! gfortran read_grid.f90 -L/usr/local/Cellar/netcdf/4.8.1/lib -lnetcdff
!一起读会超容量，上下两部分要分开读
program read_grid
implicit none
include  '/usr/local/Cellar/netcdf/4.8.1/include/netcdf.inc'
integer :: ncid,test,imin,imax,jmin,jmax
integer :: rhid 
integer,parameter :: LN=4320,JN=2041,DN=50,t=1
integer,parameter :: li=640,lj=522,ll=30,tt=1 !dimension lengths
integer i,j,k!三个维度的计数器
integer n!err计数器
real :: lon(li),lat(lj)
real :: lont(LN,1),latt(JN,1),DDt(DN,1),DD(ll)
real :: X_edge,Y_edge
real ::H(li,lj),Ht(LN,JN,1),HH(li,lj,ll),HHt(LN,JN,DN,1)
integer::MASK(li,lj,ll),MASKt(LN,JN,DN,1)!转换后的变量
integer::LEV(li,lj),LEVt(LN,JN,1)!转换后的变量

! ***************************************************************
! n=0
! test = nf_open('/Volumes/CNMES2006-2019/statics/GLO-MFC_001_030_coordinates.nc',nf_nowrite,ncid)
! 	if (test .NE. nf_noerr) then
! 		write(*,*)'1'
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if
! test=nf_inq_varid(ncid,'e3t',rhid)
! 	if (test .NE. nf_noerr) then
! 		write(*,*)'2'
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if
! test=nf_get_var_real(ncid,rhid,HHt)

! 	if (test .NE. nf_noerr) then
! 		write(*,*)'3'
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if
! HH(:,:,:)=HHt(3361:4000,800:1321,1:30,1)


! test = nf_inq_varid(ncid,'depth',rhid)
! 	if (test .NE. nf_noerr) then
! 		write(*,*)'4'
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if
! test=nf_get_var_real(ncid,rhid,DDt)
! 	if (test .NE. nf_noerr) then
! 		write(*,*)'5'
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if
! DD(:)=DDt(1:30,1)


! test=nf_close(ncid)
! 	if (test .NE. nf_noerr) then
! 		write(*,*)'6'
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if

! open(7,file='/Volumes/erika/CMEMS/set_input/grid_coordinates.dat')
! write(7,*)(DD(k), k = 1, ll)
! write(7,*)(((HH(i, j, k), i = 1, li), j = 1, lj),k = 1, ll)
! close(7)

! ********************************************************************************************
test = nf_open('/Volumes/CNMES2006-2019/statics/GLO-MFC_001_030_mask_bathy.nc',nf_nowrite,ncid)
	if (test .NE. nf_noerr) then
		write(*,*)'7'
	   write(*,*) nf_strerror(test)
	   stop
	end if

test = nf_inq_varid(ncid,'deptho',rhid)
	if (test .NE. nf_noerr) then
		write(*,*)'8'
	   write(*,*) nf_strerror(test)
	   stop
	end if
test=nf_get_var_real(ncid,rhid,Ht)
	if (test .NE. nf_noerr) then
		write(*,*)'9'
	   write(*,*) nf_strerror(test)
	   stop
	end if
H(:,:)=Ht(3361:4000,800:1321,1)


test = nf_inq_varid(ncid,'deptho_lev',rhid)
	if (test .NE. nf_noerr) then
		write(*,*)'10'
	   write(*,*) nf_strerror(test)
	   stop
	end if
test=nf_get_var(ncid,rhid, LEVt)
	if (test .NE. nf_noerr) then
		write(*,*)'11'
	   write(*,*) nf_strerror(test)
	   stop
	end if
LEV(:,:)=LEVt(3361:4000,800:1321,1)

test = nf_inq_varid(ncid,'mask',rhid)
	if (test .NE. nf_noerr) then
		write(*,*)'12'
	   write(*,*) nf_strerror(test)
	   stop
	end if
test=nf_get_var(ncid,rhid,MASKt)
	if (test .NE. nf_noerr) then
		write(*,*)'13'
	   write(*,*) nf_strerror(test)
	   stop
	end if
MASK(:,:,:)=MASKt(3361:4000,800:1321,1:30,1)


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



test=nf_close(ncid)
	if (test .NE. nf_noerr) then
		write(*,*)'14'
	   write(*,*) nf_strerror(test)
	   stop
	end if







open(8,file='/Volumes/erika/CMEMS/set_input/grid_mask_bathy.dat',status='replace')

write(8,*)((H(i,j),i=1,li),j=1,lj)
write(8,*)((LEV(i,j),i=1,li),j=1,lj)
write(8,*)(((MASK(i, j, k),i=1,li),j=1,lj),k=1,ll)



close(8)


! open(9,file='/Volumes/erika/CMEMS/set_input/grid_lonlat.dat',status='replace')
! write(9,*)(lon(i), i = 1, li)
! write(9,*)(lat(j), j = 1, lj)
! close(9)

end program

! subroutine handle_err(test,n)
! 	integer,intent(in) ::test
! 	integer,intent(inout)::n
! 	n=n+1
! 	if (test .NE. nf_noerr) then
! 		write(*,*)n
! 	   write(*,*) nf_strerror(test)
! 	   stop
! 	end if
! end subroutine

