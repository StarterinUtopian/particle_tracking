program FORA_test
	! 読めるようになりました。

!	use netcdf
!       gfortran FORA_Test-3.f90  -L/usr/local/Cellar/netcdf/4.7.4_1/lib -lnetcdff


!   いけたぞおおおおおおおおおお！！！！！！！！


	implicit none

 !        include '/usr/local/Cellar/netcdf/4.7.4_2/include/netcdf.inc'
!    自分のmacの時はこれ
      include  '/usr/local/Cellar/netcdf/4.7.4_1/include/netcdf.inc'

	! NetCDF file 
	character :: FILE_NAME*18
	character :: VAR1*2, VAR2*2
	integer :: ncid, test, rhid, i, j, var_varid
	! Data length
	integer, parameter :: LAT=826, LON=1194 ,DP=54, time=1
	real(4) :: sla(LON,LAT,DP,time)
	real(4) :: V1(LON,LAT,DP,time)
	real :: sla2(LON,LAT,DP)
	real :: U1(LON,LAT,DP)
	real :: add_offset1, scale_factor1, add_offset2, scale_factor2
    integer :: im,jm,zm


 111     format(f10.5)


!	FILE_NAME="/Users/bioocean/mount1/FORP-JPN02_version2/IPSL-CM5A-MR_hist_DS_ver2/r199101/nc/1991/hist_v_19910917.nc"
!        FILE_NAME="/Volumes/HDCA-UT/data/hist_v_20051210.nc"
         FILE_NAME='hist_v_20050920.nc'

	VAR1='vo'
	VAR2='uo'


	! V1 の 取得
	test=nf_open(FILE_NAME,nf_nowrite,ncid)
!    if (test.ne.nf_noerr) call handle_err(test)

    if (test .NE. nf_noerr) then
      write(6,*) nf_strerror(test)
      write(6,*) '1'
      stop
    endif

	test=nf_inq_varid(ncid,'vo',rhid)

    if (test .NE. nf_noerr) then
      write(6,*) nf_strerror(test)
      write(6,*) '2'
      stop
    endif

    test=nf_get_var_real(ncid,rhid,sla)

     if (test .NE. nf_noerr) then
      write(6,*) nf_strerror(test)
      write(6,*) '3'
      stop
     endif


	test=nf_get_att(ncid, rhid, 'add_offset', add_offset1)
	test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor1)
!	test=nf_get_var(ncid,rhid,sla)
	test=nf_close(ncid)
	! 終わり

	! U1 の 取得
!	test=nf_open(FILE_NAME,nf_nowrite,ncid)
!	test=nf_inq_varid(ncid,VAR2,rhid)
!	test=nf_get_att(ncid, rhid, 'add_offset', add_offset2)
!	test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor2)
!	test=nf_get_var(ncid,rhid,sla2)
!	test=nf_close(ncid)
	! 終わり



	! 保存テスト
	open(40,file="f_v.dat",form="unformatted",status="replace")
!	open(41,file="f_u.dat",form="unformatted",status="replace")



	do i=1,LON
		 do j=1,LAT
			V1(i,j,1,1)=sla(i,j,1,1)*scale_factor1+add_offset1
 !           V1(i,j,1,1)=sla(i,j,1,1)
!			U1(j,i,1)=sla2(j,i,1)*scale_factor1+add_offset1
			if (V1(i,j,1,1)==-9.99e+33) then
				V1(i,j,1,1)=9999999
			end if
!			if (U1(j,i,1)==-9.989999866169453e-32) then
!				U1(j,i,1)=9999999
!			end if
	     enddo
!	     write(*,*) i
    enddo

!	    write(6,*) ((V1(i,j,1,1),i=1,LON),j=1,LAT)
        write(6,*) ((sla(i,j,1,1),i=1,LON),j=1,LAT)
!	    write(41) ((U1(j,i,1),j=1,LON),i=1,LAT)

!       write(6,*) add_offset1

 !      write(6,*) sla

	    close (40)

end program FORA_test




subroutine handle_err(test)
  integer :: test

  write(*,*) nf_strerror(test)
  stop

end subroutine handle_err
