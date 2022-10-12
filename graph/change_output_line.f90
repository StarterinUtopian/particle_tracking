program main
	implicit none
	integer,parameter::NPCL=1000,NDAY=150
	integer :: NI,IP,NNDAY,i
	real:: RPCLX(NPCL),RPCLY(NPCL)
	open(4,file='/Volumes/erika/CMEMS/output/test.dat')
	ni=0
	do i=1,NDAY
		if (NI.LT.100) then
			write(NNDAY,'(A1)')'0',NI
		else
			write(NNDAY,'(A3)')NI
		end if
			open(7,file='/Volumes/erika/CMEMS/output/20180101/'//NNDAY//'.dat')
				ni=ni+1
				read(4,'(I6,1X,I6,1X,F11.2,1x,F11.2)')(NI,IP,RPCLX(IP),RPCLY(IP),IP=1,NPCL)
				write(7,'(F11.2,1x,F11.2)')(RPCLX(IP),RPCLY(IP),IP=1,NPCL)
		end do
	end program main
