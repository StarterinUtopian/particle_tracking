program track_data
	integer,parameter::NPCL=10000   !投入粒子数
	integer,parameter::NSTART=12,NDAY=200,MAXKAI=14400!計算
	character:: startdate(NSTART)*8,dateyear(NDAY)*8,day*4
	real::RPCLX,RPCLY
	integer::YS,YI
	integer::I,J,K,tmp,a,b,c



	open(7,file='/Volumes/erika/CMEMS/set_input/start-timing/start.txt')
	read(7,'(A8)')(startdate(YS),YS=1,NSTART)
	close(7)
	do YS=1,NSTART
		open(8, file = '/Volumes/erika/CMEMS/output/'//startdate(YS)//'.dat')
		do i=1,((MAXKAI+1)*NPCL)
			read(8,'(I6,1X,I6,1X,I6,1X,F11.6,1x,F11.6)')a,b,c,RPCLX,RPCLY
			if(MOD(a,72)==0)then
				if (MOD((i-1),NPCL)==0)then
					tmp=INT(a/72)
write(day,'(I3.3)')tmp

					open(9, file = '/Volumes/erika/CMEMS/output/points/'//startdate(YS)//&
						'.dat',STATUS='unknown')
				end if 
				! write(*,*)'b=',b
				! write(*,*)'j=',j
				! write(*,*)'k=',k
				write(*,'(I6,1X,I6,1X,I6,1X,F11.6,1x,F11.6)')a,b,c,RPCLX,RPCLY
				write(9,'(F11.6,1x,F11.6,1x,I3.3)')RPCLX,RPCLY,c

			end if
		end do
		close(8)
		close(9)
	end do
end program

