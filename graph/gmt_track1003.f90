program track_data
	integer,parameter::NPCL=10000   !投入粒子数
	integer,parameter::NSTART=12,NDAY=200,MAXKAI=14400!計算
	character:: startdate(NSTART)*8,dateyear(NDAY)*8
	real::RPCLX,RPCLY
	integer::YS,YI
	integer::I,J,K,P,a,b,c



	open(7,file='/Volumes/erika/CMEMS/set_input/start-timing/start.txt')
	read(7,'(A8)')(startdate(YS),YS=1,NSTART)
	close(7)
	do YS=1,6
		open(9,file='/Volumes/erika/CMEMS/output/track/'//startdate(YS)//'_TRACK.dat')
		j=1
		k=0
		write(9,*)'>'
		do P=1,NPCL
			open(8, file = '/Volumes/erika/CMEMS/output/'//startdate(YS)//'.dat')
			do i=1,((MAXKAI+1)*NPCL)
				read(8,'(I6,1X,I6,1X,I6,1X,F11.6,1x,F11.6)')a,b,c,RPCLX,RPCLY
				if(K==MAXKAI+1)then
						j=j+1
						K=0
						write(9,*)'>'
					else if(b==j)then
					! write(*,*)'b=',b
					! write(*,*)'j=',j
					! write(*,*)'k=',k
					write(*,'(I6,1X,I6,1X,I6,1X,F11.6,1x,F11.6)')a,b,c,RPCLX,RPCLY
					write(9,'(F11.6,1x,F11.6)')RPCLX,RPCLY
					K=K+1
				end if
			end do
			close(8)
		end do
		close(9)
	end do
end program

