program track_data
     integer,parameter::NPCL=10000   !投入粒子数
     integer,parameter::NSTART=12,NDAY=200,MAXKAI=14400!計算
     character:: startdate(NSTART)*8,dateyear(NDAY)*8
     real,dimension(MAXKAI+1)::RPCLX,RPCLY
     integer::YS,YI
     integer::I,J,K,P,a,b,c
  


open(7,file='/Volumes/erika/CMEMS/set_input/start-timing/start.txt')
read(7,'(A8)')(startdate(YS),YS=1,NSTART)
close(7)
do YS=1,1
	open(9,file='/Volumes/erika/CMEMS/output/'//startdate(YS)//'_TRACK.dat')
	j=1
	k=0
	write(9,*)'>'
do P=1,NPCL
	open(8, file = '/Volumes/erika/CMEMS/output/'//startdate(YS)//'.dat')
	do i=1,((MAXKAI+1)*NPCL)
		read(8,'(I6,1X,I6,1X,I6,1X,F11.6,1x,F11.6)')a,b,c,RPCLX(a+1),RPCLY(a+1)

if(b==j)then
		! write(*,*)'b=',b
		! write(*,*)'j=',j
		! write(*,*)'k=',k
	write(*,'(I6,1X,I6,1X,I6,1X,F11.6,1x,F11.6)')a,b,c,RPCLX(a+1),RPCLY(a+1)
	write(9,'(F11.6,1x,F11.6)')RPCLX(a+1),RPCLY(a+1)
	K=K+1
	if(K==MAXKAI+1)then
	j=j+1
	K=0
	write(9,*)'>'
end if
end if
end do
close(8)
close(9)
end do
end do
end program

