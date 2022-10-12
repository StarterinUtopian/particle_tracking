program density
	implicit none
	integer,parameter::li=551,lj=461
	integer,parameter::NPCL=7500
	integer,parameter::NSTART=156,NDAY=200
	character::startdate(NSTART)*8
	real::lon(li),lat(lj),RPCLX,RPCLY
	integer::num(li,lj)
	integer::i,j,III,JJJ,YS,N,P,IP,YI,NKAI,land


do i=1,li
	lon(i)=100.0+(i-1)*0.1
end do
do j=1,lj
	lat(j)=-15.0+(j-1)*0.1
end do

num(:,:)=0

open(9, file ='/Volumes/erika/CMEMS/set_input/start-timing/start.txt',status='old')
do YS=1,NSTART
read(9,'(A8)')startdate(YS)
write(*,'(A8)')startdate(YS)
open(14,file='/Volumes/erika/CMEMS/output/all/'//startdate(YS)//'.dat',status='old')
open(15,file='/Volumes/erika/CMEMS/output/density/'//startdate(YS)//'.dat',status='replace')

do N=1,201
	do P=1,NPCL
		read(14,'(F11.6,1x,F11.6,1X,I6,1X,I6,1X,I6,1X,I6)')RPCLX,RPCLY,YI,IP,NKAI,land
		! if (N==201)then
			III=int((RPCLX-100.0)/0.1)+1
			JJJ=int((RPCLY+15.0)/0.1)+1
			num(III,JJJ)=num(III,JJJ)+1
		! end if
    end do 
end do

do j=1,lj
	do i=1,li
		if (num(i,j)>1)&
       write(*,'(F11.2,1x,F11.2,1x,I6)') lon(i),lat(j),num(i,j)

end do	
end do
close(14)
close(15)	
end do
close(9)



end program
