! 这个程序用来定义所有的沿岸域
program shore
	implicit none
	integer,parameter :: LN=4320,JN=2041,DN=50,t=1
	integer,parameter::li = 640, lj = 522, ll =30!読み込みグリッド数 
     integer,parameter::NPCL=7500  !投入粒子数
     real,parameter::IDSY=9259.25
     real, parameter::pi = 3.1415926535
     real, parameter::X_edge = 100.000000  !読み込むデータのX方向の最西座標
     real, parameter::Y_edge = -13.4166670  !読み込むデータのY方向の最南座標
	real::H(li,lj),lon(LN),lat(JN)
	integer::i,j,k,seashore(LN,JN),lev(li,lj),mask(li,lj,ll)
	character::dateyear(4924)*8,year*4,month*2,datenow*8





!遍历所有格子，然后找出0和1交界的格子
! 先导入所有格子的信息
          open(4,file='/Volumes/erika/CMEMS/set_input/grid_mask_bathy.dat')
          read(4,*)((H(i,j),i = 1, li), j = 1, lj)
          read(4,*)((LEV(i,j),i = 1,li),j = 1, lj)
          read(4,*)(((MASK(i, j, k), i = 1, li), j = 1, lj),k = 1, ll)
          close(4)

          open(7,file='/Volumes/erika/CMEMS/set_input/grid_lonlat.dat')
          read(7,*)(lon(i),i=1,li)
          read(7,*)(lat(j),j=1,lj)
          close(7)

          
! 三层循环遍历所有格子然后定义所有沿岸域

! do j=1,lj
! 		do i=1,li
! 			if(mask(i,j,1)==0)then
! 					if((i>12).and.(i<(li-12)))then
! 					   if((j>12).and.(j<(lj-12)))then
! 						seashore((i-12):(i+12),(j-12):(j+12))=1
! 					   end if
! 					end if
! 			end if
! 		end do
! 	end do

! open(8,file='/Volumes/erika/CMEMS/set_input/shore.dat',status='replace')
! write(8,'(I6)')((seashore(i,j),i=1,li),j=1,lj)
! close(8)

open(9,file='/Volumes/erika/CMEMS/set_input/seashore.dat',status='replace')
do j=1,lj
		do i=1,li
			! if(seashore(i,j)==1)then
			write(9,'(F11.6,1X,F11.6,1x,I6)')lon(i),lat(j),mask(i,j,1)
		! end if
	end do
end do
close(9)

end program

