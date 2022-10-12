!     particle tracking made by hinata
!     めっちゃ頑張って作ったわ　(;´・д・)=3ハァ
!     基本的な内容は作成工程を記した「作業ログにある」
!     流す地点数が多すぎると、なんか俺のmacとかjamstecのsurfaceでは落ちるみたいだから気をつけること。1500個で死亡した
!     FORP用に設計しているので、FORAだと変数等の仕様に色々問題があるような気がする・・・
!     滝川先生の時みたいに、沿岸をヌルヌル動く仕様にする方法がわからん。一応これでいいけど・・・
!     hinata_range.f90, sabun_hinata8.f90,
!     /Users/hinatazyunpei/Desktop/particleの中に作成したサブルーチン類が入っている
!  /Users/hinatazyunpei/Desktop/particle/kansei_part/NC_yomikomi_kansei_lonlat.f90(石川さんのncファイル読み込みプログラムの改造版)


     program particle_tracking10
    implicit none
! 探す時
! コンパイル

!    hinata_pc
!    gfortran particle_tracking10.f90  -L/usr/local/Cellar/netcdf/4.7.4_1/lib -lnetcdff

!    research_room_imac


!     shokichi.txtはこの規格で作成
!     i5,2x,f10.5,2x,f10.5
	  

!	  real inilon(2),inilat(2)
	  
	  integer day, daytime
	  
	  real u(826,1194),v(826,1194)
	  
	  real lon(1194),lat(826)
	  
	  
      real  uf(826,1194),vf(826,1194)
      real  us(826,1194),vs(826,1194)
      character u_name1*40
      character u_name2*40
      character v_name1*40
      character v_name2*40
      integer im,jm
	 
      integer keisankaisu

       real pi

!     sabun5で出てきたもの

!       サブルーチン内部のもの
!       ini_latとかは配列を無限大という設定にしてみては?
        real,allocatable :: ini_lat(:), ini_lon(:)
        integer,allocatable :: ryu(:)
        integer ryu_num
     
        character*5 keisan_name
        integer aaa
        integer ryu_num2

!       流速算出のためのパラメータ
        real disx,disy,grddisx,grddisy
        real grd_lon(826), grd_lat(1194)
        integer grd1,grd2
        real aa,bb,cc

        real,allocatable :: nowlat(:,:,:)
        real,allocatable :: nowlon(:,:,:)
        integer par_num
        integer xx,yy,lonnum1,lonnum2,latnum1,latnum2,swich1
        real a1,a2,b1,b2
        real u1,u2,u3,u4,v1,v2,v3,v4
        real up,uq, vp,vq, uave,vave, uave2,vave2

!       地点を求めるためのパラメータ
        real,allocatable :: kyorilon, kyorilat
        real sekidokyori, idokyori
        real IDSX,IDSY
        real nowkyori_x,nowkyori_y
        real,allocatable :: nexlon(:,:,:)
        real,allocatable :: nexlat(:,:,:)

!       track7以降で作成したパラメータ
        integer nowday,secday,nnday
        character*97 pathname_u, pathname_v  !ここは可変
        character*4 year
        character*2 month
        integer start_date, finish_date

!       track8以降で
        real kakusan_x, kakusan_y

!       track9以降で
        integer ryusisu,sikou

!       track10での処理
        integer  slip


     	  pi=3.1415
	  
    !   pathの位置 'hist_u_'　まで入れてくれ
        pathname_u='/Users/hinatazyunpei/mount1/FORP-JPN02_version2/IPSL-CM5A-MR_hist_DS_ver2/r199101/nc/2005/hist_u_'
        pathname_v='/Users/hinatazyunpei/mount1/FORP-JPN02_version2/IPSL-CM5A-MR_hist_DS_ver2/r199101/nc/2005/hist_v_'
        
!       文字列の文字数が知りたい時(linuxのコマンド)
!       echo -n '/Users/hinatazyunpei/mount1/FORP-JPN02_version2/IPSL-CM5A-MR_hist_DS_ver2/r199101/nc/2005/hist_u_ ' | wc -m


        year='2005'
        month='04'
        start_date=1
        finish_date=30

!        u_name1='/Volumes/HDCA-UT/data/hist_u_20051210.nc'
!        u_name2='/Volumes/HDCA-UT/data/hist_v_20051210.nc'
!        v_name1='/Volumes/HDCA-UT/data/hist_u_20051211.nc'
!        v_name2='/Volumes/HDCA-UT/data/hist_v_20051211.nc'



!       一箇所に配置する粒子数
        ryusisu=10

!       境界で反射させる
        slip=1
!       境界で止める(個人的に使う気は無いので作成しない)
!       slip=2


!     一日、24時間、で1時間ずつ
      day=10
	  daytime=day*24




!      粒子追跡ループ

       do keisankaisu=1,daytime

        nnday=mod(keisankaisu,24)

!       1日経った時の条件(nnday=0)
       if(nnday.EQ.1)then
!        nday=nday+1

        if(keisankaisu.EQ.1)then
         nowday=start_date
        end if

        if(keisankaisu.NE.1)then
         nowday=nowday+1
        end if

         secday=nowday+1

!       おかしい日数（32日目とか・・・）を指定した際はここでなんとかする
        if(secday.EQ.finish_date+1)then
          go to 1211
        end if
          
          write(6,*) nowday,secday,'xxx'
        call yomikomi(pathname_u,pathname_v,year,month,nowday,secday,uf,vf,us,vs)

         write(6,*) uf(1,1), vf(2,2), us(1,1), vs(4,4)
       end if

      


        write(6,*) 'vvvv', keisankaisu
!　　　　流速の読み
        call ryusoku(uf,vf,us,vs,u,v,keisankaisu)
          write(6,*) 'ssssss', keisankaisu
  !      write(6,*)keisankaisu, u(1,1),v(1,1)


 !       call sabun5(keisankaisu,daytime,u,v,pi,ryu_num)

!       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       !!!!!!!!!!!以下、sabun5の内容!!!!!!!!!!!!!!!!!!!!!!
          disx=150.05-122.55
          disy=47.53-23.67
          grddisx=disx/(826-1)
          grddisy=disy/(1194-1)

          grd_lon(1)=122.55
          grd_lat(1)=23.67

           do grd1=2,826
              grd_lon(grd1)=grd_lon(grd1-1) + grddisx
           end do

           do grd2=2,1194
              grd_lat(grd2)=grd_lat(grd2-1) + grddisy
           end do

 !          write(6,*) grd_lon(1),grd_lon(826),grd_lat(1),grd_lat(1194)



!              aaa=0
!         do keisankaisu=1,24
              aaa=keisankaisu
          write(6,*) 'start_keisankaisu=',aaa

!     t=1の時は読み込み結果、t=2以降は値を持ち越すイメージ
        if(keisankaisu.EQ.1)then

!     　初期位置の読み(このファイルは、粒子番号、lat,lonをまとめたファイル)
        open(101,file='shokichi.txt',status='unknown')

!       初期(t=1)の時の記入すべきファイル
         write(keisan_name,'(i5.5)') aaa
!        open(110,file='shokichi'//keisan_name//'.txt',status='unknown')
!        open(110,file='shokichi1.txt',status='unknown')


!       ここはallocateのためだけにやっている。aaaがなんやかんや役に立ってて草生えるwww
         if(aaa.EQ.1)then
          ryu_num2=1
912      read(101,*,end=911) aa,bb,cc
          ryu_num2=ryu_num2 +1
         go to  912
  
911      ryu_num2=ryu_num2 -1
        close(101)
         end if

        allocate(ini_lat(ryu_num2), ini_lon(ryu_num2), ryu(ryu_num2))


        open(100,file='shokichi.txt',status='unknown')
         ryu_num=1
!       ryuは粒子番号,ryu_numはその番号
1001   read(100,'(i5,2x,f10.5,2x,f10.5)',end=1000) ryu(ryu_num),   ini_lon(ryu_num), ini_lat(ryu_num)
         
 !      write(6,*) ryu(ryu_num),   ini_lat(ryu_num), ini_lon(ryu_num)

    !   keisankaisuが1の時のファイルを作成した

!        write(110,'(i5,2x,f10.5,2x,f10.5)') ryu(ryu_num), ini_lat(ryu_num), ini_lon(ryu_num)
     
        ryu_num=ryu_num+1

        go to 1001

 1000   ryu_num=ryu_num-1

 !       write(6,*) ryu_num,'ok'

         close(100)
 !        close(110)

           end if


 !      allocate(kyorilon(ryu_num), kyorilat(ryu_num))
!      ここは分かり次第入れる感じ？

!     allocateな部分
      if( .not. allocated(nowlon) ) then
       allocate(nowlon(ryu_num,daytime,ryusisu))
      end if
 
      if( .not. allocated(nowlat) ) then
       allocate(nowlat(ryu_num,daytime,ryusisu))
      end if

      if( .not. allocated(nexlon) ) then
       allocate(nexlon(ryu_num,daytime,ryusisu))
      end if

      if( .not. allocated(nexlat) ) then
       allocate(nexlat(ryu_num,daytime,ryusisu))
      end if




!        粒子ひとつひとつのこと(粒子ひとつごとのループ)
         do par_num=1,ryu_num

!         一箇所から大量の粒子を流す
          do sikou=1,ryusisu

!      t=1の時
        if(keisankaisu.EQ.1)then

!        この書き方はありみたい,あとごめん、ここなんか逆にして書いてしまっった
         nowlat(par_num,keisankaisu,sikou)=ini_lat(par_num)
         nowlon(par_num,keisankaisu,sikou)=ini_lon(par_num)
        write(6,*) nowlat(1,1,1),nowlon(1,1,1), par_num,keisankaisu

        end if

!      以降t=2以降の時
        if(keisankaisu.GE.2)then
         nowlat(par_num,keisankaisu,sikou)=nexlat(par_num,keisankaisu-1,sikou)
         nowlon(par_num,keisankaisu,sikou)=nexlon(par_num,keisankaisu-1,sikou)
    !    write(6,*) nowlat(1,1),nowlon(1,1), par_num,keisankaisu
        end if
 

!       書き込み用ファイルを開ける
!       5桁で表示し、なければ前に0を追加する
!       あと、ここwrite文な
         write(keisan_name,'(i5.5)') aaa

!       ファイルを開く（この中で、差分方程式を解く予定）
        open(120,file='shokichi'//keisan_name//'.txt',status='unknown')


          swich1=0
!        粒子の場所を特定し、その周辺の格子点を特定
           do xx=1,826
            if(grd_lon(xx).LE.nowlon(par_num,keisankaisu,sikou).AND.nowlon(par_num,keisankaisu,sikou).LE.grd_lon(xx+1))then
             do yy=1,1194
              if(grd_lat(yy).LE.nowlat(par_num,keisankaisu,sikou).AND.nowlat(par_num,keisankaisu,sikou).LE.grd_lat(yy+1))then
  !       格子点番号
              lonnum1=xx
              lonnum2=xx+1
              latnum1=yy
              latnum2=yy+1
              swich1=1
   
             end if
  !      発見し次第、doループを抜け出す（時間短縮）
            if(swich1.EQ.1) exit
             end do

           end if
            if(swich1.EQ.1) exit
            end do

   !     write(6,*) xx,yy , 'bb'
 !       write(6,*) lonnum1, lonnum2,'aa'

!       ここの確認は超絶大切
  !      write(6,*) grd_lon(lonnum1), grd_lon(lonnum2)
   !     write(6,*) grd_lat(latnum1), grd_lat(latnum2)

!       ここまではOKs


!         ここで問題が出ている
           a1=nowlon(par_num,keisankaisu,sikou) - grd_lon(lonnum1)
           a2=grd_lon(lonnum2) - nowlon(par_num,keisankaisu,sikou)
           b1=grd_lat(latnum2) - nowlat(par_num,keisankaisu,sikou)
           b2=nowlat(par_num,keisankaisu,sikou) - grd_lat(latnum1)

!           write(6,*) 'aaa'

!        ここ、流速u,vはサブルーチン外から抜いてくること
!        四方の流速の定義
         u1=u(lonnum1,latnum1)
         u2=u(lonnum2,latnum1)
         u3=u(lonnum1,latnum2)
         u4=u(lonnum2,latnum2)
         v1=v(lonnum1,latnum1)
         v2=v(lonnum2,latnum1)
         v3=v(lonnum1,latnum2)
         v4=v(lonnum2,latnum2)

!        重み流速の算出(右辺は割合になるはず)
         up=u1*a1/(a1+a2) + u2*a2/(a1+a2)
         uq=u3*a1/(a1+a2) + u4*a2/(a1+a2)
         uave=b1*uq/(b1+b2) + b2*up/(b1+b2)

         vp=v1*a1/(a1+a2) + v2*a2/(a1+a2)
         vq=v3*a1/(a1+a2) + v4*a2/(a1+a2)
         vave=b1*vq/(b1+b2) + b2*vp/(b1+b2)

!      km/hに戻した（元々はcm/s）
         uave2=uave*3600/100/1000
         vave2=vave*3600/100/1000

!       uave,vaveが異常値を取ったら、そこを境界と定義して動作させる
!         yomikomiのサブルーチンより、異常値は9999999に設定、よって999を境目に考える

         if(uave.GE.999..OR.vave.GE.999.)then
           if(slip.EQ.1)then
             nexlon(par_num,keisankaisu,sikou)=nowlon(par_num,keisankaisu,sikou)
             nexlat(par_num,keisankaisu,sikou)=nowlat(par_num,keisankaisu,sikou)
              go to 10012
           end if

!           if(slip.EQ.2)then

         end if


         write(6,*) 'uave2=',uave2,'vave2',vave2

!      以上、平均流速の算出
!      以下、地点の算出


        kyorilon=nowlon(par_num,keisankaisu,sikou)-122.55
        kyorilat=nowlat(par_num,keisankaisu,sikou)-23.67
!         write(6,*) kyorilon,kyorilat

!       緯度軽度の計算
!       http://blog.netandfield.com/shar/2014/04/post-1789.html
!       経度１度で111.111km,
         pi=3.1415

        sekidokyori=pi*6378.137*2/360
        idokyori=6356.752*2*pi/360
!        IDSXはnowlatの時の距離
        IDSX=sekidokyori*cos(nowlat(par_num,keisankaisu,sikou)*2*pi/360.)
        IDSY=idokyori

   !     write(6,*) IDSX,IDSY

!       拡散（距離換算）をここで投入
        call random_kakusan(kakusan_x,kakusan_y)


        nowkyori_x=IDSX*kyorilon + uave2 + kakusan_x
        nowkyori_y=IDSY*kyorilat + vave2 + kakusan_y

        nexlon(par_num,keisankaisu,sikou)=nowkyori_x/IDSX +  122.55
        nexlat(par_num,keisankaisu,sikou)=nowkyori_y/IDSY +  23.67

     !  ここで、グリッドをまたぐことになるかもしれんが、それは上で調整されるので問題ない？

10012    write(120,'(i5,2x,f10.5,2x,f10.5,2x,i2,2x,i2)') par_num,&
         nexlon(par_num,keisankaisu,sikou),&
         nexlat(par_num,keisankaisu,sikou), 35, 35

 !       write(120,*) par_num, nexlon(par_num,keisankaisu), nexlat(par_num,keisankaisu), '35', '35'

 !          write(6,*) nexlon(par_num,keisankaisu),nexlat(par_num,keisankaisu)

        if(par_num.EQ.ryu_num)then
         close (120)
         exit
        end if

       end do ! sikou(複数試行)のループ

       end do !par_numのdoループ

       write(6,*) 'owari_keisankaisu=', keisankaisu

!     !!!!!!! sabun5の終了 !!!!!!!!!
!     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       if(keisankaisu.EQ.daytime)then
        write(6,*) 'bbbb'
       end if

       end do

!     中身はkari.f90にしまっておいた

1211    if(secday.EQ.finish_date+1) write(6,*) 'nissugaokasii'


      stop
      end





        subroutine ryusoku(uf,vf,us,vs,u,v,keisankaisu)
!       implicit none
!       uf,vfが前,us,vsが後
        real uf(826,1194),vf(826,1194)
        real us(826,1194),vs(826,1194)
        real u(826,1194),v(826,1194)
        integer i, j, t
        real di_u(826,1194),di_v(826,1194)
        integer sikou

         t=mod(keisankaisu,24)
          
 !         if(keisankaisu.EQ.24) write(6,*) 'eeeee'

        do i=1,826
         do j=1,1194

!         tの配列に意味はない、ただ、区別しといたほうがいいというおまじない
!         tを入れるとt＝0の時に変になるからここの配列は二次元にしてくれ
          di_u(i,j)=us(i,j)/24.-uf(i,j)/24.
          di_v(i,j)=vs(i,j)/24.-vf(i,j)/24.
!         if(keisankaisu.EQ.24) write(6,*) 'eeeee'

         !始まりのときはt=1で、これは0時と定義
          if(t.EQ.1)then
           u(i,j)=uf(i,j)
           v(i,j)=vf(i,j)
          end if

         !その次からは補完していく,t=0(余り0の時)の時は23時の定義になる
          if(t.NE.1)then
           u(i,j)=u(i,j)+di_u(i,j)
           v(i,j)=v(i,j)+di_v(i,j)
          end if

          if(t.EQ.0)then
           u(i,j)=us(i,j)
           v(i,j)=vs(i,j)
          end if



          end do
         end do

        write(6,*) t, u(1,1), v(1,1),keisankaisu

!      以降、u,vが線形補完した流速
 !       return
       end subroutine ryusoku
      





       
      subroutine yomikomi(pathname_u,pathname_v,year,month,nowday,secday,uf,vf,us,vs)
       implicit none

!      大きな方のmacの場合
!      include '/usr/local/Cellar/netcdf/4.7.4_2/include/netcdf.inc'
!      俺のmacの場合
       include  '/usr/local/Cellar/netcdf/4.7.4_1/include/netcdf.inc'
       character pathname_u*97
       character pathname_v*97
       character year*4
       character month*2
       integer start_date, finish_date
       integer nowday,secday

       character u_name1*108
       character u_name2*108
       character v_name1*108
       character v_name2*108
       character*2 start_date2
       character*2 sec_date2

!      おそらくここの配列の読みは問題ない
       real uf(826,1194),vf(826,1194)
       real us(826,1194),vs(826,1194)

       real add_offset1, scale_factor1, add_offset2, scale_factor2
       real add_offset3, scale_factor3, add_offset4, scale_factor4
!      なぜか、ここ、sla1とするとエラーを吐くので何もつけない”sla”を一つ作る必要がある
       real sla(826,1194,54,1)
       real sla2(826,1194,54,1)
       real sla3(826,1194,54,1)
       real sla4(826,1194,54,1)

       character  VAR1*2, VAR2*2, VAR3*2, VAR4*2
       integer  ncid, test, rhid, i, j, var_varid
	   
	   integer grd1,grd2,LAT,LON,DP
	   real grd_lat, grd_lon
	   

       VAR1="uo"
       VAR2="vo"

       VAR3="uo"
       VAR4="vo"

        LAT=826
        LON=1194
        DP=54


       
        write(start_date2,'(i2.2)') nowday
        write(sec_date2,'(i2.2)') secday

        write(6,*) nowday, secday
        write(6,*) start_date2, sec_date2, 'ccc'

        u_name1=pathname_u//year//month//start_date2//'.nc'
        u_name2=pathname_u//year//month//sec_date2//'.nc'
        v_name1=pathname_v//year//month//start_date2//'.nc'
        v_name2=pathname_v//year//month//sec_date2//'.nc'

       write(6,*) u_name1


!     流速の読み(1日目)
!     u_name1(ufの読み込み)
      test=nf_open(u_name1,nf_nowrite,ncid)
	   if(test.ne.nf_noerr)then
	      write(6,*) nf_strerror(test)
		  write(6,*) '1'
	   end if
	  test=nf_inq_varid(ncid,'uo',rhid)
	   if(test.ne.nf_noerr)then
	      write(6,*) nf_strerror(test)
          write(6,*) '2'
	   end if
      test=nf_get_att(ncid, rhid, 'add_offset', add_offset1)
	   if(test.ne.nf_noerr)then
	      write(6,*) nf_strerror(test)

	   end if
      test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor1)
	   if(test.ne.nf_noerr)then
	      write(6,*) nf_strerror(test)
	   end if
      test=nf_get_var(ncid,rhid,sla)
	   if(test.ne.nf_noerr)then
	      write(6,*) nf_strerror(test)
	   end if
      test=nf_close(ncid)


!     v_name1(vfの読み込み)
      test=nf_open(v_name1,nf_nowrite,ncid)
      test=nf_inq_varid(ncid,VAR2,rhid)
      test=nf_get_att(ncid, rhid, 'add_offset', add_offset2)
      test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor2)
      test=nf_get_var(ncid,rhid,sla2)
      test=nf_close(ncid)

!     u_name2(usの読み込み)
      test=nf_open(u_name2,nf_nowrite,ncid)
      test=nf_inq_varid(ncid,VAR3,rhid)
      test=nf_get_att(ncid, rhid, 'add_offset', add_offset3)
      test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor3)
      test=nf_get_var(ncid,rhid,sla3)
      test=nf_close(ncid)


!     v_name2(vsの読み込み)
      test=nf_open(v_name2,nf_nowrite,ncid)
      test=nf_inq_varid(ncid,VAR4,rhid)
      test=nf_get_att(ncid, rhid, 'add_offset', add_offset4)
      test=nf_get_att(ncid, rhid, 'scale_factor', scale_factor4)
      test=nf_get_var(ncid,rhid,sla4)
      test=nf_close(ncid)


     do i=1,LAT
         do j=1,LON
            uf(j,i)=sla(j,i,1,1)*scale_factor1+add_offset1
            vf(j,i)=sla2(j,i,1,1)*scale_factor2+add_offset2
            us(j,i)=sla3(j,i,1,1)*scale_factor3+add_offset3
            vs(j,i)=sla4(j,i,1,1)*scale_factor4+add_offset4

!       異常値を9999に飛ばす（表層のみ）
          if (uf(j,i)==-9.98999971E+33) then
             uf(j,i)=9999999
          end if
          if (vf(j,i)==-9.98999971E+33) then
             vf(j,i)=9999999
          end if
          if (us(j,i)==-9.98999971E+33) then
             us(j,i)=9999999
          end if
          if (vs(j,i)==-9.98999971E+33) then
             vs(j,i)=9999999
          end if
       enddo
!     write(*,*) i

      enddo

!      出力はここに記入
!        write(6,*) uf

!     一応、上には54層で戻すけど、

       return
 !     end subroutine yomikomi

 !    stop

    

     end subroutine yomikomi




     subroutine random_kakusan(kakusan_x,kakusan_y)
      implicit none
      real :: x
      real :: pi
      real :: rad1,rad2,sya

!     拡散係数をどの値で与えるか？
      real :: kakusan
      real :: kakusan_x, kakusan_y

!     拡散係数=1m2/s~100m2/s   間をとって50くらいでやってみ・・・
       kakusan=100.*60.*60./10**6

       call random_number(x)

         pi=3.1415

        rad1=cos(2*x*pi)
        rad2=sin(2*x*pi)

        sya=((rad1)**2+(rad2)**2)**0.5

        kakusan_x=rad1*kakusan
        kakusan_y=rad2*kakusan

!      print *, rad1,rad2,sya,kakusan
!      print *, kakusan_x,kakusan_y
!     syaが１になっていれば完璧

       end subroutine random_kakusan

