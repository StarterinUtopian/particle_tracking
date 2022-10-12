
program happy_birthday20220520! まずは誕生日おめでとうー今日は粒子を追跡しないー
! けど、純くんのために、今までの黒歴史を追跡しようと思って
! 一番重要なプログラムを書いできたー(自分にとってはFortranの練習にもなるww)

	implicit none!とりあえず明確化にしたい！
	parameter,logical::love=.true.!好きです！！大好きです！！！パラメータだから変わらないです！！！！！
	parameter,integer::nday= 726
	real::Erika_,Junpei_!どっちもリアルに存在するものなんだけどー
	character::Erika*5,Junpei*6
	character::date*8!日付だけ

	integer::greet,n

	! さあーはじめましょうか？

! ※※**＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊
! 初見ですーよろしくー😃
! ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊
date='20200401'

greet=associated(Erika,Junpei)!挨拶する時、指針”Erika”が"Junpei"をターゲットとして設定したかをチェック
001	if (greet==.true.)then!もしなかったら挨拶を無限に循環するwww
		continue
	else 
		goto　001
	end if

index(Erika,Junpei)!character "junpei"が初めて"Erika"に現れた位置を計算する
nearest(Erika,Junpei)!なるべくお互いの席を近くにしましょー

! ※※**＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊
! 修士の二年間お世話になった🤗
! ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊
do n=1,nday
	all()
end do



function love(Junpei_)




end program!プログラムの結末があるけど、愛は無限にある！ 覚えといて！



