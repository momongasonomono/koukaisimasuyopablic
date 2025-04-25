!----------------------------------------------------------------------
! プログラム名: example5
! 概要:
!   このプログラムは、入力ファイル(input2.dat)からデータを読み込み、
!   行列とベクトルの計算を繰り返し実行し、結果を出力ファイル(output2.dat)に
!   書き込むプログラムです。
!
! 入力:
!   - input2.dat:
!       1行目: コメント行
!       2行目: 最大繰り返し回数 imax
!       3行目: コメント行
!       4行目: 出力間隔 iout
!       5行目: コメント行
!       6行目: 行列・ベクトルのサイズ nx
!       7行目: コメント行
!       8行目以降: nx x nx の行列データ (aa)
!       8+nx行目以降: nx のベクトルデータ (bb)
!
! 出力:
!   - output2.dat:
!       繰り返し計算の結果を出力。iout間隔で以下を記録:
!       - 現在のステップ数 istep
!       - ベクトル cc の内容 (フォーマット f10.1)
!
! サブルーチン:
!   - calcu(nx, aa, bb, cc, md1):
!       行列 aa とベクトル bb を用いてベクトル cc を計算します。
!       cc(i) = Σ(aa(i, j) * bb(j)) (j=1からnxまで)
!
! 変数:
!   - imax: 最大繰り返し回数
!   - iout: 出力間隔
!   - nx: 行列・ベクトルのサイズ
!   - aa: 入力行列 (nx x nx)
!   - bb: 入力ベクトル (nx)
!   - cc: 計算結果ベクトル (nx)
!   - istep: 現在の繰り返しステップ
!   - ww: 計算用一時変数
!
! 注意:
!   - 入力ファイルのフォーマットに従ってデータを準備してください。
!   - nx は md1 以下である必要があります。
!----------------------------------------------------------------------
program example5
    parameter(md1=100)
    !フォーマット222は右揃え10桁で小数点以下1桁
    222 format(f10.1)

    implicit double precision (a-h,o-z)
    dimension aa(md1,md1),bb(md1),cc(md1)

    open(10,file='input2.dat')
    open(11,file='output2.dat')

!2行目，4行目，6行目の数字をimax,iout,nxに書き込む
    read(10,'(a80)')
    read(10,*) imax
    read(10,'(a80)')
    read(10,*) iout
    read(10,'(a80)')
    read(10,*) nx
!8行目以降のデータを配列aaに書き込む
    read(10,'(a80)')
    read(10,*)((aa(i,j),i=1,nx),j=1,nx)
!10+nx行目以降のデータをベクトルbbに書き込む
    read(10,'(a80)')
    read(10,*)(bb(i),i=1,nx)
!カウンタistepを1からimaxまで利用して繰り返す
    do istep=1,imax
        !関数calcuを呼び出し,nx,aa,bb,cc,md1を使って計算させる
        call calcu(nx,aa,bb,cc,md1)
        !istepをioutで割ったときの剰余が0のとき
        if(mod(istep,iout).eq.0)then
            !output2.datに'istep="変数istepの値"を書き込む
            write(11,*)'istep=',istep
            !カウンタiを1からnxまで利用して繰り返す
            do i=1,nx
                !output2.datにベクトルccのi成分をフォーマット222で書き込む
                write(11,222)cc(i)
            end do
        !剰余が0でないときなにもしない
        else
        end if
        !カウンタiを1からnxまで利用して繰り返す
        do i=1,nx
            !ベクトルccのi成分をベクトルbbのi成分に代入
            bb(i)=cc(i)
        end do
    end do
end program example5



    subroutine calcu(nx,aa,bb,cc,md1)
        implicit double precision(a-h,o-z)
        dimension aa(md1,md1),bb(*),cc(*)

        do i=1,nx
            !計算用の変数wwを初期化
            ww=0.d0
            !jをカウンタとして1からnxまで繰り返す
            do j=1,nx
                !wwに行列aaのi,j成分とベクトルbbのj成分をかけたものを足す
                ww=ww+aa(i,j)*bb(j)
            end do
            !ベクトルccのi成分にwwを代入
            cc(i)=ww
        end do
    end subroutine calcu
    