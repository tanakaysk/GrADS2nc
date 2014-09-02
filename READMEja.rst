GrADS2nc -- GrADS形式のデータから netCDF への変換ツール
====================================================================================================

.. toctree::
   :maxdepth: 2


機能
--------------------------------------------------

GrADS 形式の格子データ (Grid Point Value: GPV) を `CF convention <http://cfconventions.org/>`_ に準拠した `netCDF <https://www.unidata.ucar.edu/software/netcdf/>`_ ファイルに変換する．
変換は2段階で行われる:

#. データのメタ情報を記述したファイルを作成する．
#. メタ情報と実データを元に変換を実行する．

本パッケージでは以下のツールを提供する:

* GrADS description file (*.ctl) を用いたメタ情報の作成支援
* ファイルの変換


環境
--------------------------------------------------

本パッケージのツールを利用するためには以下の環境が必要である:

* bash
* Python (Ver. 2.6 以上)
* Fortran 2003 規格に対応した Fortran コンパイラ
* 上記のコンパイラで生成された netCDF の Fortran 90 ライブラリ
* `cnst / nclib モジュール <https://github.com/tanakaysk/commonlib.git>`_

準備
--------------------------------------------------

メタ情報作成支援ツールは Python のスクリプトなので，特別な準備は必要ない．

ファイルの変換ツールは Fortran で書かれており，以下の手順でコンパイルする:

#. cnst / nclib モジュールを取得する::

      $ git clone https://github.com/tanakaysk/commonlib.git

   commonlib/env/env.mk.default の netCDF ライブラリのインストール先ディレクトリ $(NC_D) を環境に合わせて書き換える．
#. コンパイルスクリプトを実行する::

   $ ./compile.bash

   #   コンパイルをやり直す場合は次のコマンドを実行して初期化する
   $ ./compile.bash clean

現状ではコンパイラとして gfortran (Ver. 4.4 以降) にのみ対応しているので，その他のコンパイラを利用するときは環境に合わせてファイルを編集・追加する．

* compile.bash 内の変数 COMPILE_F95 を書き換える．
* ncmake.mk.${COMPILE_F95} を作成．
* commonlib/compiler/comiler.mk.${COMPILE_F95} を作成．


使い方
--------------------------------------------------

メタ情報を記述したファイルの作成
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GrADS 形式のファイルに含まれるデータのメタ情報を記述したファイルを作成する．
ファイルは，Fortran で書かれた変換プログラムが読めるように，Fortran の namelist ファイルの形式のテキストファイルである．

GrADS description file がある場合はメタ情報ファイルの作成支援ツールを利用できる::

   $ python preproc.py [GrADS description file 名] [メタ情報ファイル名]

作成されたファイルをもとに，内容を修正・追加し，メタ情報ファイルを準備する．

ファイルの変換
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

メタ情報ファイルが作成されたら，そのファイル名を namelist.ncmake とし，ファイル変換ツールを実行する::

   $ ./ncmake_CF16


ディレクトリ構成
--------------------------------------------------

本パッケージには以下の構成でファイルが格納されている::

   -GrADS2nc
    |  compile.bash            # コンパイルスクリプト
    |  Makefile                # ファイル変換ツールの makefile
    |  ncmake_CF16.f90         # ファイル変換ツールのソースコード
    |  ncmake.mk.gfortran44    # gfortran (Ver. 4.4 以上) 向けのコンパイルオプション設定ファイル
    |  preproc.py              # メタ情報作成支援ツール
    |  README.rst              # このファイル

