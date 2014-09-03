GrADS2nc -- GrADS形式のデータから netCDF への変換ツール
====================================================================================================

.. toctree::
   :maxdepth: 2


機能
--------------------------------------------------

GrADS 形式の格子データ (Grid Point Value: GPV) を `CF convention <http://cfconventions.org/>`_ に準拠した `netCDF <https://www.unidata.ucar.edu/software/netcdf/>`_ ファイルに変換する．
変換は3段階で行われる:

#. データのメタ情報を記述したファイルを作成する．
#. メタ情報と実データを元に変換を実行する．
#. 変換したデータが CF convention に準拠しているかをチェックする．

本パッケージでは以下を提供する:

* GrADS description file (*.ctl) を用いたメタ情報の作成支援ツール
* ファイルの変換ツール
* CF convention の compliance checker のインストール方法に関するドキュメント

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


Compliance checker のインストール方法
--------------------------------------------------

CF convention の策定グループから提供されている compliance checker **cfchecks** のインストール方法についてまとめる．

準備
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**cfchecks** には以下のライブラリが必要である:

* `UDUNITS <http://www.unidata.ucar.edu/software/udunits/>`_ (Ver. 2.0 以上)
* netCDF (Ver. 3 以上)
* `CDAT <http://www2-pcmdi.llnl.gov/cdat>`_ (Ver. 5.x)

UDUNITS-2 のインストール
""""""""""""""""""""""""""""""""""""""""""""""""""

Ubuntu の場合は，リポジトリから取得可能::

   $ sudo apt-get install libudunits2-0 liudunits2-dev

リポジトリから取得できない場合は，ソースを取得してインストールする (*todo*)．

CDAT のインストール
""""""""""""""""""""""""""""""""""""""""""""""""""

ここでは，CDAT から描画機能などを削ったコアのみのパッケージである **cdat-lite** を使うことにする．

* easy_install を使うことができる
* netCDF ライブラリがインストール済みである
ことを前提とする．

#. netCDF のインストールディレクトリ，netCDF-4 の場合は HDF5 ライブラリがあるディレクトリを環境変数に設定する ::

      $ export NETCDF_HOME=/usr/local/netcdf/4.3.2/gcc4.8.2
      $ export HDF5_HOME=/usr/lib

   .. note::

      setup.py は /usr/lib, /usr/include を自動で探しに行くので，その場合は環境変数の設定は必要ない．
      また，ライブラリを ${NETCDF_HOME}/lib, ${HDF5_HOME}/lib に，ヘッダファイルを ${NETCDF_HOME}/include, ${HDF5_HOME}/include に探しに行くので，それ以外のディレクトリ構成となっている場合は，シンボリックリンクなどで対応する．

#. ソースファイルを取得する ::

      $ easy_install -eb . cdat-lite==5.2rc1

   .. warning::

      cdat-lite の最新バージョンは 6.0rc2 だが， **cfchecks** が対応しているのは 5.x なので Ver. 5.2rc1 を使用する．

#. コンパイル，インストール ::

      $ python setup.py bdist_egg > install.log 2>&1
      $ sudo easy_install dist/cdat_lite*.egg


cfcheks のインストール
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. `リポジトリ <https://github.com/cf-convention>`_ からソースファイルを取得する ::

      $ git clone https://github.com/cf-convention/repository-cf.git

#. cfcheks のインストール ::

      $ cd ./repository-cf/cf-checker/trunk
      $ sudo python setup.py install

#. XML の table データを準備する ::

      $ cd ./repository-cf/cf-standard-names/trunk/src
      $ mv area-type-table.xml cf-standard-name-table.xml ${CF_TABLES}

      #   以下の環境変数を設定しない場合は，cfchecks のオプションで table データを指定する必要あり
      $ export CF_AREA_TYPES=${CF_TABLES}/area-type-table.xml
      $ export CF_STANDARD_NAMES=${CF_TABLES}/cf-standard-name-table.xml
      #   必要に応じて udunits2.xml ファイルの場所も環境変数に設定する
      $ export UDUNITS=${Path_to_udunits2.xml}

使い方
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

引数にファイル名を指定して **cfchecks** を実行する ::

   $ cfchecks <netCDF_file.nc>


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

