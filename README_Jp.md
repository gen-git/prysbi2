# PRYSBI2 : crop development simulation

「PRYSBI2」は、地球規模で作物開発モデル（PRYSBI2）をシミュレートすることを可能にします。このプログラムには、シミュレーションモジュールだけでなく、ユーザーがMCMCを使用して作物モデルのパラメーターの確率分布を推定できるモジュールも含まれています。

## Description

基本的な構造は、Sakurai et al. (2014) で説明されています。また、シミュレートできる作物の数は複数ありますが、作物間のモデルの違いは基本的にパラメータ値のみです。したがって、どの作物も基本的なモデル構造を共有します（Program_1.2 / PRYSBI2 / spm_param_fix.f90を参照のこと）。モデルの説明を含む論文は現在レビュー中です。ただし、論文が受理される前に、自由に原稿を提出することができます（たとえば、Okada et al. 2015、Porwollik et al. 2016、Müller et al. 2017、Schew et al. 2019、 Yokohata et al. 2020）。

## Requirement

プログラムを実行するには、PCまたは使用するサーバーにFortran MPIコンパイラをインストールする必要があります。動作チェックはmpif90とifortで行っています。

## Usage

「all_excute.sh」で「val_platform = 1」を選択すると、mpif90を使用したプログラムのmakefileが作成され、コンソールにbash all_excute.shを入力するだけで、実行可能ファイルのコンパイルと実行が自動的に処理されます。

「all_excute.sh」で「val_platform = 2」を選択すると、ifortを使用したプログラムのmakefileが作成され、サーバー（PBS）のコンソールにbash all_excute.shを入力するだけで、ジョブファイルのコンパイルと送信が自動的に処理されます。ジョブ管理アプリケーションとして想定されています）。

環境に応じてコンパイラを変更したい場合は、「/ Program1.2 / Base_settting」ディレクトリ（「base_makefile_pc」または「base_makefile_sv」）のmakefileを変更してください。その上で、「all_excute.sh」を実行すると、PRYSBI2をシミュレートできます。

一部のシミュレーション設定は、「all_excute.sh」で変更できます。変数の説明は、ファイル内のコメントとして記述されています。
* シミュレーション年
* 作物名
* シミュレーション名
* MCMC設定　など

## Path of input data

入力ファイル（ディレクトリ）のパスは、「base_control.sh」で指定されます。このファイルでは、以下を変更する。
* 出力ディレクトリのパス（val_output_dir_ebおよびval_output_dir_nb）
* 気候データのパス（val_posi_wether）
* パラメータディレクトリのパス（val_posi_param）
* 他のデータのパス（data_dir）

パスを変更するときは、「val_output_dir / nb」を除いて、スラッシュの前に必ずバックスラッシュを追加してください。 PCでデータパスを指定する場合は、以下の「＃Data path1」で説明してください。サーバー上のデータパスを指定する場合は、以下の「＃Data path2」で説明する必要があります。

## Input data

入力データは、一部のデータ（CO2データなど）を除いて、ラスター形式で準備する必要があります。フォーマットの基本構造は、4バイトの長さ51200（320 x 160）のベクトルです。ベクトルの開始点は南緯90度・西経0度で、終点は北緯90度・東経0度に配置する必要があります。ラスターデータの空間解像度は1.125度x1.125度です。データファイルに複数のマップが含まれている場合（つまり、ファイルに複数の320 x 160のベクトルデータがある場合）、ベクトルは時間順に相互に接続されます。たとえば、各年の気候データには、320 x 160 x365の長さのベクトルがあります。以下では、このフォーマットを「grdフォーマット」と呼びます。

### Climate data

以下は、作物モデルのシミュレーションに必要な気候データのリストです。
1. 日射量 (MJ/m2): dswrfsfc****.grd
2. 降水量 (mm/day): precsfc****.grd
3. 相対湿度 (%): rhsfc****.grd
4. 平均気温 (degree Celsius): tavesfc****.grd
5. 日最高気温 (degree Celsius): tmaxsfc****.grd
6. 日最低気温 (degree Celsius): tminsfc****.grd
7. 風速 (m/s): windsfc****.grd

### Parameter values

各空間グリッドの（いくつかの）パラメーター値を変更する場合は、パラメーター値のデータファイルをgrd形式として準備する必要があります。複数のアンサンブルをシミュレートする場合は、51200 xアンサンブル長の数のベクトルを持つデータを準備する必要があります。パラメータデータのファイルには、「grd_param_list_all_ [パラメータ番号] _ [作物名] .grd」という名前を付ける必要があります。プログラムで読み取られるパラメーター値は、通常、サブルーチン「parameter_setting」（Program_1.2 / MCMC / Mcalc_llk.f90）で割り当てられます。各グリッドで変更されるパラメーターの数（デフォルトは4）を変更する場合は、サブルーチンの変更に加えて、以下の定数を変更する必要があります。

* avar_param (Program_1.2/Reader/var_read_all.f90)
* val_ndim (all_excute.sh)
* mcmc_setting_[crop name].dat (Program_1.2/Base_setting/)

ファイル「mcmc_setting_ [作物名] .dat」の主な役割は、以前の配布とMCMCの設定を変更することです。このファイルでは、平均と標準偏差などに関して、対象パラメータの事前分布等が変更されます。したがって、シミュレーションにパラメータ値のみを使用する場合は、パラメータの数に応じて次のように行を挿入するだけで済みます：x * -9999 -9999 -9999 -9999 -9999。このファイルは主に以下で説明するMCMCの計算を利用するときに変更します。

## MCMC

作物モデルのいくつかのパラメーターの確率分布を推定したい場合は、以下の変更が必要です。 MCMCの収束には数万のMCMCステップが必要であるため、広い範囲でパラメーター値を推定する場合は、数十（または数百）のコアを持つクラスタリングコンピューティングシステムを使用することをお勧めします。

1. all_excute.sh
	コメントに従って、このファイルに含まれる複数の定数を変更する必要があります。 「val_mcmc_sim」を2に設定した場合、定数「val_nstep」はMCMCステップの数を指定することに注意してください。一方、「val_mcmc_sim」を3に設定した場合、定数はシミュレートされるアンサンブルの数を指定します。「val_ndim」も変更する必要があります。 「val_ndim」は出力の次元数をしていします。
	
2. mcmc_setting_[作物名].dat (Program_1.2/Base_setting/)
	ファイルでは作物ごとに、パラメーターの事前配布の情報を設定する必要があります。 「typeprior」列は、事前配布のタイプを定義します。 事前分布として一様分布を想定している場合は、それに0を割り当て、分布の範囲を定義する必要があります（列「lim（lw）」および「lim（up）」）。 正規分布を想定している場合は、それに1を割り当て、平均と標準偏差を定義する必要があります。 分布の（列「avprior」および「sdprior」）。 定数「dream_b」、「dreambdash」、「dream_delta」は、MCMCの詳細設定です。 したがって、必ずしもそれらを変更する必要はありません。

3. Mcalc_llk.f90 (Program_1.2/MCMC)
	パラメーター設定の概念に従って、このファイルのサブルーチン「parameter_setting」を変更する必要があります。

4. avar_param (Program_1.2/Reader/var_read_all.f90)


## Refefences

Sakurai et al. (2014) How much has the increase in atmospheric CO2 directly affected past soybean production? Scientific Reports.

Okada et al. (2015) Modeling irrigation-based climate change adaptation in agriculture: model development and evaluation in Northeast China. Journal of Advances in Modeling Earth Systems.

Müller et al. (2017) Global gridded crop model evaluation: benchmarking, skills, deficiencies and implications. Geoscientific Model Development.
Porwollik et al. (2016) Spatial and temporal uncertainty of crop yield aggregations. European Journal of Zoology.
