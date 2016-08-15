# playground
Playground

## このパブリックリポジトリのプライベートなダウンストリームを作成する方法

空のプライベートリポジトリ (仮にprivとする) を GitHub上に作成し、次を行う

```
git clone git@github.com:wbrxcorp/priv.git
cd priv
git remote add upstream git@github.com:wbrxcorp/playground.git
git fetch upstream
git merge upstream/master
git push -u origin master
```

## アップストリームの変更を取り込むには

```
git fetch upstream
git merge upstream/master
```

## ダウンストリームのカスタマイズ

project/build.propertiesファイルを作成し、そこに name, scalaVersion, version を Javaのproperties形式で書き込む。

## 起動

sbt run で Webインターフェイスが起動する。停止するには Ctrl+Cする(Windowsでは止まらないのでターミナルを閉じる)
sbt console すると REPLに入る。REPLを抜けるには :quit とする。

## プロファイル

scalaパッケージ profiles 以下に各プロファイル名のサブパッケージがあり、起動時にコマンドライン引数で指定できる。省略時はsbtのプロジェクト名が採用される。プロジェクト名のプロファイルが存在しない場合は default が使用される。

```
sbt "run myprofile"
```

## モジュール

scalaパッケージ modules以下に各モジュール名のサブパッケージがあり、直下のModuleオブジェクトがモジュール本体として扱われる。どのモジュールが読み込まれるかはプロファイルの ImportModule#modulesで決定される。

モジュールのinitにはREPL環境のの参照が渡されるので、ここでおせっかいなimportやimplicitの宣言をしておいたりする。

### databaseモジュール

プロファイルの DataSourceDefinition オブジェクトで指定されたデータベースへのアクセスを提供する。scalikejdbcの  sqlリテラルをREPL上から直接使用可能にする。AutoSessionが有効なのでDB接続への参照を取り回さなくて良い

```
scala> sql"select 1".map(_.int(1)).single.apply
```

## Atomの linter-scalacを使う場合

下記コマンドの出力を jee/.classpath として保存する

```
sbt 'export test:fullClasspath'
```

## ensimeを使う場合

```
sbt ensimeConfig
```
