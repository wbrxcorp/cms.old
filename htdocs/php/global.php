<?php
// コンフィグの読み込み
define("CONFIG_PHP", dirname(__FILE__) . "/php/config.php");
if (file_exists(CONFIG_PHP)) { require_once(CONFIG_PHP); }

///////////////////////////////////////////////////////////////////////////
// HTTPのエラーステータスを例外で表現するためのクラス
class HttpErrorStatus extends Exception {}

///////////////////////////////////////////////////////////////////////////
// 例外発生時の処理
function exception_handler($exception) {
    // HTTPエラーステータスの場合
    if ($exception instanceof HttpErrorStatus) {
        $message = sprintf("%d %s", $exception->getCode(), $exception->getMessage());
        header(sprintf("HTTP/1.1 %s", $message));
        echo $message;
        exit;
    }
    // その他の例外の場合は500エラーを返す TODO: XHRの場合はJSONでレスポンスする
    debug_log($exception);
    header("HTTP/1.1 500 Internal Server Error");
    echo "<html><body><h1>500 Internal Server Error</h1>";
    echo $exception->getMessage();
    echo "<pre>";
    echo $exception->getTraceAsString();
    echo "</pre>";
    echo "</body></html>";
    exit;
}

set_exception_handler("exception_handler");

///////////////////////////////////////////////////////////////////////////
// (REST APIでは特に)エラーに対しては容赦なく例外を上げてHTTPレベルでエラーにすべき
function exception_error_handler($errno, $errstr, $errfile, $errline ) {
    throw new ErrorException($errstr, $errno, 0, $errfile, $errline);
}

set_error_handler("exception_error_handler");

///////////////////////////////////////////////////////////////////////////
// デバッグログ出力用関数
function debug_log($value) {
  // PHPの組み込みWebサーバで実行されている時以外はデバッグログを出力しない
  if (php_sapi_name() != "cli-server") return;

  $value_type = gettype($value);
  if ($value_type == "array" || $value_type == "object") {
    $value = var_export($value, TRUE);
  }

  error_log($value);
}

///////////////////////////////////////////////////////////////////////////
// このスクリプト自身のURLを生成する
function script_url() {
  $scheme = (isset($_SERVER["HTTPS"]) && $_SERVER["HTTPS"])? "https":"http";
  return sprintf("%s://%s%s/", $scheme, $_SERVER["HTTP_HOST"], $_SERVER["SCRIPT_NAME"]);
}

///////////////////////////////////////////////////////////////////////////
// 古いPHPのために boolval関数を提供
if (PHP_VERSION_ID < 50500) {
  function boolval($var) {
    return !!$var;
  }
}

///////////////////////////////////////////////////////////////////////////
// 文字列をNFKC正規化または mb_convert_kanaにかける
function normalize($str) {
  if ($str === null) return null;
  if (method_exists("Normalizer", "normalize")) { // intlモジュール有効時(PHP5.3以降)
    debug_log("Using Normalizer");
    return Normalizer::normalize($str, Normalizer::FORM_KC);
  }
  // else
  if (function_exists("mb_convert_kana")) { // intlがなければmbstringを使用
    // 「全角」英数字を「半角」に変換
    // 「全角」スペースを「半角」に変換
    // 「半角カタカナ」を「全角カタカナ」に変換
    // 濁点付きの文字を一文字に変換
    debug_log("Using mb_convert_kana");
    return mb_convert_kana($str, "asKV", "UTF-8");
  }
  // intlもmbstringもなければあきらめる
  return $str;
}
?>
