<?php
require_once("global.php");
///////////////////////////////////////////////////////////////////////////
// HTTP POSTのボディ部分をJSONとしてパースする
function parse_json() {
  $request_body = file_get_contents('php://input');
  debug_log("Request: " . $request_body);
  return json_decode($request_body, true);
}

///////////////////////////////////////////////////////////////////////////
// Unicode文字列をエスケープせずにArrayをJSONエンコードする
function raw_json_encode($value) {
    // PHPのバージョンが 5.4以上の場合は JSON_UNESCAPED_UNICODEオプションが使えるのでそれを使う
    if (PHP_VERSION_ID >= 50400) return json_encode($value, JSON_UNESCAPED_UNICODE);
    // version_compare関数を使う方が良いかも http://php.net/manual/ja/function.version-compare.php

    // CentOS <= 6 など、PHPが古い場合は正規表現による置換でどうにかする
    return preg_replace_callback(
        '/\\\\u([0-9a-zA-Z]{4})/',
        function($matches) { return mb_convert_encoding(pack('H*',$matches[1]),'UTF-8','UTF-16'); },
        json_encode($value)
    );
}

///////////////////////////////////////////////////////////////////////////
// ArrayをJSONエンコードしてレスポンスする
function response_json($value) {
    $response = raw_json_encode($value);
    debug_log("Response: " . $response);
    header('Content-Type: application/json');
    // 古いIEがおかしいMIMEタイプ判定をしてXSSの原因になる問題を避けるための呪符
    header('X-Content-Type-Options: nosniff');
    echo $response;
}

///////////////////////////////////////////////////////////////////////////
// AngualrJS要XSRFトークン検証。セッションが有効でない場合は常にfalseが返る
function validate_xsrf_token() {
  if (php_sapi_name() == "cli") return true; // cli実行の場合は常に検証OKにする
  $sessionId = session_id();
  if ($sessionId == "" || !isset($_SERVER["HTTP_X_XSRF_TOKEN"])) return false;
  return $_SERVER["HTTP_X_XSRF_TOKEN"] === $sessionId;
}
?>
