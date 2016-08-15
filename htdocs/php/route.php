<?php
require_once("global.php");

$__routes__ = array();

function register_route($regex, $method, $func) {
  global $__routes__;
  if (!isset($routes[$regex])) $rouees[$regex] = array();
  $__routes__[$regex] []= array($method, $func);
}

function dispatch_request() {
  global $__routes__;
  $method = $_SERVER["REQUEST_METHOD"];
  $path = isset($_SERVER["PATH_INFO"])? $_SERVER["PATH_INFO"] : "/";

  foreach($__routes__ as $regex => $routes) {
    $params = array();
    if (preg_match($regex, $path, $params)) { // TODO: regexが複数マッチする時はよりコンクリートなマッチを優先する
      foreach($routes as $route) {
        if ($route[0] == $method) {
          $route[1]($params);  // TODO: 文字列か関数かで呼び出し方を分ける
          exit;
        }
      }
      // else
      throw new HttpErrorStatus("Method Not Allowed", 405);
    }
  }
  // else
  throw new HttpErrorStatus("Not Found", 404);
}
?>
