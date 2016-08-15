<?php
require_once("global.php");
if (!defined("DBHOST")) define("DBHOST", "localhost");
if (!defined("DBNAME")) define("DBNAME", "playground");
if (!defined("DBUSER")) define("DBUSER", "playground");
if (!defined("DBPASSWORD")) define("DBPASSWORD", "");

///////////////////////////////////////////////////////////////////////////
// データベース接続を得る
function get_database_connection() {
  return new PDO("mysql:dbname=" . DBNAME . ";charset=utf8",DBUSER,DBPASSWORD,array(PDO::ATTR_ERRMODE=>PDO::ERRMODE_EXCEPTION));
}
?>
