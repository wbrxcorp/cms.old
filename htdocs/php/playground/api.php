<?php
require_once("../route.php");
require_once("../json.php");

register_route("/^\/hello\/([0-9]+)\/([0-9]+)$/", "GET", function($params) {
  debug_log($params);
  response_json($params);
});

dispatch_request();
?>
