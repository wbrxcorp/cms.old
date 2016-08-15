<?php session_start(); ?>
<html>
<head>
  <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" rel="stylesheet">
  <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular.js"></script>
  <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular-resource.js"></script>
  <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular-route.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/angular-ui-bootstrap/2.0.0/ui-bootstrap-tpls.js"></script>
  <script src="./js/app.js"></script>
</head>
<body>
  <div class="navbar navbar-inverse navbar-static-top">
    <div class="container">
      <div class="navbar-header">
          <a href="./"><span class="navbar-brand">CMS version {{version}}</span></a>
      </div>
      <div ng-controller="LoginStatus as loginStatus">
        <ul class="nav navbar-nav navbar-right">
          <li class="dropdown" uib-dropdown ng-if="loginStatus.status">
            <a id="loginstatus-dropdown" href uib-dropdown-toggle>{{loginStatus.username}} でログイン中 <b class="caret"></b></a>
            <ul class="dropdown-menu" uib-dropdown-menu aria-labelledby="loginstatus-dropdown">
             <li class="dropdown-header">アカウント</li>
             <li><a href="#/profile">設定</a></li>
             <li><a href="" ng-click="quit()">退会する</a></li>
             <li class="divider"></li>
             <li><a href ng-click="loginStatus.logout()">ログアウト</a></li>
            </ul>
          </li>
          <li class="navbar-text" ng-if="!loginStatus.status">ログインされていません</li>
        </ul>
      </div>
      <!--/.navbar-collapse -->
    </div>
  </div>
  <div class="container">
    <?php if (isset($_SESSION["userId"])) {?>
      <div ng-view><!--ログインされている--></div>
    <?php } else {?>
      <!-- ログインされていない場合ログイン画面を表示 -->
      <div ng-controller="Login as login">
        <form name="form" class="form-horizontal">
          <h3 class="title-divider"><span>ログイン</span></h3>
          <div class="form-group">
              <label for="username" class="col-sm-3 control-label">ユーザー名</label>
              <div class="col-sm-9" ng-class="{'has-success':(form.username.$valid && form.username.$dirty),'has-error':form.username.$invalid && form.username.$dirty}">
                  <input type="text" id="username" name="username" ng-model="username" class="form-control" ng-required="true">
              </div>
          </div>
          <div class="form-group">
              <label for="password" class="col-sm-3 control-label">パスワード</label>
              <div class="col-sm-9" ng-class="{'has-success':(form.password.$valid && form.password.$dirty),'has-error':form.password.$invalid && form.password.$dirty}">
                  <input type="password" id="password" name="password" ng-model="password" class="form-control" ng-required="true">
              </div>
          </div>
          <span class="text-danger" ng-show="error">{{ error }}<br></span>
          <button ng-click="login.login()" ng-disabled="!form.$valid" class="btn btn-primary">ログイン</button>
          <a href="./guest.jsp#/signup">新規登録</a>
        </form>
      </div>
    <?php }?>
  </div>

  <footer id="footer">
      <hr>
      <div class="container">
          <div class="row">
              <div class="col-md-6">
                  <p>Copyright 2014-2016 &copy; <a href="http://www.walbrix.com/jp/">Walbrix Corporation</a></p>
              </div>
              <div class="col-md-6">
                  <ul class="list-inline footer-menu">
                      <li><a href="http://www.walbrix.com/jp/contact.html">お問い合わせ</a></li>
                  </ul>
              </div>
          </div>
      </div>
  </footer>
</body>
</html>
