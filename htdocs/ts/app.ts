/// <reference path="typings/angular-ui-bootstrap/angular-ui-bootstrap.d.ts" />
angular.module("CMS", ["ngResource", "ngRoute", "ui.bootstrap"])
.config(["$routeProvider", function($routeProvider) {
  $routeProvider
    .when("/dashboard", {templateUrl:"dashboard.html",controller:"Dashboard"})
    .when("/profile", {templateUrl:"profile.html",controller:"Profile"})
    .when("/entry/:entryId", {templateUrl:"edit.html",controller:"Entry",controllerAs:"c"})
    .when("/prefix/:prefix*", {templateUrl:"prefix.html",controller:"Prefix",controllerAs:"c"})
    .otherwise({redirectTo:"/dashboard"});
}])
.controller("Login", ["$resource", "$window", LoginController])
.controller("LoginStatus", ["$resource", "$window", LoginStatusController])
.controller("Dashboard", ["$resource", function($resource) {

}])
.run(["$rootScope","$resource",   function($scope, $resource) {
  let info = $resource("./api.php/");
  info.get({}, function(result) {
    $scope.version = result.version;
  });
}]);
angular.element(document).ready(function() {
  angular.bootstrap(document, ['CMS']);
});
