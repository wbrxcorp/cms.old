/// <reference path="typings/angular-ui-bootstrap/angular-ui-bootstrap.d.ts" />
/// <reference path="typings/angularjs/angular-resource.d.ts" />
class LoginController {
  private $login;

  constructor(private $resource:ng.resource.IResourceService, private $window:ng.IWindowService) {
    this.$login =  $resource("./api.php/login");
  }

  public login() {
    this.$login.save({}, {}, (result) => {
      if (result.success) {
        this.$window.location.href = "./";
      }
    });

  }
}

class LoginStatusController {
  private status:boolean;
  private username:string;

  constructor(private $resource:ng.resource.IResourceService, private $window:ng.IWindowService) {
    $resource("./api.php/login").get({}, (result) => {
      if (result.success) {
        this.status = true;
        this.username = result.info.name;
      }
    });
  }

  public logout() {
    this.$resource("./api.php/logout").save({}, {}, (result) => {
      if (result.success) {
        this.status = false;
        delete this.username;
        this.$window.location.href = "./";
      }
    });

  }
}

angular.module("CMS")
.controller("Login", ["$resource", "$window", LoginController])
.controller("LoginStatus", ["$resource", "$window", LoginStatusController])
