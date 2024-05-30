terraform {
  backend "s3" {
    key    = "pastureen/test-application.tfstate"
    bucket = "pastureen-tf-state-store"
    region = "ap-southeast-2"
  }
}

provider "helm" {
  kubernetes {
    config_path = "~/.kube/config_test"
    config_context = "test"
  }
}

provider "kubernetes" {
  config_path = "~/.kube/config_test"
}

module "application_deployment" {
  source      = "../../module/application"
  environment = "TEST"
}
