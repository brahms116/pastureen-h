terraform {
  backend "s3" {
    key    = "pastureen/production-application.tfstate"
    bucket = "pastureen-tf-state-store"
    region = "ap-southeast-2"
  }
}

provider "helm" {
  kubernetes {
    config_path = "~/.kube/config"
    config_context = "context-czktpqrhmza"
  }
}

provider "kubernetes" {
  config_path = "~/.kube/config"
}

module "application_deployment" {
  source      = "../../module-application"
  environment = "PRODUCTION"
}
