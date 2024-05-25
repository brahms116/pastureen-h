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
    config_context = "oracle-context"
  }
}

provider "kubernetes" {
  config_path = "~/.kube/config"
  config_context = "oracle-context"
}

module "application_deployment" {
  source      = "../../module-application"
  environment = "PRODUCTION"
}
