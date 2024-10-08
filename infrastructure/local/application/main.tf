terraform {
  backend "s3" {
    key    = "pastureen/local-application.tfstate"
    bucket = "pastureen-tf-state-store"
    region = "ap-southeast-2"
  }
}

provider "helm" {
  kubernetes {
    config_path = "~/.kube/config_local"
    config_context = "local"
  }
}

provider "kubernetes" {
  config_path = "~/.kube/config_local"
}

module "application_deployment" {
  source      = "../../modules/application"
  environment = "LOCAL"
  cabal_dir = var.cabal_dir
  application_dir = var.application_root
  noco_meta_dir = var.noco_meta_dir
}
