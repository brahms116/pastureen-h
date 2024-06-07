terraform {
  backend "s3" {
    key    = "pastureen/production-application.tfstate"
    bucket = "pastureen-tf-state-store"
    region = "ap-southeast-2"
  }
}

provider "helm" {
  kubernetes {
    config_path = "~/.kube/config_production"
    config_context = "production"
  }
}

provider "kubernetes" {
  config_path = "~/.kube/config_production"
  config_context = "production"
}

module "application_deployment" {
  source      = "../../modules/application"
  environment = "PRODUCTION"
  noco_meta_dir = "/var/lib/noco"
  # Not applicable for production
  stack_dir = null
  application_dir = null
}
