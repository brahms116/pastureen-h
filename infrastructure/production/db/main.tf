terraform {
  backend "s3" {
    key    = "pastureen/production-db.tfstate"
    bucket = "pastureen-tf-state-store"
    region = "ap-southeast-2"
  }
}

provider "kubernetes" {
  config_path = "~/.kube/config"
  config_context = "context-czktpqrhmza"
}

module "db_deployment" {
  source      = "../../module-db"
  environment = "PRODUCTION"
}
