terraform {
  backend "s3" {
    key    = "pastureen/local-db.tfstate"
    bucket = "pastureen-tf-state-store"
    region = "ap-southeast-2"
  }
}

provider "kubernetes" {
  config_path = "~/.kube/config_local"
  config_context = "local"
}

module "db_deployment" {
  source      = "../../modules/db"
  environment = "LOCAL"
}
