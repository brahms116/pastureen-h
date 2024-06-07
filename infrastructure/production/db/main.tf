terraform {
  backend "s3" {
    key    = "pastureen/production-db.tfstate"
    bucket = "pastureen-tf-state-store"
    region = "ap-southeast-2"
  }
}

provider "kubernetes" {
  config_path = "~/.kube/config_production"
  config_context = "production"
}

module "db_deployment" {
  source      = "../../modules/db"
  environment = "PRODUCTION"
  db_storage_hostpath = "/var/lib/postgresql/data"
}
