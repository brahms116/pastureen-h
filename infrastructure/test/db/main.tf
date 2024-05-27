terraform {
  backend "s3" {
    key    = "pastureen/test-db.tfstate"
    bucket = "pastureen-tf-state-store"
    region = "ap-southeast-2"
  }
}

provider "kubernetes" {
  config_path = "~/.kube/config"
  config_context = "docker-desktop"
}

module "db_deployment" {
  source      = "../../module-db"
  environment = "TEST"
}
