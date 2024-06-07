terraform {
  backend "s3" {
    key    = "pastureen/test-db.tfstate"
    bucket = "pastureen-tf-state-store"
    region = "ap-southeast-2"
  }
}

provider "kubernetes" {
  config_path = "~/.kube/config_test"
  config_context = "test"
}

module "db_deployment" {
  source      = "../../modules/db"
  environment = "TEST"
  db_storage_hostpath = var.db_test_data_dir
}
