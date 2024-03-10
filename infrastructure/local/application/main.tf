terraform {
  backend "s3" {
    key    = "pastureen/local-db.tfstate"
    bucket = "pastureen-tf-state-store"
    region = "ap-southeast-2"
  }
}

provider "helm" {
  kubernetes {
    config_path = "~/.kube/config"
  }
}
