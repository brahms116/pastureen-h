
locals {
  namespace = {
    "LOCAL"      = "pastureen-local",
    "TEST"       = "pastureen-test",
    "PRODUCTION" = "pastureen-production"
  }

  storage_class = {
    "LOCAL"      = "hostpath",
    "TEST"       = "hostpath",
    "PRODUCTION" = "microk8s-hostpath"
  }
}

resource "kubernetes_persistent_volume" "data" {

  metadata {
    name = var.name
    labels = {
      pv_name = var.name
    }
  }

  spec {
    access_modes = ["ReadWriteOnce"]
    capacity = {
      storage = "20Gi"
    }
    persistent_volume_reclaim_policy = "Retain"
    storage_class_name               = local.storage_class[var.environment]

    persistent_volume_source {
      host_path {
        path = var.hostpath
      }
    }
  }
}

resource "kubernetes_persistent_volume_claim" "data" {
  metadata {
    name      = var.name
    namespace = local.namespace[var.environment]
  }

  spec {
    selector {
      match_labels = {
        pv_name = var.name
      }
    }

    access_modes = ["ReadWriteOnce"]
    resources {
      requests = {
        storage = var.storage_size
      }
    }
  }
}
