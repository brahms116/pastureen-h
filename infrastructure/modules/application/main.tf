locals {
  namespace = {
    "LOCAL"      = "pastureen",
    "TEST"       = "pastureen-test",
    "PRODUCTION" = "pastureen"
  }

  noco_host = {
    "LOCAL"      = "noco.me.davidkwong.net",
    "TEST"       = "noco.me.davidkwong.net",
    "PRODUCTION" = "noco.davidkwong.net"
  }

  ingress_classname = {
    "LOCAL"      = "traefik-local",
    "TEST"       = "traefik-test",
    "PRODUCTION" = "traefik"
  }

  noco_data_store_name = {
    "LOCAL"      = "noco-data-local",
    "TEST"       = "noco-data-test",
    "PRODUCTION" = "noco-data-production"
  }

  noco_data_hostpath = {
    "LOCAL"      = "/Users/david/noco",
    "TEST"       = "/Users/david/noco-test"
    "PRODUCTION" = "/var/lib/noco"
  }
}


resource "kubernetes_persistent_volume" "application_dir" {
  count = var.environment == "PRODUCTION" ? 0 : 1

  metadata {
    name = "${local.namespace[var.environment]}-application-dir"
    labels = {
      pv_name = "${local.namespace[var.environment]}-application-dir"
    }
  }

  spec {
    access_modes = ["ReadWriteOnce"]
    capacity = {
      storage = "20Gi"
    }
    persistent_volume_reclaim_policy = "Retain"
    storage_class_name               = "hostpath"

    persistent_volume_source {
      host_path {
        path = "/Users/david/dev/pastureen-h/application"
      }
    }
  }
}

resource "kubernetes_persistent_volume_claim" "application_dir" {
  count = var.environment == "PRODUCTION" ? 0 : 1

  metadata {
    name      = "application-dir"
    namespace = local.namespace[var.environment]
  }

  spec {

    selector {
      match_labels = {
        pv_name = "${local.namespace[var.environment]}-application-dir"
      }
    }
    access_modes = ["ReadWriteOnce"]
    resources {
      requests = {
        storage = "20Gi"
      }
    }
  }
}

resource "kubernetes_deployment" "development_container" {
  count = var.environment == "PRODUCTION" ? 0 : 1
  metadata {
    name      = "development-container"
    namespace = local.namespace[var.environment]
  }
  spec {
    replicas = 1
    selector {
      match_labels = {
        app = "development-container"
      }
    }
    template {
      metadata {
        labels = {
          app = "development-container"
        }
      }
      spec {
        container {
          name  = "development-container"
          image = "20544dk/dev-container:1.0"
          port {
            container_port = 8080
          }

          volume_mount {
            name       = "application-dir"
            mount_path = "/app"
          }
        }
        volume {
          name = "application-dir"
          persistent_volume_claim {
            claim_name = "application-dir"
          }
        }
      }
    }
  }
}
