module "development_container_storage" {
  count = var.environment == "PRODUCTION" ? 0 : 1
  source      = "../storage"
  environment = var.environment
  hostpath    = "/Users/david/dev/pastureen-h/application"
  name        = local.application_dir_name[var.environment]
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
          image_pull_policy = "Always"

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
            claim_name = local.application_dir_name[var.environment]
          }
        }
      }
    }
  }
}
