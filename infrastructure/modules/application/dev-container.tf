module "development_container_storage" {
  count = var.environment == "PRODUCTION" ? 0 : 1
  source      = "../storage"
  environment = var.environment
  hostpath    = "/Users/david/dev/pastureen-h/application"
  name        = local.application_dir_name[var.environment]
}

module "stack_dir_storage" {
  count = var.environment == "PRODUCTION" ? 0 : 1
  source      = "../storage"
  environment = var.environment
  hostpath    = "/Users/david/.stack-k8"
  name        = local.stack_dir_name[var.environment]
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
        app = "api"
      }
    }
    template {
      metadata {
        labels = {
          app = "api"
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

          volume_mount {
            name       = "stack-dir"
            mount_path = "/root/.stack"
          }

        }
        volume {
          name = "application-dir"
          persistent_volume_claim {
            claim_name = local.application_dir_name[var.environment]
          }
        }
        volume {
          name = "stack-dir"
          persistent_volume_claim {
            claim_name = local.stack_dir_name[var.environment]
          }
        }
      }
    }
  }
}
