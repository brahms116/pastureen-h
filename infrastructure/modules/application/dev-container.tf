module "development_container_storage" {
  count       = var.environment == "PRODUCTION" ? 0 : 1
  source      = "../storage"
  environment = var.environment
  hostpath    = var.application_dir
  name        = local.application_dir_name[var.environment]
}

module "cabal_dir_storage" {
  count       = var.environment == "PRODUCTION" ? 0 : 1
  source      = "../storage"
  environment = var.environment
  hostpath    = var.cabal_dir
  name        = local.cabal_dir_name[var.environment]
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
          name              = "development-container"
          image             = "20544dk/dev-container:1.0"
          image_pull_policy = "Always"

          command = [
            "tail", "-f", "/dev/null"
          ]

          dynamic "env" {
            for_each = local.application_container_envs
            content {
              name = env.value.name
              value_from {
                secret_key_ref {
                  name = "pt-secrets"
                  key  = env.value.name
                }
              }
            }
          }

          port {
            container_port = 8080
          }

          volume_mount {
            name       = "application-dir"
            mount_path = "/app"
          }

          volume_mount {
            name       = "cabal-dir"
            mount_path = "/root/.cabal"
          }

        }
        volume {
          name = "application-dir"
          persistent_volume_claim {
            claim_name = local.application_dir_name[var.environment]
          }
        }
        volume {
          name = "cabal-dir"
          persistent_volume_claim {
            claim_name = local.cabal_dir_name[var.environment]
          }
        }
      }
    }
  }
}
