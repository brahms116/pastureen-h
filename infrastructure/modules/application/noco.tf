resource "kubernetes_ingress_v1" "noco" {
  metadata {
    name      = "nocodb"
    namespace = local.namespace[var.environment]
  }

  spec {
    ingress_class_name = local.ingress_classname[var.environment]
    rule {
      host = local.noco_host[var.environment]
      http {
        path {
          path = "/"
          backend {
            service {
              name = kubernetes_service.noco.metadata[0].name
              port {
                number = kubernetes_service.noco.spec[0].port[0].port
              }
            }
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "noco" {
  metadata {
    name      = "nocodb"
    namespace = local.namespace[var.environment]
  }
  spec {
    selector = {
      app = "noco"
    }

    port {
      port        = 8080
      target_port = 8080
    }
  }
}

resource "kubernetes_deployment" "noco" {
  metadata {
    name      = "noco"
    namespace = local.namespace[var.environment]
  }
  spec {
    replicas = 1
    selector {
      match_labels = {
        app = "noco"
      }
    }
    template {
      metadata {
        labels = {
          app = "noco"
        }
      }
      spec {
        container {
          name  = "noco"
          image = "nocodb/nocodb:latest"
          image_pull_policy = "Always"

          port {
            container_port = 8080
          }

          env {
            name  = "NC_DB"
            value = "pg://database.${local.namespace[var.environment]}:5432?u=postgres&p=my_password&d=nocodb"
          }

          volume_mount {
            name       = "noco-storage"
            mount_path = "/usr/app/data"
          }
        }

        volume {
          name = "noco-storage"
          persistent_volume_claim {
            claim_name = local.noco_data_store_name[var.environment]
          }
        }
      }
    }
  }
}

module "noco_storage" {
  source       = "../storage"
  hostpath     = local.noco_data_hostpath[var.environment]
  environment  = var.environment
  storage_size = "1Gi"
  name         = local.noco_data_store_name[var.environment]
}
