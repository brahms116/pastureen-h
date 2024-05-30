
resource "kubernetes_ingress_v1" "api" {
  metadata {
    name = "api"
    namespace = local.namespace[var.environment]
  }

  spec {
    ingress_class_name = local.ingress_classname[var.environment]

    rule {
      host = local.api_host[var.environment]
      http {
        path {
          path = "/"
          backend {
            service {
              name = kubernetes_service.api.metadata[0].name
              port {
                number = kubernetes_service.api.spec[0].port[0].port
              }
            }
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "api" {
  metadata {
    name = "api"
    namespace = local.namespace[var.environment]
  }

  spec {
    selector = {
      app = "api"
    }

    port {
      port = 8080
      target_port = 8080
    }
  }
}
