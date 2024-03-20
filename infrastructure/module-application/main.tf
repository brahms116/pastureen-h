locals {
  namespace = "pastureen"
}

resource "helm_release" "traefik_proxy" {
  namespace  = local.namespace
  name       = "traefik"
  repository = "https://traefik.github.io/charts"
  chart      = "traefik"
  set {
    name  = "ports.websecure.expose"
    value = "false"
  }
}

resource "kubernetes_ingress_v1" "noco" {
  metadata {
    name      = "nocodb"
    namespace = local.namespace
  }

  spec {
    rule {
      host = "noco.me.davidkwong.net"
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
    namespace = local.namespace
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
    namespace = local.namespace
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

          port {
            container_port = 8080
          }

          env {
            name  = "NC_DB"
            value = "pg://database.pastureen:5432?u=postgres&p=my_password&d=nocodb"
          }
        }
      }
    }
  }
}
