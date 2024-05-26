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
}

resource "helm_release" "traefik_proxy" {
  namespace  = local.namespace[var.environment]
  name       = "traefik"
  repository = "https://traefik.github.io/charts"
  chart      = "traefik"
  set {
    name  = "ports.websecure.expose.default"
    value = var.environment == "PRODUCTION" ? "true" : "false"
  }

  set {
    name = "ingressClass.isDefaultClass"
    value = "false"
  }

  set {
    name  = "ingressClass.name"
    value = local.ingress_classname[var.environment]
  }

  dynamic "set" {
    for_each = var.environment == "TEST" ? [
      {
        name = "ports.web.exposedPort"
        value = "3000"
      }
    ] : []

    content {
      name  = set.value.name
      value = set.value.value
    }
  }

  dynamic "set" {
    for_each = var.environment == "PRODUCTION" ? [
      {
        name  = "ports.web.redirectTo.port"
        value = "websecure"
      },
      {
        name  = "ports.websecure.tls.certResolver"
        value = "letsencrypt"
      },
      {
        name  = "ports.websecure.tls.domains"
        value = "null"
      },
      {
        name  = "certResolvers.letsencrypt.email"
        value = "davidkwong17@gmail.com"
      },
      {
        name  = "certResolvers.letsencrypt.httpChallenge.entryPoint"
        value = "web"
      },
      {
        name  = "certResolvers.letsencrypt.storage"
        value = "/data/acme.json"
      },
      {
        name  = "service.spec.loadBalancerIP"
        value = "192.9.182.251"
      }
    ] : []

    content {
      name  = set.value.name
      value = set.value.value
    }
  }
}

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
