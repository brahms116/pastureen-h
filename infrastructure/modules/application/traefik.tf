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
    name  = "ingressClass.isDefaultClass"
    value = "false"
  }

  set {
    name  = "ingressClass.name"
    value = local.ingress_classname[var.environment]
  }

  dynamic "set" {
    for_each = var.environment == "TEST" ? [
      {
        name  = "ports.web.exposedPort"
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
      # I run a single node microk8, so I set the service to nodeport and expose the ports
      # directly. I've also hacked the service node port range to allow for this
      {
        name  = "service.type"
        value = "NodePort"
      },
      {
        name  = "ports.web.nodePort"
        value = "80"
      },
      {
        name  = "ports.websecure.nodePort"
        value = "443"
      }
    ] : []

    content {
      name  = set.value.name
      value = set.value.value
    }
  }
}

