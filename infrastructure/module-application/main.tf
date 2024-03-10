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
