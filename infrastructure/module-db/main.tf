locals {
  namespace = {
    "LOCAL"      = "pastureen",
    "TEST"       = "pastureen-test",
    "PRODUCTION" = "pastureen"
  }

  hostpath = {
    "LOCAL"      = "/Users/david/pg-data",
    "TEST"       = "/Users/david/pg-test-data",
    "PRODUCTION" = "/var/lib/postgresql/data"
  }
}

resource "kubernetes_namespace" "main" {
  metadata {
    name = local.namespace[var.environment]
  }
}

resource "kubernetes_service" "database" {
  metadata {
    name      = "database"
    namespace = local.namespace[var.environment]
  }

  spec {
    selector = {
      app = "database"
    }

    port {
      protocol    = "TCP"
      port        = 5432
      target_port = 5432
    }
  }
}

resource "kubernetes_persistent_volume" "data" {
  metadata {
    name = "${local.namespace[var.environment]}-data"
    labels = {
      pv_name = "${local.namespace[var.environment]}-data"
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
        path = local.hostpath[var.environment]
      }
    }
  }
}

resource "kubernetes_persistent_volume_claim" "data" {
  metadata {
    name      = "data"
    namespace = local.namespace[var.environment]
  }

  spec {
    selector {
      match_labels = {
        pv_name = "${local.namespace[var.environment]}-data"
      }
    }

    access_modes = ["ReadWriteOnce"]
    resources {
      requests = {
        storage = "8Gi"
      }
    }
  }
}

resource "kubernetes_deployment" "database" {
  metadata {
    name      = "database"
    namespace = local.namespace[var.environment]
    labels = {
      app = "database"
    }
  }

  spec {
    replicas = 1
    strategy {
      type = "Recreate"
    }

    selector {
      match_labels = {
        app = "database"
      }
    }

    template {
      metadata {
        name      = "database"
        namespace = local.namespace[var.environment]
        labels = {
          app = "database"
        }
      }

      spec {
        container {
          name  = "database"
          image = "postgres:13"
          env {
            name  = "POSTGRES_PASSWORD"
            value = "my_password"
          }

          port {
            container_port = 5432
          }

          volume_mount {
            name       = "data"
            mount_path = "/var/lib/postgresql/data"
          }
        }

        volume {
          name = "data"
          persistent_volume_claim {
            claim_name = kubernetes_persistent_volume_claim.data.metadata.0.name
          }
        }
      }
    }
  }
}
