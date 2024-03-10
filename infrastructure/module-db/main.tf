locals {
  namespace     = "pastureen"
  hostnode      = var.environment == "LOCAL" ? "docker-desktop" : "???"
  storage_class = var.environment == "LOCAL" ? "hostpath" : "???"
  mount_path    = var.environment == "LOCAL" ? "/Users/david/pg-data" : "???"
}

resource "kubernetes_namespace" "main" {
  metadata {
    name = local.namespace
  }
}

resource "kubernetes_service" "database" {
  metadata {
    name      = "database"
    namespace = local.namespace
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
    name = "data"
    labels = {
      pv_name = "data"
    }
  }

  spec {
    access_modes = ["ReadWriteOnce"]
    capacity = {
      storage = "8Gi"
    }
    persistent_volume_reclaim_policy = "Retain"
    storage_class_name               = local.storage_class

    persistent_volume_source {
      dynamic "host_path" {
        for_each = var.environment == "LOCAL" ? [local.mount_path] : []
        content {
          path = local.mount_path
        }
      }

      dynamic "local" {
        for_each = var.environment == "REMOTE" ? [local.mount_path] : []
        content {
          path = local.mount_path
        }
      }
    }

    node_affinity {
      required {
        node_selector_term {
          match_expressions {
            key      = "kubernetes.io/hostname"
            operator = "In"
            values   = [local.hostnode]
          }
        }
      }
    }
  }
}

resource "kubernetes_persistent_volume_claim" "data" {
  metadata {
    name      = "data"
    namespace = local.namespace
  }

  spec {
    selector {
      match_labels = {
        pv_name = "data"
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


# Single node k8 so why not
resource "kubernetes_pod" "database" {
  metadata {
    name      = "database"
    namespace = local.namespace
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
