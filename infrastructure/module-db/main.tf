locals {
  namespace     = "pastureen"
  hostnode      = "docker-desktop"
  storage_class = var.environment == "LOCAL" ? "hostpath" : "longhorn"
  mount_path    = var.environment == "LOCAL" ? "/Users/david/pg-data" : "/pg-data"
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
  count = var.environment == "LOCAL" ? 1 : 0
  metadata {
    name = "data"
    labels = {
      pv_name = "data"
    }
  }

  spec {
    access_modes = ["ReadWriteOnce"]
    capacity = {
      storage = "20Gi"
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

    dynamic "node_affinity" {
      for_each = var.environment == "LOCAL" ? [local.hostnode] : []
      content {
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
}

resource "kubernetes_persistent_volume_claim" "data" {
  metadata {
    name      = "data"
    namespace = local.namespace
  }

  spec {
    dynamic "selector" {
      for_each = var.environment == "LOCAL" ? [true] : []
      content {
        match_labels = {
          pv_name = "data"
        }
      }
    }

    storage_class_name = var.environment == "LOCAL" ? null : local.storage_class

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
    namespace = local.namespace
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

          dynamic "env" {
            for_each = var.environment == "LOCAL" ? [] : [true]
            content {
              name  = "PGDATA"
              value = "/var/lib/postgresql/data/pgdata"
            }
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
