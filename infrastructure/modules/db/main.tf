locals {
  namespace = {
    "LOCAL"      = "pastureen-local",
    "TEST"       = "pastureen-test",
    "PRODUCTION" = "pastureen-production"
  }

  db_hostpath = {
    "LOCAL"      = "/Users/david/pg-data",
    "TEST"       = "/Users/david/pg-test-data",
    "PRODUCTION" = "/var/lib/postgresql/data"
  }

  db_data_name = {
    "LOCAL"      = "db-data-local",
    "TEST"       = "db-data-test",
    "PRODUCTION" = "db-data-production"
  }
}

module "db_storage" {
  source       = "../storage"
  environment  = var.environment
  storage_size = "10Gi"
  name         = local.db_data_name[var.environment]
  hostpath    = local.db_hostpath[var.environment]
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
            name       = "db-data"
            mount_path = "/var/lib/postgresql/data"
          }
        }

        volume {
          name = "db-data"
          persistent_volume_claim {
            claim_name = local.db_data_name[var.environment]
          }
        }
      }
    }
  }
}
