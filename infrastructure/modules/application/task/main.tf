resource "kubernetes_cron_job_v1" "task" {

  metadata {
    name      = var.name
    namespace = var.namespace
  }

  spec {
    schedule = var.cron

    job_template {
      metadata {
        name = var.name
      }

      spec {
        template {
          metadata {
            labels = {
              app = var.name
            }
          }

          spec {
            container {
              name              = var.name
              image             = var.image
              image_pull_policy = "Always"

              env {
                name  = "PT_TASK_NAME"
                value = var.task_command
              }

              dynamic "env" {
                for_each = var.envs
                content {
                  name = env.value.name
                  value_from {
                    secret_key_ref {
                      name = "pt-secrets"
                      key  = env.value.name
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}


