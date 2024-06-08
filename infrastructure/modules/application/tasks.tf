
resource "kubernetes_cron_job_v1" "notify_overdue_todos" {

  count = var.environment == "PRODUCTION" ? 1 : 0

  metadata {
    name = "notify-overdue-todos"
    namespace = local.namespace[var.environment]
  }

  spec {
    schedule = "*/10 * * * *"

    job_template {
      metadata {
        name = "notify-overdue-todos"
      }

      spec {
        template {
          metadata {
            labels = {
              app = "notify-overdue-todos"
            }
          }

          spec {
            container {
              name              = "notify-overdue-todos"
              image             = "20544dk/worker:1.0"
              image_pull_policy = "Always"

              env {
                name  = "PT_TASK_NAME"
                value = "NOTIFY_OVERDUE_TODOS"
              }

              dynamic "env" {
                for_each = local.application_container_envs
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
