module "notify_overdue_todos" {
  count        = var.environment == "PRODUCTION" ? 1 : 0
  source       = "./task"
  name         = "notify-overdue-todos"
  namespace    = local.namespace[var.environment]
  cron         = "*/10 * * * *"
  image        = "20544dk/worker:1.0"
  task_command = "NOTIFY_OVERDUE_TODOS"
  envs         = local.application_container_envs
}

module "notify_pending_serving_requests" {
  count        = var.environment == "PRODUCTION" ? 1 : 0
  source       = "./task"
  name         = "pending-serving-requests"
  namespace    = local.namespace[var.environment]
  cron         = "0 0 * * *"
  image        = "20544dk/worker:1.0"
  task_command = "PENDING_SERVING_REQUESTS"
  envs         = local.application_container_envs
}
