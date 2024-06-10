
variable "cron" {
  description = "Cron job schedule"
  type        = string
}

variable "name" {
  description = "Name of the cron job"
  type        = string
}

variable "task_command" {
  description = "The PT_TASK_NAME variable"
  type        = string
}

variable "namespace" {
  description = "The k8 namespace to use"
  type        = string
}

variable "image" {
  description = "The docker image to use"
}

variable "envs" {
  description = "The environment variables to use"
  type        = list(map(string))
  default     = []
}
