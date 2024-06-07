variable "environment" {
  description = "LOCAL, TEST or PRODUCTION"
  type        = string
}

variable "db_storage_hostpath" {
  description = "Host path for database storage"
  type        = string
}
