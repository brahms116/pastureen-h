variable "environment" {
  description = "LOCAL, TEST or PRODUCTION"
  type        = string
}

variable "storage_size" {
  description = "Size of the storage"
  type        = string
  default     = "8Gi"
}

variable "name" {
  description = "Name of the storage space"
  type        = string
}

# This will need to be optional if there are more nodes
# But that will never happen
variable "hostpath" {
  description = "Hostpath for the storage"
  type        = string
}
