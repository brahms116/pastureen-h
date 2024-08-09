variable "environment" {
  description = "LOCAL, TEST or PRODUCTION"
  type        = string
}

variable "noco_meta_dir" {
  description = "Host path for noco volume storage, dunno why this is needed"
  type        = string
}

variable "application_dir" {
  description = "Host path to the application directory"
  type        = string
  # Not applicable for production
  nullable    = true
}

variable "cabal_dir" {
  description = "Host path for the global cabal directory"
  type        = string
  # Not applicable for production
  nullable    = true
}
