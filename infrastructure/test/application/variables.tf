variable "noco_test_meta_dir" {
  description = "The absolute path for the nocodb volume, dunno why this is needed"
  type        = string
}

variable "stack_dir" {
  description = "The absolute path for the global stack directory"
  type        = string
}

variable "application_root" {
  description = "The absolute path to the haskell stack application root"
  type        = string
}
