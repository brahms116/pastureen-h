variable "db_data_dir" {
  description = "The absolute path to the postgres db data dir for local development"
  type        = string
}

variable "noco_meta_dir" {
  description = "The absolute path for the nocodb volume, dunno why this is needed"
  type        = string
}

variable "application_root" {
  description = "The absolute path to the haskell stack application root"
  type        = string
}
