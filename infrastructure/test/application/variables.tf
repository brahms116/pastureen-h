variable "noco_test_meta_dir" {
  description = "The absolute path for the nocodb volume, dunno why this is needed"
  type        = string
}

variable "cabal_dir" {
  description = "The absolute path for the global cabal directory"
  type        = string
}

variable "application_root" {
  description = "The absolute path to the haskell cabal application root"
  type        = string
}
