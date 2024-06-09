locals {
  namespace = {
    "LOCAL"      = "pastureen-local",
    "TEST"       = "pastureen-test",
    "PRODUCTION" = "pastureen-production"
  }

  noco_host = {
    "LOCAL"      = "noco.me.davidkwong.net",
    "TEST"       = "noco.me.davidkwong.net",
    "PRODUCTION" = "noco.davidkwong.net"
  }

  ingress_classname = {
    "LOCAL"      = "traefik-local",
    "TEST"       = "traefik-test",
    "PRODUCTION" = "traefik"
  }

  noco_data_store_name = {
    "LOCAL"      = "noco-data-local",
    "TEST"       = "noco-data-test",
    "PRODUCTION" = "noco-data-production"
  }

  application_dir_name = {
    "LOCAL" = "application-dir-local",
    "TEST"  = "application-dir-test",
  }

  stack_dir_name = {
    "LOCAL" = "stack-dir-local",
    "TEST"  = "stack-dir-test",
  }

  api_host = {
    "LOCAL"      = "me.davidkwong.net",
    "TEST"       = "me.davidkwong.net",
    "PRODUCTION" = "www.davidkwong.net"
  }

  application_container_envs = [
    {
      name = "PT_TODOIST_TOKEN"
    },
    {
      name = "PT_NTFY_TOPIC"
    },
    {
      name = "PT_ELVANTO_EMAIL"
    },
    {
      name = "PT_ELVANTO_PASSWORD"
    }
  ]
}
